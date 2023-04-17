{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.CLI.Shelley.Run.Governance
  ( ShelleyGovernanceCmdError
  , renderShelleyGovernanceError
  , runGovernanceCmd
  ) where

import           Control.Monad (unless, when)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT, handleIOExceptT, left, newExceptT,
                   onLeft)
import           Data.Aeson (eitherDecode)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as LB
import           Data.Function ((&))
import           Data.String (fromString)
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Read as Text
import           Formatting (build, sformat)
import           System.IO (stderr, stdin, stdout)

import           Cardano.Api
import           Cardano.Api.Shelley

import           Cardano.CLI.Shelley.Key (VerificationKeyOrHashOrFile,
                   readVerificationKeyOrHashOrFile, readVerificationKeyOrHashOrTextEnvFile)
import           Cardano.CLI.Shelley.Parsers
import           Cardano.CLI.Shelley.Run.Read (CddlError, fileOrPipe, readFileTx)
import           Cardano.CLI.Types

import           Cardano.Binary (DecoderError)
import qualified Cardano.Ledger.Shelley.TxBody as Shelley
import           Control.Monad.IO.Class (MonadIO (..))
import qualified System.IO as IO

data ShelleyGovernanceCmdError
  = ShelleyGovernanceCmdTextEnvReadError !(FileError TextEnvelopeError)
  | ShelleyGovernanceCmdCddlError !CddlError
  | ShelleyGovernanceCmdKeyReadError !(FileError InputDecodeError)
  | ShelleyGovernanceCmdCostModelReadError !(FileError ())
  | ShelleyGovernanceCmdTextEnvWriteError !(FileError ())
  | ShelleyGovernanceCmdEmptyUpdateProposalError
  | ShelleyGovernanceCmdMIRCertificateKeyRewardMistmach
      !FilePath
      !Int
      -- ^ Number of stake verification keys
      !Int
      -- ^ Number of reward amounts
  | ShelleyGovernanceCmdCostModelsJsonDecodeErr !FilePath !Text
  | ShelleyGovernanceCmdEmptyCostModel !FilePath
  | ShelleyGovernanceCmdUnexpectedKeyType
      ![TextEnvelopeType]
      -- ^ Expected key types
  | ShelleyGovernanceCmdPollOutOfBoundAnswer
      !Int
      -- ^ Maximum answer index
  | ShelleyGovernanceCmdPollInvalidChoice
  | ShelleyGovernanceCmdDecoderError !DecoderError
  | ShelleyGovernanceCmdVerifyPollError !GovernancePollError
  | ShelleyGovernanceCmdWriteFileError !(FileError ())
  deriving Show

renderShelleyGovernanceError :: ShelleyGovernanceCmdError -> Text
renderShelleyGovernanceError err =
  case err of
    ShelleyGovernanceCmdTextEnvReadError fileErr -> Text.pack (displayError fileErr)
    ShelleyGovernanceCmdCddlError cddlErr -> Text.pack (displayError cddlErr)
    ShelleyGovernanceCmdKeyReadError fileErr -> Text.pack (displayError fileErr)
    ShelleyGovernanceCmdTextEnvWriteError fileErr -> Text.pack (displayError fileErr)
    -- TODO: The equality check is still not working for empty update proposals.
    ShelleyGovernanceCmdEmptyUpdateProposalError ->
      "Empty update proposals are not allowed"
    ShelleyGovernanceCmdMIRCertificateKeyRewardMistmach fp numVKeys numRwdAmts ->
       "Error creating the MIR certificate at: " <> textShow fp
       <> " The number of staking keys: " <> textShow numVKeys
       <> " and the number of reward amounts: " <> textShow numRwdAmts
       <> " are not equivalent."
    ShelleyGovernanceCmdCostModelsJsonDecodeErr fp err' ->
      "Error decoding cost model: " <> err' <> " at: " <> Text.pack fp
    ShelleyGovernanceCmdEmptyCostModel fp ->
      "The decoded cost model was empty at: " <> Text.pack fp
    ShelleyGovernanceCmdCostModelReadError err' ->
      "Error reading the cost model: " <> Text.pack (displayError err')
    ShelleyGovernanceCmdUnexpectedKeyType expected ->
      "Unexpected poll key type; expected one of: "
      <> Text.intercalate ", " (textShow <$> expected)
    ShelleyGovernanceCmdPollOutOfBoundAnswer nMax ->
      "Poll answer out of bounds. Choices are between 0 and " <> textShow nMax
    ShelleyGovernanceCmdPollInvalidChoice ->
      "Invalid choice. Please choose from the available answers."
    ShelleyGovernanceCmdDecoderError decoderError ->
      "Unable to decode metadata: " <> sformat build decoderError
    ShelleyGovernanceCmdVerifyPollError pollError ->
      renderGovernancePollError pollError
    ShelleyGovernanceCmdWriteFileError fileErr -> Text.pack (displayError fileErr)

runGovernanceCmd :: GovernanceCmd -> ExceptT ShelleyGovernanceCmdError IO ()
runGovernanceCmd (GovernanceMIRPayStakeAddressesCertificate mirpot vKeys rewards out) =
  runGovernanceMIRCertificatePayStakeAddrs mirpot vKeys rewards out
runGovernanceCmd (GovernanceMIRTransfer amt out direction) =
  runGovernanceMIRCertificateTransfer amt out direction
runGovernanceCmd (GovernanceGenesisKeyDelegationCertificate genVk genDelegVk vrfVk out) =
  runGovernanceGenesisKeyDelegationCertificate genVk genDelegVk vrfVk out
runGovernanceCmd (GovernanceUpdateProposal out eNo genVKeys ppUp mCostModelFp) =
  runGovernanceUpdateProposal out eNo genVKeys ppUp mCostModelFp
runGovernanceCmd (GovernanceCreatePoll prompt choices nonce out) =
  runGovernanceCreatePoll prompt choices nonce out
runGovernanceCmd (GovernanceAnswerPoll poll ix mOutFile) =
  runGovernanceAnswerPoll poll ix mOutFile
runGovernanceCmd (GovernanceVerifyPoll poll metadata mOutFile) =
  runGovernanceVerifyPoll poll metadata mOutFile
runGovernanceCmd (GovernanceActionCmd cmd) =
  runGovernanceActionCmd cmd

runGovernanceActionCmd :: GovernanceActionCmd -> ExceptT ShelleyGovernanceCmdError IO ()
runGovernanceActionCmd = \case
  GovernanceActionCreate -> runGovernanceAction

runGovernanceAction :: ExceptT ShelleyGovernanceCmdError IO ()
runGovernanceAction = do
  -- TODO CIP-1694
  --
  -- # Deposits
  -- The Ada Holder must provide a deposit of govDeposit Lovelace, which will be returned when
  -- the action is finalized (whether it is ratified, has been dropped, or has expired).
  -- The deposit amount will be added to the deposit pot, similar to stake key deposits.
  -- It will also be counted towards the stake of the reward address it will be paid back to,
  -- to not reduce the submitter's voting power to vote on their own (and competing) actions.
  --
  -- # Metadata
  -- Instead of specific dedicated fields in the transaction format, we could instead use the existing
  -- transaction metadata field.
  --
  -- Governance-related metadata can be clearly identified by registering a CIP-10 metadata label.
  -- Within that, the structure of the metadata can be determined by this CIP (exact format TBD), using
  -- an index to map the vote or governance action ID to the corresponding metadata URL and hash.
  --
  -- This avoids the need to add additional fields to the transaction body, at the risk of making it easier
  -- or submitters to ignore. However, since the required metadata can be empty (or can point to a
  -- non-resolving URL), it is already easy for submitters to not provide metadata, and so it is unclear
  -- whether this makes the situation worse.
  --
  -- Note that transaction metadata is never stored in the ledger state, so it would be up to clients to
  -- pair the metadata with the actions and votes in this alternative, and would not be available as a
  -- ledger state query.
  liftIO $ IO.hPutStrLn IO.stderr "TODO: implement"

runGovernanceMIRCertificatePayStakeAddrs
  :: Shelley.MIRPot
  -> [StakeAddress] -- ^ Stake addresses
  -> [Lovelace]     -- ^ Corresponding reward amounts (same length)
  -> File () Out
  -> ExceptT ShelleyGovernanceCmdError IO ()
runGovernanceMIRCertificatePayStakeAddrs mirPot sAddrs rwdAmts oFp = do

    unless (length sAddrs == length rwdAmts) $
      left $ ShelleyGovernanceCmdMIRCertificateKeyRewardMistmach
               (unFile oFp) (length sAddrs) (length rwdAmts)

    let sCreds  = map stakeAddressCredential sAddrs
        mirCert = makeMIRCertificate mirPot (StakeAddressesMIR $ zip sCreds rwdAmts)

    firstExceptT ShelleyGovernanceCmdTextEnvWriteError
      . newExceptT
      $ writeLazyByteStringFile oFp $ textEnvelopeToJSON (Just mirCertDesc) mirCert
  where
    mirCertDesc :: TextEnvelopeDescr
    mirCertDesc = "Move Instantaneous Rewards Certificate"

runGovernanceMIRCertificateTransfer
  :: Lovelace
  -> File () Out
  -> TransferDirection
  -> ExceptT ShelleyGovernanceCmdError IO ()
runGovernanceMIRCertificateTransfer ll oFp direction = do
  mirCert <- case direction of
                 TransferToReserves ->
                   return . makeMIRCertificate Shelley.TreasuryMIR $ SendToReservesMIR ll
                 TransferToTreasury ->
                   return . makeMIRCertificate Shelley.ReservesMIR $ SendToTreasuryMIR ll

  firstExceptT ShelleyGovernanceCmdTextEnvWriteError
    . newExceptT
    $ writeLazyByteStringFile oFp
    $ textEnvelopeToJSON (Just $ mirCertDesc direction) mirCert
 where
  mirCertDesc :: TransferDirection -> TextEnvelopeDescr
  mirCertDesc TransferToTreasury = "MIR Certificate Send To Treasury"
  mirCertDesc TransferToReserves = "MIR Certificate Send To Reserves"


runGovernanceGenesisKeyDelegationCertificate
  :: VerificationKeyOrHashOrFile GenesisKey
  -> VerificationKeyOrHashOrFile GenesisDelegateKey
  -> VerificationKeyOrHashOrFile VrfKey
  -> File () Out
  -> ExceptT ShelleyGovernanceCmdError IO ()
runGovernanceGenesisKeyDelegationCertificate genVkOrHashOrFp
                                             genDelVkOrHashOrFp
                                             vrfVkOrHashOrFp
                                             oFp = do
    genesisVkHash <- firstExceptT ShelleyGovernanceCmdKeyReadError
      . newExceptT
      $ readVerificationKeyOrHashOrTextEnvFile AsGenesisKey genVkOrHashOrFp
    genesisDelVkHash <-firstExceptT ShelleyGovernanceCmdKeyReadError
      . newExceptT
      $ readVerificationKeyOrHashOrTextEnvFile AsGenesisDelegateKey genDelVkOrHashOrFp
    vrfVkHash <- firstExceptT ShelleyGovernanceCmdKeyReadError
      . newExceptT
      $ readVerificationKeyOrHashOrFile AsVrfKey vrfVkOrHashOrFp
    firstExceptT ShelleyGovernanceCmdTextEnvWriteError
      . newExceptT
      $ writeLazyByteStringFile oFp
      $ textEnvelopeToJSON (Just genKeyDelegCertDesc)
      $ makeGenesisKeyDelegationCertificate genesisVkHash genesisDelVkHash vrfVkHash
  where
    genKeyDelegCertDesc :: TextEnvelopeDescr
    genKeyDelegCertDesc = "Genesis Key Delegation Certificate"

runGovernanceUpdateProposal
  :: File () Out
  -> EpochNo
  -> [VerificationKeyFile In]
  -- ^ Genesis verification keys
  -> ProtocolParametersUpdate
  -> Maybe FilePath -- ^ Cost models file path
  -> ExceptT ShelleyGovernanceCmdError IO ()
runGovernanceUpdateProposal upFile eNo genVerKeyFiles upPprams mCostModelFp = do
  finalUpPprams <- case mCostModelFp of
    Nothing -> return upPprams
    Just fp -> do
      costModelsBs <- handleIOExceptT (ShelleyGovernanceCmdCostModelReadError . FileIOError fp) $ LB.readFile fp

      cModels <- pure (eitherDecode costModelsBs)
        & onLeft (left . ShelleyGovernanceCmdCostModelsJsonDecodeErr fp . Text.pack)

      let costModels = fromAlonzoCostModels cModels

      when (null costModels) $ left (ShelleyGovernanceCmdEmptyCostModel fp)

      return $ upPprams {protocolUpdateCostModels = costModels}

  when (finalUpPprams == mempty) $ left ShelleyGovernanceCmdEmptyUpdateProposalError

  genVKeys <- sequence
    [ firstExceptT ShelleyGovernanceCmdTextEnvReadError . newExceptT $ readFileTextEnvelope (AsVerificationKey AsGenesisKey) vkeyFile
    | vkeyFile <- genVerKeyFiles
    ]
  let genKeyHashes = fmap verificationKeyHash genVKeys
      upProp = makeShelleyUpdateProposal finalUpPprams genKeyHashes eNo

  firstExceptT ShelleyGovernanceCmdTextEnvWriteError . newExceptT
    $ writeLazyByteStringFile upFile $ textEnvelopeToJSON Nothing upProp

runGovernanceCreatePoll
  :: Text
  -> [Text]
  -> Maybe Word
  -> File GovernancePoll Out
  -> ExceptT ShelleyGovernanceCmdError IO ()
runGovernanceCreatePoll govPollQuestion govPollAnswers govPollNonce out = do
  let poll = GovernancePoll{ govPollQuestion, govPollAnswers, govPollNonce }

  let description = fromString $ "An on-chain poll for SPOs: " <> Text.unpack govPollQuestion
  firstExceptT ShelleyGovernanceCmdTextEnvWriteError . newExceptT $
    writeFileTextEnvelope out (Just description) poll

  let metadata = asTxMetadata poll
        & metadataToJson TxMetadataJsonDetailedSchema

  let outPath = unFile out & Text.encodeUtf8 . Text.pack

  liftIO $ do
    BSC.hPutStrLn stderr $ mconcat
      [ "Poll created successfully.\n"
      , "Please submit a transaction using the resulting metadata.\n"
      ]
    BSC.hPutStrLn stdout (prettyPrintJSON metadata)
    BSC.hPutStrLn stderr $ mconcat
      [ "\n"
      , "Hint (1): Use '--json-metadata-detailed-schema' and '--metadata-json-file' "
      , "from the build or build-raw commands.\n"
      , "Hint (2): You can redirect the standard output of this command to a JSON "
      , "file to capture metadata.\n\n"
      , "Note: A serialized version of the poll suitable for sharing with "
      , "participants has been generated at '" <> outPath <> "'."
      ]

runGovernanceAnswerPoll
  :: File GovernancePoll In
  -> Maybe Word -- ^ Answer index
  -> Maybe (File () Out) -- ^ Output file
  -> ExceptT ShelleyGovernanceCmdError IO ()
runGovernanceAnswerPoll pollFile maybeChoice mOutFile = do
  poll <- firstExceptT ShelleyGovernanceCmdTextEnvReadError . newExceptT $
    readFileTextEnvelope AsGovernancePoll pollFile

  choice <- case maybeChoice of
    Nothing -> do
      askInteractively poll
    Just ix -> do
      validateChoice poll ix
      liftIO $ BSC.hPutStrLn stderr $ Text.encodeUtf8 $ Text.intercalate "\n"
        [ govPollQuestion poll
        , "→ " <> (govPollAnswers poll !! fromIntegral ix)
        , ""
        ]
      pure ix

  let pollAnswer = GovernancePollAnswer
        { govAnsPoll = hashGovernancePoll poll
        , govAnsChoice = choice
        }
  let metadata =
        metadataToJson TxMetadataJsonDetailedSchema (asTxMetadata pollAnswer)

  liftIO $ BSC.hPutStrLn stderr $ mconcat
      [ "Poll answer created successfully.\n"
      , "Please submit a transaction using the resulting metadata.\n"
      , "To be valid, the transaction must also be signed using a valid key\n"
      , "identifying your stake pool (e.g. your cold key).\n"
      ]

  lift (writeByteStringOutput mOutFile (prettyPrintJSON metadata))
    & onLeft (left . ShelleyGovernanceCmdWriteFileError)

  liftIO $ BSC.hPutStrLn stderr $ mconcat
      [ "\n"
      , "Hint (1): Use '--json-metadata-detailed-schema' and '--metadata-json-file' "
      , "from the build or build-raw commands.\n"
      , "Hint (2): You can redirect the standard output of this command to a JSON "
      , "file to capture metadata."
      ]
 where
  validateChoice :: GovernancePoll -> Word -> ExceptT ShelleyGovernanceCmdError IO ()
  validateChoice GovernancePoll{govPollAnswers} ix = do
    let maxAnswerIndex = length govPollAnswers - 1
    when (fromIntegral ix > maxAnswerIndex) $ left $
      ShelleyGovernanceCmdPollOutOfBoundAnswer maxAnswerIndex

  askInteractively :: GovernancePoll -> ExceptT ShelleyGovernanceCmdError IO Word
  askInteractively poll@GovernancePoll{govPollQuestion, govPollAnswers} = do
    liftIO $ BSC.hPutStrLn stderr $ Text.encodeUtf8 $ Text.intercalate "\n"
      ( govPollQuestion
      : [ "[" <> textShow ix <> "] " <> answer
        | (ix :: Int, answer) <- zip [0..] govPollAnswers
        ]
      )
    liftIO $ BSC.hPutStrLn stderr ""
    liftIO $ BSC.hPutStr stderr "Please indicate an answer (by index): "
    txt <- liftIO $ Text.hGetLine stdin
    liftIO $ BSC.hPutStrLn stderr ""
    case Text.decimal txt of
      Right (choice, rest) | Text.null rest ->
        choice <$ validateChoice poll choice
      _ ->
        left ShelleyGovernanceCmdPollInvalidChoice

runGovernanceVerifyPoll
  :: File GovernancePoll In
  -> File (Tx ()) In
  -> Maybe (File () Out) -- ^ Output file
  -> ExceptT ShelleyGovernanceCmdError IO ()
runGovernanceVerifyPoll pollFile txFile mOutFile = do
  poll <- firstExceptT ShelleyGovernanceCmdTextEnvReadError . newExceptT $
    readFileTextEnvelope AsGovernancePoll pollFile

  txFileOrPipe <- liftIO $ fileOrPipe (unFile txFile)
  tx <- firstExceptT ShelleyGovernanceCmdCddlError . newExceptT $
    readFileTx txFileOrPipe

  signatories <- firstExceptT ShelleyGovernanceCmdVerifyPollError . newExceptT $ pure $
    verifyPollAnswer poll tx

  liftIO $ IO.hPutStrLn stderr $ "Found valid poll answer with " <> show (length signatories) <> " signatories"

  lift (writeByteStringOutput mOutFile (prettyPrintJSON signatories))
    & onLeft (left . ShelleyGovernanceCmdWriteFileError)
