{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Testnet.Test.LedgerEvents.Gov.PredefinedNoConfidenceDRep
  ( hprop_check_predefined_no_confidence_drep
  ) where

import           Cardano.Api as Api
import           Cardano.Api.Error (displayError)
import           Cardano.Api.Ledger (StrictMaybe (..))
import qualified Cardano.Api.Ledger as L

import qualified Cardano.Ledger.Conway.Governance as L
import qualified Cardano.Ledger.Shelley.LedgerState as L
import           Cardano.Testnet
import           Cardano.Testnet.Test.LedgerEvents.Gov.PredefinedAbstainDRep
                   (delegateToAutomaticDRep, desiredPoolNumberProposalTest,
                   getDesiredPoolNumberValue, voteChangeProposal)

import           Prelude

import           Control.Monad (void)
import           Control.Monad.Catch (MonadCatch)
import           Control.Monad.State.Strict (MonadState (put), StateT)
import qualified Data.Map as Map
import           Data.String (fromString)
import qualified Data.Text as Text
import           Data.Word (Word32)
import           GHC.Stack (callStack)
import           Lens.Micro ((^.))
import           System.FilePath ((</>))

import           Testnet.Components.DReps (retrieveTransactionId, signTx, submitTx)
import           Testnet.Components.Query (EpochStateView, findLargestUtxoForPaymentKey,
                   getCurrentEpochNo, getEpochStateView, getMinDRepDeposit)
import           Testnet.Defaults (defaultDelegatorStakeKeyPair)
import qualified Testnet.Process.Cli as P
import qualified Testnet.Process.Run as H
import qualified Testnet.Property.Utils as H
import           Testnet.Runtime

import           Hedgehog
import qualified Hedgehog.Extras as H
import qualified Hedgehog.Extras.Stock.IO.Network.Sprocket as IO

-- | Execute me with:
-- @DISABLE_RETRIES=1 cabal test cardano-testnet-test --test-options '-p "/Predefined No Confidence DRep/"'@
hprop_check_predefined_no_confidence_drep :: Property
hprop_check_predefined_no_confidence_drep = H.integrationWorkspace "test-activity" $ \tempAbsBasePath' -> do
  -- Start a local test net
  conf@Conf { tempAbsPath } <- mkConf tempAbsBasePath'
  let tempAbsPath' = unTmpAbsPath tempAbsPath
      tempBaseAbsPath = makeTmpBaseAbsPath tempAbsPath

  work <- H.createDirectoryIfMissing $ tempAbsPath' </> "work"

  -- Create default testnet with 3 DReps and 3 stake holders delegated, one to each DRep.
  let ceo = ConwayEraOnwardsConway
      sbe = conwayEraOnwardsToShelleyBasedEra ceo
      era = toCardanoEra sbe
      cEra = AnyCardanoEra era
      fastTestnetOptions = cardanoDefaultTestnetOptions
        { cardanoEpochLength = 100
        , cardanoNodeEra = cEra
        , cardanoNumDReps = 3
        }

  testnetRuntime@TestnetRuntime
    { testnetMagic
    , poolNodes
    , wallets=wallet0:wallet1:wallet2:_
    , configurationFile
    }
    <- cardanoTestnetDefault fastTestnetOptions conf

  poolNode1 <- H.headM poolNodes
  poolSprocket1 <- H.noteShow $ nodeSprocket $ poolRuntime poolNode1
  execConfig <- H.mkExecConfig tempBaseAbsPath poolSprocket1 testnetMagic

  let socketName' = IO.sprocketName poolSprocket1
      socketBase = IO.sprocketBase poolSprocket1 -- /tmp
      socketPath = socketBase </> socketName'

  epochStateView <- getEpochStateView (File configurationFile) (File socketPath)

  startLedgerNewEpochStateLogging testnetRuntime tempAbsPath'

  H.note_ $ "Sprocket: " <> show poolSprocket1
  H.note_ $ "Abs path: " <> tempAbsBasePath'
  H.note_ $ "Socketpath: " <> socketPath
  H.note_ $ "Foldblocks config file: " <> configurationFile

  gov <- H.createDirectoryIfMissing $ work </> "governance"

  -- Create constitutional committee and check it exists
  constitutionalAction <- updateConstitutionalCommittee execConfig epochStateView configurationFile socketPath ceo work "committeeUpdate"
                                                        wallet0 Nothing [(3, "yes")] 10

  -- Do some proposal and vote yes with all the DReps
  -- and assert that proposal passes.
  initialDesiredNumberOfPools <- getDesiredPoolNumberValue epochStateView ceo

  let newNumberOfDesiredPools = fromIntegral (initialDesiredNumberOfPools + 1)

  firstProposalInfo <- desiredPoolNumberProposalTest execConfig epochStateView configurationFile socketPath ceo gov "firstProposal"
                                                     wallet1 Nothing [(3, "yes")] newNumberOfDesiredPools 0 (Just newNumberOfDesiredPools) 10

  -- Take the last two stake delegators and delegate them to "No Confidence".
  delegateToAlwaysNoConfidence execConfig epochStateView configurationFile socketPath sbe gov "delegateToNoConfidence1"
                               wallet2 (defaultDelegatorStakeKeyPair 2)
  delegateToAlwaysNoConfidence execConfig epochStateView configurationFile socketPath sbe gov "delegateToNoConfidence2"
                               wallet2 (defaultDelegatorStakeKeyPair 3)

  -- Do some other proposal and vote yes with all the DReps
  -- and assert the new proposal does NOT pass.
  let newNumberOfDesiredPools2 = fromIntegral (newNumberOfDesiredPools + 1)

  void $ desiredPoolNumberProposalTest execConfig epochStateView configurationFile socketPath ceo gov "secondProposal"
                                       wallet0 (Just firstProposalInfo) [(3, "yes")] newNumberOfDesiredPools2 3 (Just newNumberOfDesiredPools) 10

  -- Create a no confidence proposal and vote "no" to the proposal with all DReps.
  -- Assert the no confidence proposal passes.
  void $ testNoConfidenceProposal execConfig epochStateView configurationFile socketPath ceo gov "noConfidenceProposal"
                                  wallet1 constitutionalAction [(3, "no")] 10

waitTillCommittee
  :: (MonadTest m, MonadIO m, MonadCatch m)
  => Bool
  -> FilePath
  -> FilePath
  -> Integer
  -> m [(L.Credential L.ColdCommitteeRole L.StandardCrypto, EpochNo)]
waitTillCommittee exists configurationFile socketPath waitTillEpoch = do
  !foldResult
    <- runExceptT $ foldEpochState
                      (File configurationFile)
                      (File socketPath)
                      FullValidation
                      (EpochNo $ fromIntegral waitTillEpoch)
                      []
                      (\epochState _ _ -> foldEpochStateCommitteee exists epochState)
  case foldResult of
    Right (ConditionMet, committee) -> return committee
    Left (FoldBlocksApplyBlockError (TerminationEpochReached (EpochNo n))) ->
      if exists
      then H.failMessage callStack ("waitTillCommittee: Timed out when waiting for committee to get registered, on epoch: " <> show n)
      else H.failMessage callStack ("waitTillCommittee: Timed out when waiting for committee to disolve, on epoch: " <> show n)
    e -> H.failMessage callStack ("waitTillCommittee: foldEpochState failed with the following reason: " <> show e)

foldEpochStateCommitteee :: Bool -> AnyNewEpochState -> StateT [(L.Credential L.ColdCommitteeRole L.StandardCrypto, EpochNo)] IO LedgerStateCondition
foldEpochStateCommitteee hasCommittee anyNewEpochState = do
  let committee = filterCommittee anyNewEpochState
  put committee
  if null committee == not hasCommittee
  then return ConditionMet
  else return ConditionNotMet

filterCommittee :: AnyNewEpochState -> [(L.Credential L.ColdCommitteeRole L.StandardCrypto, EpochNo)]
filterCommittee (AnyNewEpochState sbe newEpochState) =
  caseShelleyToBabbageOrConwayEraOnwards
    (const $ error "filterNoCommittee: Only conway era supported")
    (const $ do
      let rState = L.extractDRepPulsingState $ newEpochState ^. L.newEpochStateGovStateL . L.drepPulsingStateGovStateL
          ensCommittee = rState ^. L.rsEnactStateL . L.ensCommitteeL
      case ensCommittee of
        SNothing -> []
        SJust x -> Map.toList $ L.committeeMembers x
    )
    sbe

updateConstitutionalCommittee
  :: (MonadTest m, MonadIO m, H.MonadAssertion m, MonadCatch m, Foldable t)
  => H.ExecConfig
  -> EpochStateView
  -> FilePath
  -> FilePath
  -> ConwayEraOnwards ConwayEra
  -> FilePath
  -> FilePath
  -> PaymentKeyInfo
  -> Maybe (String, Word32)
  -> t (Int, String)
  -> Integer
  -> m (String, Word32)
updateConstitutionalCommittee execConfig epochStateView configurationFile socketPath ceo work prefix
                              wallet previousProposalInfo votes waitTillEpoch = do
  baseDir <- H.createDirectoryIfMissing $ work </> prefix

  let propVotes :: [(String, Int)]
      propVotes = zip (concatMap (uncurry replicate) votes) [1..]
  annotateShow propVotes

  let coldVKeyFile = baseDir </> "cold-key.vkey"
      coldSKeyFile = baseDir </> "cold-key.skey"

  void $ H.execCli' execConfig
      [ "conway", "governance", "committee", "key-gen-cold"
      , "--cold-verification-key-file", coldVKeyFile
      , "--cold-signing-key-file", coldSKeyFile
      ]

  coldKeyHash <- Text.unpack . Text.strip . Text.pack <$> H.execCli' execConfig
    [ "conway", "governance", "committee", "key-hash"
    , "--verification-key-file", coldVKeyFile
    ]

  thisProposal@(governanceActionTxId, governanceActionIndex) <-
    makeUpdateConstitutionalCommitteeProposal execConfig epochStateView (File configurationFile) (File socketPath)
                                              ceo baseDir "proposal" previousProposalInfo [coldKeyHash] wallet

  voteChangeProposal execConfig epochStateView ceo baseDir "vote"
                     governanceActionTxId governanceActionIndex propVotes (zip (repeat "yes") [1..3]) wallet

  (EpochNo epochAfterProp) <- getCurrentEpochNo epochStateView
  H.note_ $ "Epoch after \"" <> prefix <> "\" prop: " <> show epochAfterProp

  committee <- waitTillCommittee True configurationFile socketPath waitTillEpoch
  H.note_ $ show committee

  return thisProposal

makeUpdateConstitutionalCommitteeProposal
  :: (H.MonadAssertion m, MonadTest m, MonadCatch m, MonadIO m, Foldable f)
  => H.ExecConfig
  -> EpochStateView
  -> NodeConfigFile 'In
  -> SocketPath
  -> ConwayEraOnwards ConwayEra
  -> FilePath
  -> String
  -> Maybe (String, Word32)
  -> f String
  -> PaymentKeyInfo
  -> m (String, Word32)
makeUpdateConstitutionalCommitteeProposal execConfig epochStateView configurationFile socketPath
                                          ceo work prefix prevGovActionInfo coldKeyHashes wallet = do

  let sbe = conwayEraOnwardsToShelleyBasedEra ceo
      era = toCardanoEra sbe
      cEra = AnyCardanoEra era

  baseDir <- H.createDirectoryIfMissing $ work </> prefix

  let stakeVkeyFp = baseDir </> "stake.vkey"
      stakeSKeyFp = baseDir </> "stake.skey"

  _ <- P.cliStakeAddressKeyGen baseDir
         $ P.KeyNames { P.verificationKeyFile = stakeVkeyFp
                      , P.signingKeyFile = stakeSKeyFp
                      }

  proposalAnchorFile <- H.note $ baseDir </> "sample-proposal-anchor"
  H.writeFile proposalAnchorFile "dummy anchor data"

  proposalAnchorDataHash <- H.execCli' execConfig
    [ "conway", "governance"
    , "hash", "anchor-data", "--file-text", proposalAnchorFile
    ]

  minDRepDeposit <- getMinDRepDeposit epochStateView ceo

  proposalFile <- H.note $ baseDir </> "sample-proposal-file"

  void $ H.execCli' execConfig $
    [ "conway", "governance", "action", "update-committee"
    , "--testnet"
    , "--governance-action-deposit", show @Integer minDRepDeposit
    , "--deposit-return-stake-verification-key-file", stakeVkeyFp
    ] ++ concatMap (\(prevGovernanceActionTxId, prevGovernanceActionIndex) ->
                      [ "--prev-governance-action-tx-id", prevGovernanceActionTxId
                      , "--prev-governance-action-index", show prevGovernanceActionIndex
                      ]) prevGovActionInfo ++
    [ "--anchor-url", "https://tinyurl.com/3wrwb2as"
    , "--anchor-data-hash", proposalAnchorDataHash
    ] ++ concatMap (\keyHash ->
                    [ "--add-cc-cold-verification-key-hash", keyHash
                    , "--epoch", show (100 :: Int)
                    ]) coldKeyHashes ++
    [ "--threshold", "0"
    , "--out-file", proposalFile
    ]

  proposalBody <- H.note $ baseDir </> "tx.body"
  txIn <- findLargestUtxoForPaymentKey epochStateView sbe wallet

  void $ H.execCli' execConfig
    [ "conway", "transaction", "build"
    , "--change-address", Text.unpack $ paymentKeyInfoAddr wallet
    , "--tx-in", Text.unpack $ renderTxIn txIn
    , "--proposal-file", proposalFile
    , "--out-file", proposalBody
    ]

  signedProposalTx <- signTx execConfig cEra baseDir "signed-proposal"
                             (File proposalBody) [paymentKeyInfoPair wallet]

  submitTx execConfig cEra signedProposalTx

  governanceActionTxId <- retrieveTransactionId execConfig signedProposalTx

  !propSubmittedResult <- findCondition (maybeExtractGovernanceActionIndex sbe (fromString governanceActionTxId))
                                        (unFile configurationFile)
                                        (unFile socketPath)
                                        (EpochNo 10)

  governanceActionIndex <- case propSubmittedResult of
                             Left e ->
                               H.failMessage callStack
                                 $ "findCondition failed with: " <> displayError e
                             Right Nothing ->
                               H.failMessage callStack "Couldn't find proposal."
                             Right (Just a) -> return a

  return (governanceActionTxId, governanceActionIndex)

delegateToAlwaysNoConfidence
  :: (MonadTest m, MonadIO m, H.MonadAssertion m, MonadCatch m)
  => H.ExecConfig
  -> EpochStateView
  -> FilePath
  -> FilePath
  -> ShelleyBasedEra ConwayEra
  -> FilePath
  -> String
  -> PaymentKeyInfo
  -> StakingKeyPair
  -> m ()
delegateToAlwaysNoConfidence execConfig epochStateView configurationFile socketPath sbe work prefix =
  delegateToAutomaticDRep execConfig epochStateView configurationFile socketPath sbe work prefix
                          "--always-no-confidence"

testNoConfidenceProposal
  :: (MonadTest m, MonadIO m, H.MonadAssertion m, MonadCatch m, Foldable t)
  => H.ExecConfig
  -> EpochStateView
  -> FilePath
  -> FilePath
  -> ConwayEraOnwards ConwayEra
  -> FilePath
  -> FilePath
  -> PaymentKeyInfo
  -> (String, Word32)
  -> t (Int, String)
  -> Integer
  -> m (String, Word32)
testNoConfidenceProposal execConfig epochStateView configurationFile socketPath ceo work prefix
                         wallet previousProposalInfo votes waitTillEpoch = do
  baseDir <- H.createDirectoryIfMissing $ work </> prefix

  let propVotes :: [(String, Int)]
      propVotes = zip (concatMap (uncurry replicate) votes) [1..]
  annotateShow propVotes

  thisProposal@(governanceActionTxId, governanceActionIndex) <-
    makeNoConfidenceProposal execConfig epochStateView (File configurationFile) (File socketPath)
                             ceo baseDir "proposal" previousProposalInfo wallet

  voteChangeProposal execConfig epochStateView ceo baseDir "vote"
                     governanceActionTxId governanceActionIndex propVotes (zip (repeat "yes") [1..3]) wallet

  (EpochNo epochAfterProp) <- getCurrentEpochNo epochStateView
  H.note_ $ "Epoch after \"" <> prefix <> "\" prop: " <> show epochAfterProp

  committee <- waitTillCommittee False configurationFile socketPath waitTillEpoch
  H.note_ $ show committee

  return thisProposal


makeNoConfidenceProposal
  :: (H.MonadAssertion m, MonadTest m, MonadCatch m, MonadIO m)
  => H.ExecConfig
  -> EpochStateView
  -> NodeConfigFile 'In
  -> SocketPath
  -> ConwayEraOnwards ConwayEra
  -> FilePath
  -> String
  -> (String, Word32)
  -> PaymentKeyInfo
  -> m (String, Word32)
makeNoConfidenceProposal execConfig epochStateView configurationFile socketPath
                         ceo work prefix (prevGovernanceActionTxId, prevGovernanceActionIndex) wallet = do
  let sbe = conwayEraOnwardsToShelleyBasedEra ceo
      era = toCardanoEra sbe
      cEra = AnyCardanoEra era

  baseDir <- H.createDirectoryIfMissing $ work </> prefix

  let stakeVkeyFp = baseDir </> "stake.vkey"
      stakeSKeyFp = baseDir </> "stake.skey"

  _ <- P.cliStakeAddressKeyGen baseDir
         $ P.KeyNames { P.verificationKeyFile = stakeVkeyFp
                      , P.signingKeyFile = stakeSKeyFp
                      }

  proposalAnchorFile <- H.note $ baseDir </> "sample-proposal-anchor"
  H.writeFile proposalAnchorFile "dummy anchor data"

  proposalAnchorDataHash <- H.execCli' execConfig
    [ "conway", "governance"
    , "hash", "anchor-data", "--file-text", proposalAnchorFile
    ]

  minDRepDeposit <- getMinDRepDeposit epochStateView ceo

  proposalFile <- H.note $ baseDir </> "sample-proposal-file"

  void $ H.execCli' execConfig
    [ "conway", "governance", "action", "create-no-confidence"
    , "--testnet"
    , "--governance-action-deposit", show @Integer minDRepDeposit
    , "--deposit-return-stake-verification-key-file", stakeVkeyFp
    , "--prev-governance-action-tx-id", prevGovernanceActionTxId
    , "--prev-governance-action-index", show prevGovernanceActionIndex
    , "--anchor-url", "https://tinyurl.com/3wrwb2as"
    , "--anchor-data-hash", proposalAnchorDataHash
    , "--out-file", proposalFile
    ]

  proposalBody <- H.note $ baseDir </> "tx.body"
  txIn <- findLargestUtxoForPaymentKey epochStateView sbe wallet

  void $ H.execCli' execConfig
    [ "conway", "transaction", "build"
    , "--change-address", Text.unpack $ paymentKeyInfoAddr wallet
    , "--tx-in", Text.unpack $ renderTxIn txIn
    , "--proposal-file", proposalFile
    , "--out-file", proposalBody
    ]

  signedProposalTx <- signTx execConfig cEra baseDir "signed-proposal"
                             (File proposalBody) [paymentKeyInfoPair wallet]

  submitTx execConfig cEra signedProposalTx

  governanceActionTxId <- retrieveTransactionId execConfig signedProposalTx

  !propSubmittedResult <- findCondition (maybeExtractGovernanceActionIndex sbe (fromString governanceActionTxId))
                                        (unFile configurationFile)
                                        (unFile socketPath)
                                        (EpochNo 30)

  governanceActionIndex <- case propSubmittedResult of
                             Left e ->
                               H.failMessage callStack
                                 $ "findCondition failed with: " <> displayError e
                             Right Nothing ->
                               H.failMessage callStack "Couldn't find proposal."
                             Right (Just a) -> return a

  return (governanceActionTxId, governanceActionIndex)
