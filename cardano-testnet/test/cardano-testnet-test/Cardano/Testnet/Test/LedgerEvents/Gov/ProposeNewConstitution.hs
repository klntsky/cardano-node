{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Testnet.Test.LedgerEvents.Gov.ProposeNewConstitution
  ( hprop_ledger_events_propose_new_constitution
  , foldBlocksCheckProposalWasSubmitted
  , retrieveGovernanceActionIndex
  ) where

import           Cardano.Api as Api
import           Cardano.Api.Error (displayError)
import           Cardano.Api.Shelley

import qualified Cardano.Crypto.Hash as L
import qualified Cardano.Ledger.Conway.Governance as L
import qualified Cardano.Ledger.Conway.Governance as Ledger
import qualified Cardano.Ledger.Hashes as L
import qualified Cardano.Ledger.Shelley.LedgerState as L
import           Cardano.Testnet

import           Prelude

import           Control.Monad
import           Control.Monad.State.Strict (StateT)
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.Maybe.Strict
import           Data.String
import qualified Data.Text as Text
import           Data.Word
import           GHC.IO.Exception (IOException)
import           GHC.Stack (HasCallStack, callStack)
import           Lens.Micro
import           System.FilePath ((</>))

import           Testnet.Components.Configuration
import           Testnet.Components.Query
import           Testnet.Defaults
import qualified Testnet.Process.Cli as P
import qualified Testnet.Process.Run as H
import qualified Testnet.Property.Utils as H
import           Testnet.Runtime

import           Hedgehog
import qualified Hedgehog.Extras as H
import qualified Hedgehog.Extras.Stock.IO.Network.Sprocket as IO


newtype AdditionalCatcher
  = IOE IOException
  deriving Show

-- | Execute me with:
-- @DISABLE_RETRIES=1 cabal test cardano-testnet-test --test-options '-p "/ProposeAndRatifyNewConstitution/"'@
hprop_ledger_events_propose_new_constitution :: Property
hprop_ledger_events_propose_new_constitution = H.integrationWorkspace "propose-new-constitution" $ \tempAbsBasePath' -> do
  -- Start a local test net
  conf@Conf { tempAbsPath } <- mkConf tempAbsBasePath'
  let tempAbsPath' = unTmpAbsPath tempAbsPath
      tempBaseAbsPath = makeTmpBaseAbsPath tempAbsPath

  work <- H.createDirectoryIfMissing $ tempAbsPath' </> "work"

  let sbe = ShelleyBasedEraConway
      era = toCardanoEra sbe
      cEra = AnyCardanoEra era
      fastTestnetOptions = cardanoDefaultTestnetOptions
        { cardanoEpochLength = 100
        , cardanoSlotLength = 0.1
        , cardanoNodeEra = cEra
        }

  TestnetRuntime
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

  H.note_ $ "Sprocket: " <> show poolSprocket1
  H.note_ $ "Abs path: " <> tempAbsBasePath'
  H.note_ $ "Socketpath: " <> socketPath
  H.note_ $ "Foldblocks config file: " <> configurationFile

  -- Create Conway constitution
  gov <- H.createDirectoryIfMissing $ work </> "governance"
  proposalAnchorFile <- H.note $ work </> gov </> "sample-proposFal-anchor"
  consitutionFile <- H.note $ work </> gov </> "sample-constitution"
  constitutionActionFp <- H.note $ work </> gov </> "constitution.action"

  H.writeFile proposalAnchorFile "dummy anchor data"
  H.writeFile consitutionFile "dummy constitution data"
  constitutionHash <- H.execCli' execConfig
    [ "conway", "governance"
    , "hash", "anchor-data", "--file-text", consitutionFile
    ]

  proposalAnchorDataHash <- H.execCli' execConfig
    [ "conway", "governance"
    , "hash", "anchor-data", "--file-text", proposalAnchorFile
    ]

  let stakeVkeyFp = gov </> "stake.vkey"
      stakeSKeyFp = gov </> "stake.skey"

  _ <- P.cliStakeAddressKeyGen tempAbsPath'
         $ P.KeyNames { P.verificationKeyFile = stakeVkeyFp
                      , P.signingKeyFile = stakeSKeyFp
                      }

  -- TODO share this piece of code that is duplicated in multiple tests
  let drepVkeyFp :: Int -> FilePath
      drepVkeyFp n = tempAbsPath' </> "drep-keys" </> ("drep" <> show n) </> "drep.vkey"

      drepSKeyFp :: Int -> FilePath
      drepSKeyFp n = tempAbsPath' </> "drep-keys" </> ("drep" <> show n) </> "drep.skey"

  -- Create Drep registration certificates
  let drepCertFile :: Int -> FilePath
      drepCertFile n = gov </> "drep-keys" <>"drep" <> show n <> ".regcert"
  forM_ [1..3] $ \n -> do
    H.execCli' execConfig
       [ "conway", "governance", "drep", "registration-certificate"
       , "--drep-verification-key-file", drepVkeyFp n
       , "--key-reg-deposit-amt", show @Int 1_000_000 -- TODO: retrieve this from conway genesis.
       , "--out-file", drepCertFile n
       ]

  -- Retrieve UTxOs for registration submission
  txin1 <- findLargestUtxoForPaymentKey epochStateView sbe wallet0

  drepRegTxbodyFp <- H.note $ work </> "drep.registration.txbody"
  drepRegTxSignedFp <- H.note $ work </> "drep.registration.tx"

  void $ H.execCli' execConfig
    [ "conway", "transaction", "build"
    , "--change-address", Text.unpack $ paymentKeyInfoAddr wallet0
    , "--tx-in", Text.unpack $ renderTxIn txin1
    , "--tx-out", Text.unpack (paymentKeyInfoAddr wallet1) <> "+" <> show @Int 5_000_000
    , "--certificate-file", drepCertFile 1
    , "--certificate-file", drepCertFile 2
    , "--certificate-file", drepCertFile 3
    , "--witness-override", show @Int 4
    , "--out-file", drepRegTxbodyFp
    ]

  void $ H.execCli' execConfig
    [ "conway", "transaction", "sign"
    , "--tx-body-file", drepRegTxbodyFp
    , "--signing-key-file", paymentSKey $ paymentKeyInfoPair wallet0
    , "--signing-key-file", drepSKeyFp 1
    , "--signing-key-file", drepSKeyFp 2
    , "--signing-key-file", drepSKeyFp 3
    , "--out-file", drepRegTxSignedFp
    ]

  void $ H.execCli' execConfig
    [ "conway", "transaction", "submit"
    , "--tx-file", drepRegTxSignedFp
    ]

  -- Create constitution proposal
  guardRailScript <- H.note $ work </> "guard-rail-script.plutusV3"
  H.writeFile guardRailScript $ Text.unpack plutusV3NonSpendingScript
  -- TODO: Update help text for policyid. The script hash is not
  -- only useful for minting scripts
  constitutionScriptHash <- filter (/= '\n') <$>
    H.execCli' execConfig
      [ anyEraToString cEra, "transaction"
      , "policyid"
      , "--script-file", guardRailScript
      ]
  void $ H.execCli' execConfig
    [ "conway", "governance", "action", "create-constitution"
    , "--testnet"
    , "--governance-action-deposit", show @Int 1_000_000 -- TODO: retrieve this from conway genesis.
    , "--deposit-return-stake-script-file", guardRailScript
    , "--anchor-url", "https://tinyurl.com/3wrwb2as"
    , "--anchor-data-hash", proposalAnchorDataHash
    , "--constitution-url", "https://tinyurl.com/2pahcy6z"
    , "--constitution-hash", constitutionHash
    , "--constitution-script-hash", constitutionScriptHash
    , "--out-file", constitutionActionFp
    ]

  txbodyFp <- H.note $ work </> "tx.body"
  txbodySignedFp <- H.note $ work </> "tx.body.signed"

  txin2 <- findLargestUtxoForPaymentKey epochStateView sbe wallet1

  void $ H.execCli' execConfig
    [ "conway", "transaction", "build"
    , "--change-address", Text.unpack $ paymentKeyInfoAddr wallet1
    , "--tx-in", Text.unpack $ renderTxIn txin2
    , "--tx-out", Text.unpack (paymentKeyInfoAddr wallet0) <> "+" <> show @Int 5_000_000
    , "--proposal-file", constitutionActionFp
    , "--out-file", txbodyFp
    ]

  void $ H.execCli' execConfig
    [ "conway", "transaction", "sign"
    , "--tx-body-file", txbodyFp
    , "--signing-key-file", paymentSKey $ paymentKeyInfoPair wallet1
    , "--out-file", txbodySignedFp
    ]

  void $ H.execCli' execConfig
    [ "conway", "transaction", "submit"
    , "--tx-file", txbodySignedFp
    ]

  txidString <- mconcat . lines <$> H.execCli' execConfig
    [ "transaction", "txid"
    , "--tx-file", txbodySignedFp
    ]
  !propSubmittedResult
    <- runExceptT $ handleIOExceptT IOE
                  $ runExceptT $ foldBlocks
                      (File configurationFile)
                      (File socketPath)
                      FullValidation
                      Nothing -- Initial accumulator state
                      (foldBlocksCheckProposalWasSubmitted (fromString txidString))

  newProposalEvents <- case propSubmittedResult of
                        Left (IOE e) ->
                          H.failMessage callStack
                            $ "foldBlocksCheckProposalWasSubmitted failed with: " <> show e
                        Right (Left e) ->
                          H.failMessage callStack
                            $ "foldBlocksCheckProposalWasSubmitted failed with: " <> displayError e
                        Right (Right events) -> return events

  governanceActionIndex <- retrieveGovernanceActionIndex newProposalEvents

  let voteFp :: Int -> FilePath
      voteFp n = work </> gov </> "vote-" <> show n

  -- Proposal was successfully submitted, now we vote on the proposal and confirm it was ratified
  forM_ [1..3] $ \n -> do
    H.execCli' execConfig
      [ "conway", "governance", "vote", "create"
      , "--yes"
      , "--governance-action-tx-id", txidString
      , "--governance-action-index", show @Word32 governanceActionIndex
      , "--drep-verification-key-file", drepVkeyFp n
      , "--out-file", voteFp n
      ]

  -- We need more UTxOs

  txin3 <- findLargestUtxoForPaymentKey epochStateView sbe wallet0

  voteTxFp <- H.note $ work </> gov </> "vote.tx"
  voteTxBodyFp <- H.note $ work </> gov </> "vote.txbody"

  -- Submit votes
  void $ H.execCli' execConfig
    [ "conway", "transaction", "build"
    , "--change-address", Text.unpack $ paymentKeyInfoAddr wallet0
    , "--tx-in", Text.unpack $ renderTxIn txin3
    , "--tx-out", Text.unpack (paymentKeyInfoAddr wallet1) <> "+" <> show @Int 3_000_000
    , "--vote-file", voteFp 1
    , "--vote-file", voteFp 2
    , "--vote-file", voteFp 3
    , "--witness-override", show @Int 4
    , "--out-file", voteTxBodyFp
    ]


  void $ H.execCli' execConfig
    [ "conway", "transaction", "sign"
    , "--tx-body-file", voteTxBodyFp
    , "--signing-key-file", paymentSKey $ paymentKeyInfoPair wallet0
    , "--signing-key-file", drepSKeyFp 1
    , "--signing-key-file", drepSKeyFp 2
    , "--signing-key-file", drepSKeyFp 3
    , "--out-file", voteTxFp
    ]

  void $ H.execCli' execConfig
    [ "conway", "transaction", "submit"
    , "--tx-file", voteTxFp
    ]
  H.threadDelay 10_000_000
  -- We check that constitution was succcessfully ratified

  !eConstitutionAdopted
    <- runExceptT $ foldEpochState
                      (File configurationFile)
                      (File socketPath)
                      FullValidation
                      (EpochNo 10)
                      ()
                      (\epochState _ _ -> foldBlocksCheckConstitutionWasRatified constitutionHash constitutionScriptHash epochState)

  void $ evalEither eConstitutionAdopted

  -- TODO: Attempt a protocol parameters update (witnessed with guard rail script)
  pparamsUpdateFp <- H.note $ work </> "protocol-parameters-upate.action"
  void $ H.execCli' execConfig
      [ anyEraToString cEra, "governance", "action", "create-protocol-parameters-update"
      , "--testnet"
      , "--governance-action-deposit", show @Int 1_000_000 -- TODO: retrieve this from conway genesis.
      , "--deposit-return-stake-verification-key-file", stakeVkeyFp
      , "--anchor-url", "https://tinyurl.com/3wrwb2as"
      , "--anchor-data-hash", proposalAnchorDataHash
      , "--constitution-script-hash", constitutionScriptHash
      , "--committee-term-length", show @Int 1000
      , "--out-file", pparamsUpdateFp
      ]
  updateProposalTxBody <- H.note $ work </> "update-proposal.txbody"
  txin4 <- findLargestUtxoForPaymentKey epochStateView sbe wallet0
  txin5 <- findLargestUtxoForPaymentKey epochStateView sbe wallet1
  txinColl <- findLargestUtxoForPaymentKey epochStateView sbe wallet2

  void $ H.execCli' execConfig
    [ anyEraToString cEra, "transaction", "build"
    , "--change-address", Text.unpack $ paymentKeyInfoAddr wallet0
    , "--tx-in", Text.unpack $ renderTxIn txin4
    , "--tx-in", Text.unpack $ renderTxIn txin5
    , "--tx-in-collateral", Text.unpack $ renderTxIn txinColl
    , "--tx-out", Text.unpack (paymentKeyInfoAddr wallet1) <> "+" <> show @Int 5_000_000
    , "--proposal-file", pparamsUpdateFp
    , "--proposal-script-file", guardRailScript
    , "--proposal-redeemer-value", "0"
    , "--witness-override", show @Int 12
    , "--out-file", updateProposalTxBody
    ]

  updateProposalTx <- H.note $ work </> "update-proposal.tx"
  void $ H.execCli' execConfig
    [ "conway", "transaction", "sign"
    , "--tx-body-file", updateProposalTxBody
    , "--signing-key-file", paymentSKey $ paymentKeyInfoPair wallet0
    , "--signing-key-file", paymentSKey $ paymentKeyInfoPair wallet1
    , "--signing-key-file", paymentSKey $ paymentKeyInfoPair wallet2
    , "--out-file", updateProposalTx
    ]

  void $ H.execCli' execConfig
    [ "conway", "transaction", "submit"
    , "--tx-file", updateProposalTx
    ]

foldBlocksCheckProposalWasSubmitted
  :: TxId -- TxId of submitted tx
  -> Env
  -> LedgerState
  -> [LedgerEvent]
  -> BlockInMode -- Block i
  -> Maybe LedgerEvent -- ^ Accumulator at block i - 1
  -> IO (Maybe LedgerEvent, FoldStatus) -- ^ Accumulator at block i and fold status
foldBlocksCheckProposalWasSubmitted txid _ _ allEvents _ _ = do
  let newGovProposals = filter (filterNewGovProposals txid) allEvents
  pure $ case newGovProposals of
    [] ->  (Nothing, ContinueFold)
    newGovProposal:_ -> (Just newGovProposal, StopFold)


retrieveGovernanceActionIndex
  :: (HasCallStack, MonadTest m)
  => Maybe LedgerEvent -> m Word32
retrieveGovernanceActionIndex mEvent = do
  case mEvent of
    Nothing -> H.failMessage callStack "retrieveGovernanceActionIndex: No new governance proposals found"
    Just (NewGovernanceProposals _ (AnyProposals props)) ->
    -- In this test there will only be one
        H.headM [i | Ledger.GovActionIx i
                    <- map Ledger.gaidGovActionIx . Map.keys $ Ledger.proposalsActionsMap props ]
    Just unexpectedEvent ->
      H.failMessage callStack
        $ mconcat ["retrieveGovernanceActionIndex: Expected NewGovernanceProposals, got: "
                  , show unexpectedEvent
                  ]


filterNewGovProposals :: TxId -> LedgerEvent -> Bool
filterNewGovProposals txid (NewGovernanceProposals eventTxId (AnyProposals props)) =
  let _govActionStates = Ledger.proposalsActionsMap props
  in fromShelleyTxId eventTxId == txid
filterNewGovProposals _ _ = False


foldBlocksCheckConstitutionWasRatified
  :: String -- submitted constitution hash
  -> String -- submitted guard rail script hash
  -> AnyNewEpochState
  -> StateT s IO LedgerStateCondition -- ^ Accumulator at block i and fold status
foldBlocksCheckConstitutionWasRatified submittedConstitutionHash submittedGuardRailScriptHash anyNewEpochState =
  if filterRatificationState submittedConstitutionHash submittedGuardRailScriptHash anyNewEpochState
  then return ConditionMet
  else return ConditionNotMet

-- cgsDRepPulsingStateL . ratifyStateL
filterRatificationState
  :: String -- ^ Submitted constitution anchor hash
  -> String -- ^ Submitted guard rail script hash
  -> AnyNewEpochState
  -> Bool
filterRatificationState c guardRailScriptHash (AnyNewEpochState sbe newEpochState) =
  caseShelleyToBabbageOrConwayEraOnwards
    (const $ error "filterRatificationState: Only conway era supported")

    (const $ do
      let rState = Ledger.extractDRepPulsingState $ newEpochState ^. L.newEpochStateGovStateL . L.drepPulsingStateGovStateL
          constitution = rState ^. Ledger.rsEnactStateL . Ledger.ensConstitutionL
          constitutionAnchorHash = Ledger.anchorDataHash $ Ledger.constitutionAnchor constitution
          L.ScriptHash constitutionScriptHash = fromMaybe (error "filterRatificationState: consitution does not have a guardrail script")
                                                $ strictMaybeToMaybe $ constitution ^. Ledger.constitutionScriptL
      Text.pack c == renderSafeHashAsHex constitutionAnchorHash && L.hashToTextAsHex constitutionScriptHash == Text.pack guardRailScriptHash

    )
    sbe

