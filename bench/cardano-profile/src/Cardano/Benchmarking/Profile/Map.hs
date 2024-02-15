{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

--------------------------------------------------------------------------------

module Cardano.Benchmarking.Profile.Map (
  byName, profiles
, profilesNoEra
) where

--------------------------------------------------------------------------------

import           Prelude
import           Data.Function ((&))
import qualified Data.Map.Strict as Map
import qualified Data.Scientific as Scientific

import qualified Cardano.Benchmarking.Profile as P
import qualified Cardano.Benchmarking.Profile.Types as Types

--------------------------------------------------------------------------------

byName :: String -> Types.Profile
byName name = (Map.!) profiles name

--------------------------------------------------------------------------------

profiles :: Map.Map String Types.Profile
profiles = foldMap
  (\profile -> Map.fromList $
    let
        -- Add eras, like "ci-test-notracer-[alra|alzo|bage|coay|mary|shey]"
        addEra = \p era suffix ->
          let name = Types.name p
              newName = name ++ "-" ++ suffix
          in  (newName, p {Types.name = newName, Types.era = era})
    in 
        [ addEra profile Types.Allegra "alra"
        , addEra profile Types.Shelley "shey"
        , addEra profile Types.Mary    "mary"
        , addEra profile Types.Alonzo  "alzo"
        , addEra profile Types.Babbage "bage"
        , addEra profile Types.Conway  "coay"
        ]
  )
  profilesNoEra

-- The defaults
--------------------------------------------------------------------------------

dummy :: Types.Profile
dummy = Types.Profile {
    Types.name = ""
  , Types.desc = Nothing
  , Types.composition = Types.Composition {
      Types.locations = []
    , Types.n_bft_hosts = 0
    , Types.n_singular_hosts = 0
    , Types.n_dense_hosts = 0
    , Types.dense_pool_density = 0
    , Types.with_proxy = False
    , Types.with_explorer = False
    , Types.topology = Types.Line
    , Types.with_chaindb_server = Nothing
    , Types.n_hosts = 0
    , Types.n_pools = 0
    , Types.n_singular_pools = 0
    , Types.n_dense_pools = 0
    , Types.n_pool_hosts = 0
  }
  , Types.era = Types.Allegra
--  , Types.genesis = Nothing
  , Types.scenario = Types.Idle
  , Types.node = Types.Node {
      Types.rts_flags_override = []
    , Types.shutdown_on_slot_synced = Nothing
    , Types.shutdown_on_block_synced = Nothing
    , Types.tracing_backend = ""
    , Types.nodeTracer = False
    , Types.verbatim = Types.NodeVerbatim Nothing
  }
  , Types.tracer = Types.Tracer {
      Types.rtview = False
    , Types.ekg = False
    , Types.withresources = False
  }
  , Types.generator = Types.Generator {
      Types.add_tx_size = 0
    , Types.init_cooldown = 0
    , Types.inputs_per_tx = 0
    , Types.outputs_per_tx = 0
    , Types.tx_fee = 0
    , Types.epochs = 0
    , Types.tps = 0
    , Types.plutus = Nothing
    , Types.tx_count = 0
  }
  , Types.analysis = Types.Analysis {
      Types.analysisType = Nothing
    , Types.cluster_base_startup_overhead_s = 0
    , Types.start_log_spread_s = 0
    , Types.last_log_spread_s = 0
    , Types.silence_since_last_block_s = 0
    , Types.tx_loss_ratio = Scientific.fromFloatDigits (0 :: Double)
    , Types.finish_patience = 0
    , Types.filters = []
    , Types.filter_exprs = []
    , Types.minimum_chain_density = Scientific.fromFloatDigits (0 :: Double)
    , Types.cluster_startup_overhead_s = 0
  }
--  , Types.overlay = mempty
}

-- TODO: forge-stress and forge-stress-light have the same .node.shutdown_on_slot_synced
-- Adding a P.nameSuffix was abandoned to keep the code `grep` friendly!
profilesNoEra :: Map.Map String Types.Profile
-- Names:
-- wb profile all-profiles | jq .[] | jq -r .name | sort | uniq | grep "\-bage"
profilesNoEra = Map.fromList $ map (\p -> (Types.name p, p)) $
  ------------------------------------------------------------------------------
  -- fast: 2 nodes, FixedLoaded and "--shutdown-on-block-synced 1"
  ------------------------------------------------------------------------------
  let fast =   dummy
             & P.uniCircle . P.hosts 2
             . P.loopback
             . P.fixedLoaded . P.generatorTps 15
             . P.shutdownOnBlock 1
             . P.analysisStandard
  in [
    (fast & P.name "fast"            . P.tracerOn  . P.newTracing          )
  , (fast & P.name "fast-plutus"     . P.tracerOn  . P.newTracing          )
  , (fast & P.name "fast-p2p"        . P.tracerOn  . P.newTracing . P.p2pOn)
  , (fast & P.name "fast-oldtracing" . P.tracerOn  . P.oldTracing          )
  , (fast & P.name "fast-notracer"   . P.tracerOff . P.newTracing          )
  ]
  ++
  ------------------------------------------------------------------------------
  -- ci-test: 2 nodes, FixedLoaded and "--shutdown-on-block-synced 3"
  ------------------------------------------------------------------------------
  let ciTest =   dummy
               & P.hosts 2
               . P.fixedLoaded . P.generatorTps 15
               . P.shutdownOnBlock 3
               . P.analysisStandard
      -- TODO: Why are not both using UniCircle ????
      ciTestLocal     = ciTest & P.uniCircle . P.loopback
      ciTestNomadPerf = ciTest & P.torus     . P.nomadPerf . P.withExplorerNode
  in [
  -- Local
    (ciTestLocal     & P.name "ci-test"                      . P.tracerOn  . P.newTracing . P.p2pOff                 )
  , (ciTestLocal     & P.name "ci-test-plutus"               . P.tracerOn  . P.newTracing . P.p2pOff                 )
  , (ciTestLocal     & P.name "ci-test-rtview"               . P.tracerOn  . P.newTracing . P.p2pOff . P.tracerRtview)
  , (ciTestLocal     & P.name "ci-test-p2p"                  . P.tracerOn  . P.newTracing . P.p2pOn                  )
  , (ciTestLocal     & P.name "ci-test-notracer"             . P.tracerOff . P.newTracing . P.p2pOff                 )
  -- Nomad perf
  , (ciTestNomadPerf & P.name "ci-test-nomadperf"            . P.tracerOn  . P.newTracing . P.p2pOn                  )
  , (ciTestNomadPerf & P.name "ci-test-nomadperf-nop2p"      . P.tracerOn  . P.newTracing . P.p2pOff                 )
  -- TODO: FIXME: A non "nop2p" "nomadperf" profile without P2P???
  , (ciTestNomadPerf & P.name "ci-test-oldtracing-nomadperf" . P.tracerOn  . P.oldTracing . P.p2pOff                 )
  ]
  ++
  ------------------------------------------------------------------------------
  -- ci-test-dense: 10 pools, FixedLoaded and "--shutdown-on-block-synced 3"
  ------------------------------------------------------------------------------
  let ciTestDense =   dummy
                    & P.uniCircle . P.pools 10
                    . P.loopback
                    . P.p2pOff
                    . P.fixedLoaded . P.generatorTps 15
                    . P.shutdownOnBlock 3
                    . P.tracerOn  . P.newTracing
                    . P.analysisStandard
  in [
    (ciTestDense & P.name "ci-test-dense10")
  ]
  ++
  ------------------------------------------------------------------------------
  -- epoch transition: 2 nodes, FixedLoaded and "--shutdown-on-slot-synced 900"
  ------------------------------------------------------------------------------
  let epochTransition =   dummy
                        & P.uniCircle . P.hosts 2
                        . P.loopback
                        . P.fixedLoaded . P.generatorTps 15
                        . P.shutdownOnSlot 900
                        . P.tracerOn . P.newTracing
                        . P.analysisStandard
  in [
    (epochTransition & P.name "epoch-transition")
  ]
  ++
  ------------------------------------------------------------------------------
  -- ci-bench: 2|5|10 nodes, FixedLoaded and "--shutdown-on-block-synced 15"
  ------------------------------------------------------------------------------
  let ciBench =  dummy
               & P.fixedLoaded . P.generatorTps 15
               . P.shutdownOnBlock 15
               . P.analysisStandard
      ciBench02  = ciBench & P.hosts  2
      ciBench06  = ciBench & P.hosts  6
      ciBench10  = ciBench & P.hosts 10
      -- TODO: Why are not all using UniCircle ????
      ciBench02Local     = ciBench02 & P.uniCircle . P.loopback
      ciBench02NomadPerf = ciBench02 & P.torus     . P.nomadPerf . P.withExplorerNode
      ciBench06Trace     = ciBench06 & P.torus     . P.loopback  . P.tracerWithresources
      ciBench10Local     = ciBench10 & P.uniCircle . P.loopback
  in [
  -- 2 nodes, local
    (ciBench02Local     & P.name "ci-bench"                      . P.tracerOn  . P.newTracing . P.p2pOff                 )
  , (ciBench02Local     & P.name "ci-bench-plutus"               . P.tracerOn  . P.newTracing . P.p2pOff                 )
  , (ciBench02Local     & P.name "ci-bench-plutus-secp-ecdsa"    . P.tracerOn  . P.newTracing . P.p2pOff                 )
  , (ciBench02Local     & P.name "ci-bench-plutus-secp-schnorr"  . P.tracerOn  . P.newTracing . P.p2pOff                 )
  , (ciBench02Local     & P.name "ci-bench-rtview"               . P.tracerOn  . P.newTracing . P.p2pOff . P.tracerRtview)
  , (ciBench02Local     & P.name "ci-bench-p2p"                  . P.tracerOn  . P.newTracing . P.p2pOn                  )
  , (ciBench02Local     & P.name "ci-bench-notracer"             . P.tracerOff . P.newTracing . P.p2pOff                 )
  -- 2 nodes, Nomad perf
  , (ciBench02NomadPerf & P.name "ci-bench-nomadperf"            . P.tracerOn  . P.newTracing . P.p2pOn                  )
  , (ciBench02NomadPerf & P.name "ci-bench-nomadperf-nop2p"      . P.tracerOn  . P.newTracing . P.p2pOff                 )
  -- TODO: FIXME: A non "nop2p" "nomadperf" profile without P2P???
  , (ciBench02NomadPerf & P.name "ci-bench-oldtracing-nomadperf" . P.tracerOn  . P.oldTracing . P.p2pOff                 )
  -- 6 nodes, local
  , (ciBench06Trace     & P.name "trace-bench"                   . P.tracerOn  . P.newTracing . P.p2pOff                 )
  , (ciBench06Trace     & P.name "trace-bench-rtview"            . P.tracerOn  . P.newTracing . P.p2pOff . P.tracerRtview)
  , (ciBench06Trace     & P.name "trace-bench-oldtracing"        . P.tracerOn  . P.oldTracing . P.p2pOff                 )
  , (ciBench06Trace     & P.name "trace-bench-notracer"          . P.tracerOff . P.newTracing . P.p2pOff                 )
  -- 10 nodes, local
  , (ciBench10Local     & P.name "10"                            . P.tracerOn  . P.newTracing . P.p2pOff                 )
  , (ciBench10Local     & P.name "10-plutus"                     . P.tracerOn  . P.newTracing . P.p2pOff                 )
  , (ciBench10Local     & P.name "10-p2p"                        . P.tracerOn  . P.newTracing . P.p2pOn                  )
  , (ciBench10Local     & P.name "10-notracer"                   . P.tracerOff . P.newTracing . P.p2pOff                 )
  ]
  ++
  ------------------------------------------------------------------------------
  -- forge-stress
  ------------------------------------------------------------------------------
  let forgeStress =   dummy
                    & P.uniCircle
                    . P.loopback
                    . P.p2pOff
                    . P.fixedLoaded . P.generatorTps 15
                    . P.newTracing
                    . P.analysisStandard
      -- "--shutdown-on-slot-synced 2400"
      forgeStressSolo  = forgeStress & P.shutdownOnSlot 2400 . P.hosts 1
      forgeStressSmall = forgeStress & P.shutdownOnSlot 2400 . P.hosts 3
      -- "--shutdown-on-slot-synced 4800"
      forgeStressLarge = forgeStress & P.shutdownOnSlot 4800 . P.hosts 6
  in [
  -- 1 node versions.
    (forgeStressSolo  & P.name "forge-stress-solo"          . P.tracerOn                                        )
  , (forgeStressSolo  & P.name "forge-stress-pre-solo"      . P.tracerOn                                        )
  , (forgeStressSolo  & P.name "forge-stress-plutus-solo"   . P.tracerOn                                        )
  -- 3 nodes versions.
  , (forgeStressSmall & P.name "forge-stress"               . P.tracerOn                                        )
  , (forgeStressSmall & P.name "forge-stress-light"         . P.tracerOn                                        )
  , (forgeStressSmall & P.name "forge-stress-notracer"      . P.tracerOff                                       )
  -- TODO: FIXME: "forge-stress-p2p" has no P2P enabled!
  , (forgeStressSmall & P.name "forge-stress-p2p"           . P.tracerOn                                        )
  , (forgeStressSmall & P.name "forge-stress-plutus"        . P.tracerOn                                        )
  , (forgeStressSmall & P.name "forge-stress-pre"           . P.tracerOn                                        )
  , (forgeStressSmall & P.name "forge-stress-pre-plutus"    . P.tracerOn                                        )
  , (forgeStressSmall & P.name "forge-stress-pre-rtsA4m"    . P.tracerOn                   . P.rtsGcAllocSize  4)
  , (forgeStressSmall & P.name "forge-stress-pre-rtsA64m"   . P.tracerOn                   . P.rtsGcAllocSize 64)
  , (forgeStressSmall & P.name "forge-stress-pre-rtsN3"     . P.tracerOn  . P.rtsThreads 3                      )
  , (forgeStressSmall & P.name "forge-stress-pre-rtsA4mN3"  . P.tracerOn  . P.rtsThreads 3 . P.rtsGcAllocSize  4)
  , (forgeStressSmall & P.name "forge-stress-pre-rtsA64mN3" . P.tracerOn  . P.rtsThreads 3 . P.rtsGcAllocSize 64)
  , (forgeStressSmall & P.name "forge-stress-pre-rtsxn"     . P.tracerOn                   . P.rtsGcNonMoving   )
  , (forgeStressSmall & P.name "forge-stress-pre-notracer"  . P.tracerOff                                       )
  -- Double nodes and time running version.
  , (forgeStressLarge & P.name "forge-stress-large"         . P.tracerOn                                        )
  ]
  ++
  ------------------------------------------------------------------------------
  -- TODO: This is a special case of forge-stress. Mix both? Still used?
  ------------------------------------------------------------------------------
  let dish =   dummy
             & P.uniCircle . P.hosts 3
             . P.loopback
             . P.p2pOff
             . P.fixedLoaded . P.generatorTps 15
             . P.shutdownOnSlot 2400
             . P.tracerOn  . P.newTracing
             . P.analysisStandard
  in [
    (dish & P.name "dish"            )
  , (dish & P.name "dish-plutus"     )
  , (dish & P.name "dish-10M"        )
  , (dish & P.name "dish-10M-plutus" )
  ]
  ++
  ------------------------------------------------------------------------------
  -- k3: 3 nodes and no "--shutdown-on-slot-synced" and no "--shutdown-on-block-synced"
  ------------------------------------------------------------------------------
  let k3 =   dummy
           & P.uniCircle . P.hosts 3
           . P.loopback
           . P.p2pOff
           . P.fixedLoaded
           . P.shutdownOnOff
           . P.tracerOn  . P.newTracing
  in [
    (k3 & P.name "k3-3ep-18kTx-10000kU-1300kD-64kbs-10tps-fixed-loaded" )
  , (k3 & P.name "k3-3ep-22kTx-10000kU-1300kD-64kbs-fixed-loaded"       )
  , (k3 & P.name "k3-3ep-5kTx-10000kU-1300kD-64kbs-fixed-loaded"        )
  , (k3 & P.name "k3-3ep-9kTx-10000kU-1300kD-64kbs-5tps-fixed-loaded"   )
  ]
  ++
  ------------------------------------------------------------------------------
  -- 6 nodes and no "--shutdown-on-slot-synced" and no "--shutdown-on-block-synced"
  ------------------------------------------------------------------------------
  let idle =   dummy
             & P.uniCircle . P.hosts 6
             . P.loopback
             . P.p2pOff
             . P.idle
             . P.shutdownOnOff
  in [
    (idle & P.name "devops" . P.tracerOn  . P.newTracing . P.p2pOff)
  , (idle & P.name "idle"   . P.tracerOn  . P.newTracing . P.p2pOff)
  ]
  ++
  ------------------------------------------------------------------------------
  -- 6 nodes and no "--shutdown-on-slot-synced" and no "--shutdown-on-block-synced"
  ------------------------------------------------------------------------------
  let tracerOnly =   dummy
                   & P.uniCircle . P.hosts 6
                   . P.loopback
                   . P.p2pOff
                   . P.tracerOnly
                   . P.shutdownOnOff
  in [
    (tracerOnly & P.name "tracer-only" . P.tracerOn  . P.newTracing . P.p2pOff)
  ]
  ++
  ------------------------------------------------------------------------------
  -- 6 nodes and no "--shutdown-on-slot-synced" and no "--shutdown-on-block-synced"
  ------------------------------------------------------------------------------
  let noCliStop =   dummy
                  & P.hosts 6
                  . P.fixedLoaded
                  . P.shutdownOnOff
      -- TODO: Why are not all using Torus ????
      noCliStopLocal     = noCliStop & P.uniCircle . P.loopback
      noCliStopNomadPerf = noCliStop & P.torus     . P.nomadPerf . P.withExplorerNode
  in [
    (noCliStopLocal     & P.name "default"                    . P.tracerOn  . P.newTracing . P.p2pOff)
  , (noCliStopLocal     & P.name "default-p2p"                . P.tracerOn  . P.newTracing . P.p2pOn )
  , (noCliStopLocal     & P.name "oldtracing"                 . P.tracerOn  . P.oldTracing . P.p2pOff)
  , (noCliStopLocal     & P.name "plutus"                     . P.tracerOn  . P.newTracing . P.p2pOff)
  , (noCliStopLocal     & P.name "plutus-secp-ecdsa"          . P.tracerOn  . P.newTracing . P.p2pOff)
  , (noCliStopLocal     & P.name "plutus-secp-schnorr"        . P.tracerOn  . P.newTracing . P.p2pOff)
  , (noCliStopNomadPerf & P.name "default-nomadperf"          . P.tracerOn  . P.newTracing . P.p2pOn )
  , (noCliStopNomadPerf & P.name "default-nomadperf-nop2p"    . P.tracerOn  . P.newTracing . P.p2pOff)
  , (noCliStopNomadPerf & P.name "oldtracing-nomadperf"       . P.tracerOn  . P.oldTracing . P.p2pOn )
  , (noCliStopNomadPerf & P.name "oldtracing-nomadperf-nop2p" . P.tracerOn  . P.oldTracing . P.p2pOff)
  ]
  ++
  ------------------------------------------------------------------------------
  -- model: 4 nodes, FixedLoaded and "--shutdown-on-slot-synced 56000"
  ------------------------------------------------------------------------------
  let model =    dummy
              & P.uniCircle . P.hosts 4
              . P.loopback
              . P.fixedLoaded
              . P.shutdownOnSlot 56000
              . P.tracerOn . P.newTracing
              . P.analysisStandard
  in [
    (model & P.name "model-secp-ecdsa-double" )
  , (model & P.name "model-secp-ecdsa-half"   )
  , (model & P.name "model-secp-ecdsa-plain"  )
  , (model & P.name "model-value"             )
  , (model & P.name "model-value-test"        )
  ]
  ++
  ------------------------------------------------------------------------------
  -- plutuscall: 6 nodes, FixedLoaded and "--shutdown-on-slot-synced 9000"
  ------------------------------------------------------------------------------
  let plutusCall =   dummy
                   & P.uniCircle . P.hosts 6
                   . P.loopback
                   . P.p2pOff
                   . P.fixedLoaded
                   . P.shutdownOnSlot 9000
                   . P.tracerOn . P.newTracing
                   . P.analysisStandard
  in [
    (plutusCall & P.name "plutuscall-loop-double"         )
  , (plutusCall & P.name "plutuscall-loop-half"           )
  , (plutusCall & P.name "plutuscall-loop-plain"          )
  , (plutusCall & P.name "plutuscall-secp-ecdsa-double"   )
  , (plutusCall & P.name "plutuscall-secp-ecdsa-half"     )
  , (plutusCall & P.name "plutuscall-secp-ecdsa-plain"    )
  , (plutusCall & P.name "plutuscall-secp-schnorr-double" )
  , (plutusCall & P.name "plutuscall-secp-schnorr-half"   )
  , (plutusCall & P.name "plutuscall-secp-schnorr-plain"  )
  ]
  ++
  ------------------------------------------------------------------------------
  -- plutuscall: 6 nodes, FixedLoaded and "--shutdown-on-slot-synced 1200"
  ------------------------------------------------------------------------------
  let traceFull =   dummy
                  & P.torus . P.hosts 6
                  . P.loopback
                  . P.p2pOff
                  . P.fixedLoaded
                  . P.shutdownOnSlot 1200
                  . P.tracerOn . P.newTracing
                  . P.analysisStandard
  in [
    (traceFull & P.name "trace-full"        . P.tracerWithresources                 )
  , (traceFull & P.name "trace-full-rtview" . P.tracerWithresources . P.tracerRtview)
  ]
  ++
  ------------------------------------------------------------------------------
  -- cloud: (52 + 1) nodes, FixedLoaded and "--shutdown-on-slot-synced 56000"
  ------------------------------------------------------------------------------
  let cloud =   dummy
              & P.torusDense . P.hosts 52 . P.withExplorerNode
              . P.nomadPerf
              . P.fixedLoaded
              . P.tracerOn
              . P.analysisStandard
      value  = cloud & P.shutdownOnSlot 64000
      plutus = cloud & P.shutdownOnSlot 72000
  in [
  -- Value
    (value  & P.name "value-nomadperf"                   . P.newTracing . P.p2pOn )
  , (value  & P.name "value-nomadperf-nop2p"             . P.newTracing . P.p2pOff)
  , (value  & P.name "value-oldtracing-nomadperf"        . P.oldTracing . P.p2pOn )
  , (value  & P.name "value-oldtracing-nomadperf-nop2p"  . P.oldTracing . P.p2pOff)
  -- Plutus
  , (plutus & P.name "plutus-nomadperf"                  . P.newTracing . P.p2pOn )
  , (plutus & P.name "plutus-nomadperf-nop2p"            . P.newTracing . P.p2pOff)
  ]
  ++
  ------------------------------------------------------------------------------
  -- chainsync
  ------------------------------------------------------------------------------
  let chainsync =   dummy
                  & P.uniCircle . P.hostsChainsync 1 . P.withChaindbServer . P.withExplorerNode
                  . P.loopback
                  . P.chainsync
                  . P.analysisStandard
      byron  = chainsync & P.shutdownOnSlot   237599
      alonzo = chainsync & P.shutdownOnSlot 38901589
  in [
  -- Byron
    (byron & P.name "chainsync-early-byron"            . P.tracerOff . P.newTracing . P.p2pOff )
  , (byron & P.name "chainsync-early-byron-notracer"   . P.tracerOff . P.newTracing . P.p2pOff )
  , (byron & P.name "chainsync-early-byron-oldtracing" . P.tracerOff . P.oldTracing . P.p2pOff )
  -- Alonzo
  , (alonzo  & P.name "chainsync-early-alonzo"            . P.tracerOff . P.newTracing . P.p2pOff)
  , (alonzo  & P.name "chainsync-early-alonzo-notracer"   . P.tracerOff . P.newTracing . P.p2pOff)
  , (alonzo  & P.name "chainsync-early-alonzo-oldtracing" . P.tracerOff . P.oldTracing . P.p2pOff)
  , (alonzo  & P.name "chainsync-early-alonzo-p2p"        . P.tracerOff . P.newTracing . P.p2pOn )
  ]

--------------------------------------------------------------------------------

{-- jq compositions

    { composition:
      { n_singular_hosts:               1
      , n_dense_hosts:                  0
      }
    } as $solo
  |
    { composition:
      { n_singular_hosts:               0
      , n_dense_hosts:                  1
      , dense_pool_density:             10
      }
    } as $solo_dense10
  |
    { composition:
      { n_singular_hosts:               2
      , n_dense_hosts:                  0
      }
    } as $doublet
  |
    { composition:
      { n_singular_hosts:               3
      , n_dense_hosts:                  0
      }
    } as $triplet
  |
    { composition:
      { n_singular_hosts:               52
      , n_dense_hosts:                  0
      }
    } as $compose_fiftytwo
  |
    { composition:
      { n_singular_hosts:               0
      , n_dense_hosts:                  0
      , with_chaindb_server:            true
      , with_explorer:                  true
      }
    } as $chainsync_cluster
--}

--------------------------------------------------------------------------------

{--

  , generator:
    { add_tx_size:                    100
    , init_cooldown:                  5
    , inputs_per_tx:                  2
    , outputs_per_tx:                 2
    , tx_fee:                         1000000
    , epochs:                         3
    , tps:                            12
    , plutus:
      { type:   null
      , script: null
      }
    }

generatorDefault :: Types.Generator
generatorDefault = Types.Generator {
    Types.add_tx_size = 100
  , Types.init_cooldown = 5
  , Types.inputs_per_tx = 2
  , Types.outputs_per_tx = 2
  , Types.tx_fee = 1000000
  , Types.epochs = 3
  , Types.tps = 12
  , Types.plutus = Types.Plutus Nothing Nothing
  , Types.tx_count = mempty
}

--}
