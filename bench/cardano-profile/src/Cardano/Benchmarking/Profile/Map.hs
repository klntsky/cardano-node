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
  ------------------------------------------------------
  -- fast: FixedLoaded and"--shutdown-on-block-synced 1"
  ------------------------------------------------------
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
  ----------------------------------------------------------
  -- ci-test: FixedLoaded and "--shutdown-on-block-synced 3"
  ----------------------------------------------------------
  [
    (dummy { Types.name = "ci-test-dense10",                                      Types.composition = compSoloDense10     , Types.node = nodeTest                                      , Types.tracer = tracerDefault})
  ]
  ++
  let ciTest =   dummy
               & P.hosts 2
               . P.fixedLoaded . P.generatorTps 15
               . P.shutdownOnBlock 3
               . P.analysisStandard
      ciTestLocal     = ciTest & P.uniCircle . P.loopback
      ciTestNomadPerf = ciTest & P.torus     . P.nomadPerf . P.withExplorer
  in [
    (ciTestLocal     & P.name "ci-test"                      . P.tracerOn  . P.newTracing . P.p2pOff                 )
  , (ciTestLocal     & P.name "ci-test-p2p"                  . P.tracerOn  . P.newTracing . P.p2pOn                  )
  , (ciTestLocal     & P.name "ci-test-plutus"               . P.tracerOn  . P.newTracing . P.p2pOff                 )
  , (ciTestLocal     & P.name "ci-test-rtview"               . P.tracerOn  . P.newTracing . P.p2pOff . P.tracerRtview)
  , (ciTestLocal     & P.name "ci-test-notracer"             . P.tracerOff . P.newTracing . P.p2pOff                 )
  , (ciTestNomadPerf & P.name "ci-test-nomadperf"            . P.tracerOn  . P.newTracing . P.p2pOn                  )
  , (ciTestNomadPerf & P.name "ci-test-nomadperf-nop2p"      . P.tracerOn  . P.newTracing . P.p2pOff                 )
  , (ciTestNomadPerf & P.name "ci-test-oldtracing-nomadperf" . P.tracerOn  . P.oldTracing . P.p2pOff                 )
  ]
  ++
  ------------------------------------------------------------
  -- ci-bench: FixedLoaded and "--shutdown-on-block-synced 15"
  ------------------------------------------------------------
  let ciBench =  dummy
               & P.fixedLoaded . P.generatorTps 15
               . P.shutdownOnBlock 15
               . P.analysisStandard
      ciBench2  = ciBench & P.hosts  2
      ciBench6  = ciBench & P.hosts  6
      ciBench10 = ciBench & P.hosts 10
      ciBench2Local     = ciBench2  & P.uniCircle . P.loopback
      ciBench2NomadPerf = ciBench2  & P.torus     . P.nomadPerf . P.withExplorer
      ciBench6Trace     = ciBench6  & P.torus     . P.loopback  . P.tracerWithresources
      ciBench10Local    = ciBench10 & P.uniCircle . P.loopback
  in [
    (ciBench2Local     & P.name "ci-bench"                      . P.tracerOn  . P.newTracing . P.p2pOff                 )
  , (ciBench2Local     & P.name "ci-bench-notracer"             . P.tracerOff . P.newTracing . P.p2pOff                 )
  , (ciBench2Local     & P.name "ci-bench-p2p"                  . P.tracerOn  . P.newTracing . P.p2pOn                  )
  , (ciBench2Local     & P.name "ci-bench-plutus"               . P.tracerOn  . P.newTracing . P.p2pOff                 )
  , (ciBench2Local     & P.name "ci-bench-plutus-secp-ecdsa"    . P.tracerOn  . P.newTracing . P.p2pOff                 )
  , (ciBench2Local     & P.name "ci-bench-plutus-secp-schnorr"  . P.tracerOn  . P.newTracing . P.p2pOff                 )
  , (ciBench2Local     & P.name "ci-bench-rtview"               . P.tracerOn  . P.newTracing . P.p2pOff . P.tracerRtview)
  , (ciBench2NomadPerf & P.name "ci-bench-nomadperf"            . P.tracerOn  . P.newTracing . P.p2pOn                  )
  , (ciBench2NomadPerf & P.name "ci-bench-nomadperf-nop2p"      . P.tracerOn  . P.newTracing . P.p2pOff                 )
  , (ciBench2NomadPerf & P.name "ci-bench-oldtracing-nomadperf" . P.tracerOn  . P.oldTracing . P.p2pOff                 )
  , (ciBench6Trace     & P.name "trace-bench"                   . P.tracerOn  . P.newTracing . P.p2pOff                 )
  , (ciBench6Trace     & P.name "trace-bench-oldtracing"        . P.tracerOn  . P.oldTracing . P.p2pOff                 )
  , (ciBench6Trace     & P.name "trace-bench-rtview"            . P.tracerOn  . P.newTracing . P.p2pOff . P.tracerRtview)
  , (ciBench6Trace     & P.name "trace-bench-notracer"          . P.tracerOff . P.newTracing . P.p2pOff                 )
  , (ciBench10Local    & P.name "10"                            . P.tracerOn  . P.newTracing . P.p2pOff                 )
  , (ciBench10Local    & P.name "10-plutus"                     . P.tracerOn  . P.newTracing . P.p2pOff                 )
  , (ciBench10Local    & P.name "10-p2p"                        . P.tracerOn  . P.newTracing . P.p2pOn                  )
  , (ciBench10Local    & P.name "10-notracer"                   . P.tracerOff . P.newTracing . P.p2pOff                 )
  ]
  ++
  [
    (dummy { Types.name = "chainsync-early-alonzo",                               Types.composition = compSoloChainsync   , Types.node = nodeNoTracer nodeChainSyncAlo                 , Types.tracer = tracerDefault})
  , (dummy { Types.name = "chainsync-early-alonzo-notracer",                      Types.composition = compSoloChainsync   , Types.node = nodeNoTracer nodeChainSyncAlo                 , Types.tracer = tracerDefault})
  , (dummy { Types.name = "chainsync-early-alonzo-oldtracing",                    Types.composition = compSoloChainsync   , Types.node = nodeOldTracing $ nodeNoTracer nodeChainSyncAlo, Types.tracer = tracerDefault})
  ]
  ++
  [
    (dummy { Types.name = "chainsync-early-alonzo-p2p",                           Types.composition = compSoloChainsync   , Types.node = nodeNoTracer $ nodeP2P nodeChainSyncAlo       , Types.tracer = tracerDefault})
  , (dummy { Types.name = "chainsync-early-byron",                                Types.composition = compSoloChainsync   , Types.node = nodeNoTracer nodeChainSyncByr                 , Types.tracer = tracerDefault})
  , (dummy { Types.name = "chainsync-early-byron-notracer",                       Types.composition = compSoloChainsync   , Types.node = nodeNoTracer nodeChainSyncByr                 , Types.tracer = tracerDefault})
  , (dummy { Types.name = "chainsync-early-byron-oldtracing",                     Types.composition = compSoloChainsync   , Types.node = nodeOldTracing $ nodeNoTracer nodeChainSyncByr, Types.tracer = tracerDefault})
  ]
  ++
  -- No "--shutdown-on-slot-synced" and no "--shutdown-on-block-synced"
  [
    (dummy { Types.name = "k3-3ep-18kTx-10000kU-1300kD-64kbs-10tps-fixed-loaded", Types.composition = compTriplet         , Types.node = nodeNoStop                                    , Types.tracer = tracerDefault})
  , (dummy { Types.name = "k3-3ep-22kTx-10000kU-1300kD-64kbs-fixed-loaded",       Types.composition = compTriplet         , Types.node = nodeNoStop                                    , Types.tracer = tracerDefault})
  , (dummy { Types.name = "k3-3ep-5kTx-10000kU-1300kD-64kbs-fixed-loaded",        Types.composition = compTriplet         , Types.node = nodeNoStop                                    , Types.tracer = tracerDefault})
  , (dummy { Types.name = "k3-3ep-9kTx-10000kU-1300kD-64kbs-5tps-fixed-loaded",   Types.composition = compTriplet         , Types.node = nodeNoStop                                    , Types.tracer = tracerDefault})
  , (dummy { Types.name = "default",                                              Types.composition = compHexagon         , Types.node = nodeNoStop                                    , Types.tracer = tracerDefault})
  , (dummy { Types.name = "default-p2p",                                          Types.composition = compHexagon         , Types.node = nodeP2P nodeNoStop                            , Types.tracer = tracerDefault})
  , (dummy { Types.name = "devops",                                               Types.composition = compHexagon         , Types.node = nodeNoStop                                    , Types.tracer = tracerDefault})
  , (dummy { Types.name = "idle",                                                 Types.composition = compHexagon         , Types.node = nodeNoStop                                    , Types.tracer = tracerDefault})
  , (dummy { Types.name = "oldtracing",                                           Types.composition = compHexagon         , Types.node = nodeOldTracing nodeNoStop                     , Types.tracer = tracerDefault})
  , (dummy { Types.name = "plutus",                                               Types.composition = compHexagon         , Types.node = nodeNoStop                                    , Types.tracer = tracerDefault})
  , (dummy { Types.name = "plutus-secp-ecdsa",                                    Types.composition = compHexagon         , Types.node = nodeNoStop                                    , Types.tracer = tracerDefault})
  , (dummy { Types.name = "plutus-secp-schnorr",                                  Types.composition = compHexagon         , Types.node = nodeNoStop                                    , Types.tracer = tracerDefault})
  , (dummy { Types.name = "tracer-only",                                          Types.composition = compHexagon         , Types.node = nodeNoStop                                    , Types.tracer = tracerDefault})
  , (dummy { Types.name = "default-nomadperf",                                    Types.composition = compHexagonNomadPerf, Types.node = nodeP2P nodeNoStop                            , Types.tracer = tracerDefault})
  , (dummy { Types.name = "default-nomadperf-nop2p",                              Types.composition = compHexagonNomadPerf, Types.node = nodeNoStop                                    , Types.tracer = tracerDefault})
  , (dummy { Types.name = "oldtracing-nomadperf",                                 Types.composition = compHexagonNomadPerf, Types.node = nodeOldTracing $ nodeP2P nodeNoStop           , Types.tracer = tracerDefault})
  , (dummy { Types.name = "oldtracing-nomadperf-nop2p",                           Types.composition = compHexagonNomadPerf, Types.node = nodeOldTracing nodeNoStop                     , Types.tracer = tracerDefault})
  ]
  ++
  -- nodeForgeStress, "--shutdown-on-slot-synced 2400"
  let forgeStress =   dummy
                    & P.uniCircle
                    . P.loopback
                    . P.fixedLoaded . P.generatorTps 15
                    . P.shutdownOnSlot 2400
                    . P.analysisStandard
  in [
    (forgeStress & P.name "forge-stress-solo"          . P.hosts 1 . P.tracerOn  . P.newTracing . P.p2pOff                                       )
  , (forgeStress & P.name "forge-stress-pre-solo"      . P.hosts 1 . P.tracerOn  . P.newTracing . P.p2pOff                                       )
  , (forgeStress & P.name "forge-stress-plutus-solo"   . P.hosts 1 . P.tracerOn  . P.newTracing . P.p2pOff                                       )
  , (forgeStress & P.name "forge-stress"               . P.hosts 3 . P.tracerOn  . P.newTracing . P.p2pOff                                       )
  , (forgeStress & P.name "forge-stress-light"         . P.hosts 3 . P.tracerOn  . P.newTracing . P.p2pOff                                       )
  , (forgeStress & P.name "forge-stress-notracer"      . P.hosts 3 . P.tracerOff . P.newTracing . P.p2pOff                                       )
  -- TODO: FIXME: "forge-stress-p2p" has no P2P enabled!
  , (forgeStress & P.name "forge-stress-p2p"           . P.hosts 3 . P.tracerOn  . P.newTracing . P.p2pOff                                       )
  , (forgeStress & P.name "forge-stress-plutus"        . P.hosts 3 . P.tracerOn  . P.newTracing . P.p2pOff                                       )
  , (forgeStress & P.name "forge-stress-pre"           . P.hosts 3 . P.tracerOn  . P.newTracing . P.p2pOff                                       )
  , (forgeStress & P.name "forge-stress-pre-notracer"  . P.hosts 3 . P.tracerOff . P.newTracing . P.p2pOff                                       )
  , (forgeStress & P.name "forge-stress-pre-plutus"    . P.hosts 3 . P.tracerOn  . P.newTracing . P.p2pOff                                       )
  , (forgeStress & P.name "forge-stress-pre-rtsA4m"    . P.hosts 3 . P.tracerOn  . P.newTracing . P.p2pOff                  . P.rtsGcAllocSize  4)
  , (forgeStress & P.name "forge-stress-pre-rtsA4mN3"  . P.hosts 3 . P.tracerOn  . P.newTracing . P.p2pOff . P.rtsThreads 3 . P.rtsGcAllocSize  4)
  , (forgeStress & P.name "forge-stress-pre-rtsA64m"   . P.hosts 3 . P.tracerOn  . P.newTracing . P.p2pOff                  . P.rtsGcAllocSize 64)
  , (forgeStress & P.name "forge-stress-pre-rtsA64mN3" . P.hosts 3 . P.tracerOn  . P.newTracing . P.p2pOff . P.rtsThreads 3 . P.rtsGcAllocSize 64)
  , (forgeStress & P.name "forge-stress-pre-rtsN3"     . P.hosts 3 . P.tracerOn  . P.newTracing . P.p2pOff . P.rtsThreads 3                      )
  , (forgeStress & P.name "forge-stress-pre-rtsxn"     . P.hosts 3 . P.tracerOn  . P.newTracing . P.p2pOff                  . P.rtsGcNonMoving   )
  ]
  ++
  -- nodeForgeStress2x, "--shutdown-on-slot-synced 4800"
  [
    (dummy { Types.name = "forge-stress-large",                                   Types.composition = compHexagon         , Types.node = nodeForgeStress2x                             , Types.tracer = tracerDefault})
  ]
  ++
  [
    (dummy { Types.name = "dish-10M",                                             Types.composition = compTriplet         , Types.node = nodeForgeStress                               , Types.tracer = tracerDefault})
  , (dummy { Types.name = "dish-10M-plutus",                                      Types.composition = compTriplet         , Types.node = nodeForgeStress                               , Types.tracer = tracerDefault})
  , (dummy { Types.name = "dish",                                                 Types.composition = compTriplet         , Types.node = nodeForgeStress                               , Types.tracer = tracerDefault})
  , (dummy { Types.name = "dish-plutus",                                          Types.composition = compTriplet         , Types.node = nodeForgeStress                               , Types.tracer = tracerDefault})
  ]
  ++
  [
    (dummy { Types.name = "epoch-transition",                                     Types.composition = compDoubletLoopback , Types.node = nodeEpochTransition                           , Types.tracer = tracerDefault})
  ]
  ++
  [
    (dummy { Types.name = "model-secp-ecdsa-double",                              Types.composition = compQuadruplet      , Types.node = nodeModel                                     , Types.tracer = tracerDefault})
  , (dummy { Types.name = "model-secp-ecdsa-half",                                Types.composition = compQuadruplet      , Types.node = nodeModel                                     , Types.tracer = tracerDefault})
  , (dummy { Types.name = "model-secp-ecdsa-plain",                               Types.composition = compQuadruplet      , Types.node = nodeModel                                     , Types.tracer = tracerDefault})
  , (dummy { Types.name = "model-value",                                          Types.composition = compQuadruplet      , Types.node = nodeModel                                     , Types.tracer = tracerDefault})
  , (dummy { Types.name = "model-value-test",                                     Types.composition = compQuadruplet      , Types.node = nodeModel                                     , Types.tracer = tracerDefault})
  ]
  ++
  [
    (dummy { Types.name = "plutuscall-loop-double",                               Types.composition = compHexagon         , Types.node = nodePlutusCall                                , Types.tracer = tracerDefault})
  , (dummy { Types.name = "plutuscall-loop-half",                                 Types.composition = compHexagon         , Types.node = nodePlutusCall                                , Types.tracer = tracerDefault})
  , (dummy { Types.name = "plutuscall-loop-plain",                                Types.composition = compHexagon         , Types.node = nodePlutusCall                                , Types.tracer = tracerDefault})
  , (dummy { Types.name = "plutuscall-secp-ecdsa-double",                         Types.composition = compHexagon         , Types.node = nodePlutusCall                                , Types.tracer = tracerDefault})
  , (dummy { Types.name = "plutuscall-secp-ecdsa-half",                           Types.composition = compHexagon         , Types.node = nodePlutusCall                                , Types.tracer = tracerDefault})
  , (dummy { Types.name = "plutuscall-secp-ecdsa-plain",                          Types.composition = compHexagon         , Types.node = nodePlutusCall                                , Types.tracer = tracerDefault})
  , (dummy { Types.name = "plutuscall-secp-schnorr-double",                       Types.composition = compHexagon         , Types.node = nodePlutusCall                                , Types.tracer = tracerDefault})
  , (dummy { Types.name = "plutuscall-secp-schnorr-half",                         Types.composition = compHexagon         , Types.node = nodePlutusCall                                , Types.tracer = tracerDefault})
  , (dummy { Types.name = "plutuscall-secp-schnorr-plain",                        Types.composition = compHexagon         , Types.node = nodePlutusCall                                , Types.tracer = tracerDefault})
  ]
  ++
  [
    (dummy { Types.name = "trace-full",                                           Types.composition = compHexagonTorus    , Types.node = nodeTraceFull                                 , Types.tracer = tracerWithresources})
  , (dummy { Types.name = "trace-full-rtview",                                    Types.composition = compHexagonTorus    , Types.node = nodeTraceFull                                 , Types.tracer = tracerRtviewWithresources})
  ]
  ++
  [
    (dummy { Types.name = "value-nomadperf",                                      Types.composition = compComposeFiftyTwo , Types.node = nodeP2P nodeValue                             , Types.tracer = tracerDefault})
  , (dummy { Types.name = "value-nomadperf-nop2p",                                Types.composition = compComposeFiftyTwo , Types.node = nodeValue                                     , Types.tracer = tracerDefault})
  , (dummy { Types.name = "value-oldtracing-nomadperf",                           Types.composition = compComposeFiftyTwo , Types.node = nodeOldTracing $ nodeP2P nodeValue            , Types.tracer = tracerDefault})
  , (dummy { Types.name = "value-oldtracing-nomadperf-nop2p",                     Types.composition = compComposeFiftyTwo , Types.node = nodeOldTracing nodeValue                      , Types.tracer = tracerDefault})
  ]
  ++
  [
    (dummy { Types.name = "plutus-nomadperf",                                     Types.composition = compComposeFiftyTwo , Types.node = nodeP2P nodePlutus                            , Types.tracer = tracerDefault})
  , (dummy { Types.name = "plutus-nomadperf-nop2p",                               Types.composition = compComposeFiftyTwo , Types.node = nodePlutus                                    , Types.tracer = tracerDefault})
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

{-- Used by:
wb profile all-profiles | jq 'map(select(.composition.n_singular_hosts == 0))' | jq 'map(select(.composition.n_dense_hosts == 0))' | jq 'map(.name) | sort'
[
  "chainsync-early-alonzo",
  "chainsync-early-alonzo-notracer",
  "chainsync-early-alonzo-oldtracing",
  "chainsync-early-alonzo-p2p",
  "chainsync-early-byron",
  "chainsync-early-byron-notracer",
  "chainsync-early-byron-oldtracing",
]
wb profile all-profiles | jq 'map(select(.composition.n_singular_hosts == 0))' | jq 'map(select(.composition.n_dense_hosts == 0))' | jq .[] | jq -c .composition | sort | uniq
{"locations":["loopback"],"n_bft_hosts":0,"n_singular_hosts":0,"n_dense_hosts":0,"dense_pool_density":1,"with_proxy":false,"with_explorer":true,"topology":"uni-circle","with_chaindb_server":true,"n_hosts":0,"n_pools":0,"n_singular_pools":0,"n_dense_pools":0,"n_pool_hosts":0
--}
compSoloChainsync :: Types.Composition
compSoloChainsync = Types.Composition {
    Types.locations = [Types.Loopback]
  , Types.n_bft_hosts = 0
  , Types.n_singular_hosts = 0
  , Types.n_dense_hosts = 0
  , Types.dense_pool_density = 1
  , Types.with_proxy = False
  , Types.with_explorer = True
  , Types.topology = Types.UniCircle
  , Types.with_chaindb_server = Just True
  , Types.n_hosts = 0
  , Types.n_pools = 0
  , Types.n_singular_pools = 0
  , Types.n_dense_pools = 0
  , Types.n_pool_hosts = 0
}

{-- Used by:
wb profile all-profiles | jq 'map(select(.composition.n_singular_hosts == 0))' | jq 'map(select(.composition.n_dense_hosts == 1))' | jq 'map(.name) | sort'
[
  "ci-test-dense10",
]
wb profile all-profiles | jq 'map(select(.composition.n_singular_hosts == 0))' | jq 'map(select(.composition.n_dense_hosts == 1))' | jq .[] | jq -c .composition | sort | uniq
{"locations":["loopback"],"n_bft_hosts":0,"n_singular_hosts":0,"n_dense_hosts":1,"dense_pool_density":10,"with_proxy":false,"with_explorer":false,"topology":"uni-circle","n_hosts":1,"n_pools":10,"n_singular_pools":0,"n_dense_pools":10,"n_pool_hosts":1}
--}
compSoloDense10 :: Types.Composition
compSoloDense10 = Types.Composition {
    Types.locations = [Types.Loopback]
  , Types.n_bft_hosts = 0
  , Types.n_singular_hosts = 0
  , Types.n_dense_hosts = 1
  , Types.dense_pool_density = 10
  , Types.with_proxy = False
  , Types.with_explorer = False
  , Types.topology = Types.UniCircle
  , Types.with_chaindb_server = Nothing
  , Types.n_hosts = 1
  , Types.n_pools = 10
  , Types.n_singular_pools = 0
  , Types.n_dense_pools = 10
  , Types.n_pool_hosts = 1
}

{-- Used by:
wb profile all-profiles | jq 'map(select(.composition.n_singular_hosts == 2))' | jq 'map(select(.composition.locations == ["loopback"]))' | jq 'map(.name) | sort'
[
  "ci-bench",
  "ci-bench-notracer",
  "ci-bench-p2p",
  "ci-bench-plutus",
  "ci-bench-plutus-secp-ecdsa",
  "ci-bench-plutus-secp-schnorr",
  "ci-test",
  "ci-test-notracer",
  "ci-test-p2p",
  "ci-test-plutus",
  "ci-test-rtview",
  "epoch-transition",
  "fast",
  "fast-notracer",
  "fast-oldtracing",
  "fast-p2p",
  "fast-plutus",
]
wb profile all-profiles | jq 'map(select(.composition.n_singular_hosts == 2))' | jq 'map(select(.composition.locations == ["loopback"]))' | jq .[] | jq -c .composition | sort | uniq
{"locations":["loopback"],"n_bft_hosts":0,"n_singular_hosts":2,"n_dense_hosts":0,"dense_pool_density":1,"with_proxy":false,"with_explorer":false,"topology":"uni-circle","n_hosts":2,"n_pools":2,"n_singular_pools":2,"n_dense_pools":0,"n_pool_hosts":2}
--}
compDoubletLoopback :: Types.Composition
compDoubletLoopback = Types.Composition {
    Types.locations = [Types.Loopback]
  , Types.n_bft_hosts = 0
  , Types.n_singular_hosts = 2
  , Types.n_dense_hosts = 0
  , Types.dense_pool_density = 1
  , Types.with_proxy = False
  , Types.with_explorer = False
  , Types.topology = Types.UniCircle
  , Types.with_chaindb_server = Nothing
  , Types.n_hosts = 2
  , Types.n_pools = 2
  , Types.n_singular_pools = 2
  , Types.n_dense_pools = 0
  , Types.n_pool_hosts = 2
}

{-- Used by:
wb profile all-profiles | jq 'map(select(.composition.n_singular_hosts == 3))' | jq 'map(.name) | sort'
[
  "dish-10M",
  "dish-10M-plutus",
  "dish",
  "dish-plutus",
  "forge-stress",,
  "forge-stress-light",
  "forge-stress-notracer",
  "forge-stress-p2p",
  "forge-stress-plutus",
  "forge-stress-pre",
  "forge-stress-pre-notracer",
  "forge-stress-pre-plutus",
  "forge-stress-pre-rtsA4m",
  "forge-stress-pre-rtsA4mN3",
  "forge-stress-pre-rtsA64m",
  "forge-stress-pre-rtsA64mN3",
  "forge-stress-pre-rtsN3",
  "forge-stress-pre-rtsxn",
  "k3-3ep-18kTx-10000kU-1300kD-64kbs-10tps-fixed-loaded",
  "k3-3ep-22kTx-10000kU-1300kD-64kbs-fixed-loaded",
  "k3-3ep-5kTx-10000kU-1300kD-64kbs-fixed-loaded",
  "k3-3ep-9kTx-10000kU-1300kD-64kbs-5tps-fixed-loaded",
]
wb profile all-profiles | jq 'map(select(.composition.n_singular_hosts == 3))' | jq .[] | jq -c .composition | sort | uniq
{"locations":["loopback"],"n_bft_hosts":0,"n_singular_hosts":3,"n_dense_hosts":0,"dense_pool_density":1,"with_proxy":false,"with_explorer":false,"topology":"uni-circle","n_hosts":3,"n_pools":3,"n_singular_pools":3,"n_dense_pools":0,"n_pool_hosts":3}
--}
compTriplet :: Types.Composition
compTriplet = Types.Composition {
    Types.locations = [Types.Loopback]
  , Types.n_bft_hosts = 0
  , Types.n_singular_hosts = 3
  , Types.n_dense_hosts = 0
  , Types.dense_pool_density = 1
  , Types.with_proxy = False
  , Types.with_explorer = False
  , Types.topology = Types.UniCircle
  , Types.with_chaindb_server = Nothing
  , Types.n_hosts = 3
  , Types.n_pools = 3
  , Types.n_singular_pools = 3
  , Types.n_dense_pools = 0
  , Types.n_pool_hosts = 3
}

{-- Used by:
wb profile all-profiles | jq 'map(select(.composition.n_singular_hosts == 4))' | jq 'map(.name) | sort'
[
  "model-secp-ecdsa-double",
  "model-secp-ecdsa-half",
  "model-secp-ecdsa-plain",
  "model-value",
  "model-value-test",
]
wb profile all-profiles | jq 'map(select(.composition.n_singular_hosts == 4))' | jq .[] | jq -c .composition | sort | uniq
{"locations":["loopback"],"n_bft_hosts":0,"n_singular_hosts":4,"n_dense_hosts":0,"dense_pool_density":1,"with_proxy":false,"with_explorer":false,"topology":"uni-circle","n_hosts":4,"n_pools":4,"n_singular_pools":4,"n_dense_pools":0,"n_pool_hosts":4}
--}
compQuadruplet :: Types.Composition
compQuadruplet = Types.Composition {
    Types.locations = [Types.Loopback]
  , Types.n_bft_hosts = 0
  , Types.n_singular_hosts = 4
  , Types.n_dense_hosts = 0
  , Types.dense_pool_density = 1
  , Types.with_proxy = False
  , Types.with_explorer = False
  , Types.topology = Types.UniCircle
  , Types.with_chaindb_server = Nothing
  , Types.n_hosts = 4
  , Types.n_pools = 4
  , Types.n_singular_pools = 4
  , Types.n_dense_pools = 0
  , Types.n_pool_hosts = 4
}

{-- Used by:
wb profile all-profiles | jq 'map(select(.composition.n_singular_hosts == 6))' | jq 'map(select(.composition.locations == ["loopback"]))' | jq 'map(select(.composition.topology == "uni-circle"))' | jq 'map(.name) | sort'
[
  "default",
  "default-p2p",
  "devops",
  "forge-stress-large",
  "idle",
  "oldtracing",
  "plutus",
  "plutus-secp-ecdsa",
  "plutus-secp-schnorr",
  "plutuscall-loop-double",
  "plutuscall-loop-half",
  "plutuscall-loop-plain",
  "plutuscall-secp-ecdsa-double",
  "plutuscall-secp-ecdsa-half",
  "plutuscall-secp-ecdsa-plain",
  "plutuscall-secp-schnorr-double",
  "plutuscall-secp-schnorr-half",
  "plutuscall-secp-schnorr-plain",
  "tracer-only",
]
wb profile all-profiles | jq 'map(select(.composition.n_singular_hosts == 6))' | jq 'map(select(.composition.locations == ["loopback"]))' | jq 'map(select(.composition.topology == "uni-circle"))' | jq .[] | jq -c .composition | sort | uniq
{"locations":["loopback"],"n_bft_hosts":0,"n_singular_hosts":6,"n_dense_hosts":0,"dense_pool_density":1,"with_proxy":false,"with_explorer":false,"topology":"uni-circle","n_hosts":6,"n_pools":6,"n_singular_pools":6,"n_dense_pools":0,"n_pool_hosts":6}
--}
compHexagon :: Types.Composition
compHexagon = Types.Composition {
    Types.locations = [Types.Loopback]
  , Types.n_bft_hosts = 0
  , Types.n_singular_hosts = 6
  , Types.n_dense_hosts = 0
  , Types.dense_pool_density = 1
  , Types.with_proxy = False
  , Types.with_explorer = False
  , Types.topology = Types.UniCircle
  , Types.with_chaindb_server = Nothing
  , Types.n_hosts = 6
  , Types.n_pools = 6
  , Types.n_singular_pools = 6
  , Types.n_dense_pools = 0
  , Types.n_pool_hosts = 6
}

{-- Used by:
wb profile all-profiles | jq 'map(select(.composition.n_singular_hosts == 6))' | jq 'map(select(.composition.locations == ["loopback"]))' | jq 'map(select(.composition.topology != "uni-circle"))' | jq 'map(.name) | sort'
[
  "trace-bench",
  "trace-bench-notracer",
  "trace-bench-oldtracing",
  "trace-bench-rtview",
  "trace-full",
  "trace-full-rtview",
]
wb profile all-profiles | jq 'map(select(.composition.n_singular_hosts == 6))' | jq 'map(select(.composition.locations == ["loopback"]))' | jq 'map(select(.composition.topology != "uni-circle"))' | jq .[] | jq -c .composition | sort | uniq
{"locations":["loopback"],"n_bft_hosts":0,"n_singular_hosts":6,"n_dense_hosts":0,"dense_pool_density":1,"with_proxy":false,"with_explorer":false,"topology":"torus","n_hosts":6,"n_pools":6,"n_singular_pools":6,"n_dense_pools":0,"n_pool_hosts":6}
--}
compHexagonTorus :: Types.Composition
compHexagonTorus = Types.Composition {
    Types.locations = [Types.Loopback]
  , Types.n_bft_hosts = 0
  , Types.n_singular_hosts = 6
  , Types.n_dense_hosts = 0
  , Types.dense_pool_density = 1
  , Types.with_proxy = False
  , Types.with_explorer = False
  , Types.topology = Types.Torus
  , Types.with_chaindb_server = Nothing
  , Types.n_hosts = 6
  , Types.n_pools = 6
  , Types.n_singular_pools = 6
  , Types.n_dense_pools = 0
  , Types.n_pool_hosts = 6
}

{-- Used by:
wb profile all-profiles | jq 'map(select(.composition.n_singular_hosts == 6))' | jq 'map(select(.composition.locations != ["loopback"]))' | jq 'map(.name) | sort'
[
  "default-nomadperf",
  "default-nomadperf-nop2p",
  "oldtracing-nomadperf",
  "oldtracing-nomadperf-nop2p",
]
wb profile all-profiles | jq 'map(select(.composition.n_singular_hosts == 6))' | jq 'map(select(.composition.locations != ["loopback"]))' | jq .[] | jq -c .composition | sort | uniq
{"locations":["eu-central-1","us-east-1","ap-southeast-2"],"n_bft_hosts":0,"n_singular_hosts":6,"n_dense_hosts":0,"dense_pool_density":1,"with_proxy":false,"with_explorer":true,"topology":"torus","n_hosts":6,"n_pools":6,"n_singular_pools":6,"n_dense_pools":0,"n_pool_hosts":6
--}
compHexagonNomadPerf :: Types.Composition
compHexagonNomadPerf = Types.Composition {
    Types.locations = [
      Types.AWS Types.EU_CENTRAL_1
    , Types.AWS Types.US_EAST_1
    , Types.AWS Types.AP_SOUTHEAST_2
    ]
  , Types.n_bft_hosts = 0
  , Types.n_singular_hosts = 6
  , Types.n_dense_hosts = 0
  , Types.dense_pool_density = 1
  , Types.with_proxy = False
  , Types.with_explorer = True
  , Types.topology = Types.Torus
  , Types.with_chaindb_server = Nothing
  , Types.n_hosts = 6
  , Types.n_pools = 6
  , Types.n_singular_pools = 6
  , Types.n_dense_pools = 0
  , Types.n_pool_hosts = 6
}

{-- Used by:
wb profile all-profiles | jq 'map(select(.composition.n_singular_hosts == 52))' | jq 'map(.name) | sort'
[
  "plutus-nomadperf",
  "plutus-nomadperf-nop2p",
  "value-nomadperf",
  "value-nomadperf-nop2p",
  "value-oldtracing-nomadperf",
  "value-oldtracing-nomadperf-nop2p",
]
wb profile all-profiles | jq 'map(select(.composition.n_singular_hosts == 52))' | jq .[] | jq -c .composition | sort | uniq
{"locations":["eu-central-1","us-east-1","ap-southeast-2"],"n_bft_hosts":0,"n_singular_hosts":52,"n_dense_hosts":0,"dense_pool_density":1,"with_proxy":false,"with_explorer":true,"topology":"torus-dense","n_hosts":52,"n_pools":52,"n_singular_pools":52,"n_dense_pools":0,"n_pool_hosts":52}
--}
compComposeFiftyTwo :: Types.Composition
compComposeFiftyTwo = Types.Composition {
    Types.locations = [
      Types.AWS Types.EU_CENTRAL_1
    , Types.AWS Types.US_EAST_1
    , Types.AWS Types.AP_SOUTHEAST_2
    ]
  , Types.n_bft_hosts = 0
  , Types.n_singular_hosts = 52
  , Types.n_dense_hosts = 0
  , Types.dense_pool_density = 1
  , Types.with_proxy = False
  , Types.with_explorer = True
  , Types.topology = Types.TorusDense
  , Types.with_chaindb_server = Nothing
  , Types.n_hosts = 52
  , Types.n_pools = 52
  , Types.n_singular_pools = 52
  , Types.n_dense_pools = 0
  , Types.n_pool_hosts = 52
}

--------------------------------------------------------------------------------

nodeDefault :: Maybe Int -> Maybe Int -> Types.Node
nodeDefault maybeSlotShutdown maybeBlockShutdown = Types.Node {
  Types.rts_flags_override = []
, Types.shutdown_on_slot_synced = maybeSlotShutdown
, Types.shutdown_on_block_synced = maybeBlockShutdown
, Types.tracing_backend = "trace-dispatcher"
, Types.nodeTracer = True
, Types.verbatim = Types.NodeVerbatim Nothing
}

nodeNoStop :: Types.Node
nodeNoStop = nodeDefault Nothing Nothing

-- Shutdown on slot 900
nodeEpochTransition :: Types.Node
nodeEpochTransition = nodeDefault (Just 900) Nothing

-- Shutdown on slot 1200
nodeTraceFull :: Types.Node
nodeTraceFull = nodeDefault (Just 1200) Nothing

-- Shutdown on slot 2400
nodeForgeStress :: Types.Node
nodeForgeStress = nodeDefault (Just 2400) Nothing

-- Shutdown on slot 4800
nodeForgeStress2x :: Types.Node
nodeForgeStress2x = nodeDefault (Just 4800) Nothing

-- Shutdown on slot 9000
nodePlutusCall :: Types.Node
nodePlutusCall = nodeDefault (Just 9000) Nothing

-- Shutdown on slot 56000
nodeModel :: Types.Node
nodeModel = nodeDefault (Just 56000) Nothing

-- Shutdown on slot 64000
nodeValue :: Types.Node
nodeValue = nodeDefault (Just 64000) Nothing

-- Shutdown on slot 72000
nodePlutus :: Types.Node
nodePlutus = nodeDefault (Just 72000) Nothing

-- Shutdown on slot 237599
nodeChainSyncByr :: Types.Node
nodeChainSyncByr = nodeDefault (Just 237599) Nothing

-- Shutdown on slot 38901589
nodeChainSyncAlo :: Types.Node
nodeChainSyncAlo = nodeDefault (Just 38901589) Nothing

-- Shutdown on block 3
nodeTest :: Types.Node
nodeTest = nodeDefault Nothing (Just 3)

{-- Use by:
wb profile all-profiles | jq 'map(select( .node.tracer == false ))' | jq .[] | jq -r '.name' | grep "\-coay" | sort
10-notracer-coay
chainsync-early-alonzo-coay
chainsync-early-alonzo-notracer-coay
chainsync-early-alonzo-oldtracing-coay
chainsync-early-alonzo-p2p-coay
chainsync-early-byron-coay
chainsync-early-byron-notracer-coay
chainsync-early-byron-oldtracing-coay
ci-bench-notracer-coay
ci-test-notracer-coay
fast-notracer-coay
forge-stress-notracer-coay
forge-stress-pre-notracer-coay
trace-bench-notracer-coay

TODO: chainsync-early-alonzo chainsync-early-alonzo
      chainsync-early-alonzo-oldtracing chainsync-early-alonzo-p2p
      chainsync-early-byron chainsync-early-byron-oldtracing
      Have either "oldtracing" or don't have "notracer" but still have the
      tracer off!
--}
nodeNoTracer :: Types.Node -> Types.Node
nodeNoTracer node = node {Types.nodeTracer = False}

{-- Used by:
$ wb profile all-profiles | jq 'map(select( .node.tracing_backend == "iohk-monitoring" ))' | jq .[] | jq -r '.name' | grep "\-coay" | sort
chainsync-early-alonzo-oldtracing-coay
chainsync-early-byron-oldtracing-coay
ci-bench-oldtracing-nomadperf-coay
ci-test-oldtracing-nomadperf-coay
fast-oldtracing-coay
oldtracing-coay
oldtracing-nomadperf-coay
oldtracing-nomadperf-nop2p-coay
trace-bench-oldtracing-coay
value-oldtracing-nomadperf-coay
value-oldtracing-nomadperf-nop2p-coay

TODO: chainsync-early-alonzo-oldtracing-coay and
      chainsync-early-byron-oldtracing-coay have nodeNoTracer and nodeOldTracing
--}
nodeOldTracing :: Types.Node -> Types.Node
nodeOldTracing node = node {Types.tracing_backend = "iohk-monitoring"}

{-- Used by:
wb profile all-profiles | jq 'map(select( .node.verbatim.EnableP2P ))' | jq .[] | jq -r '.name' | grep "\-coay"
default-p2p-coay
default-nomadperf-coay
oldtracing-nomadperf-coay
fast-p2p-coay
ci-test-p2p-coay
ci-test-nomadperf-coay
ci-bench-p2p-coay
ci-bench-nomadperf-coay
value-nomadperf-coay
value-oldtracing-nomadperf-coay
plutus-nomadperf-coay
10-p2p-coay
chainsync-early-alonzo-p2p-coay

TODO: Should "ci-bench-oldtracing-nomadperf" and "ci-test-oldtracing-nomadperf"
      include "-nop2p" in their names ???
--}
nodeP2P :: Types.Node -> Types.Node
nodeP2P node = node {Types.verbatim = Types.NodeVerbatim (Just True)}

--------------------------------------------------------------------------------

{--
> wb profile all-profiles | jq .[] | jq -c '.tracer' | sort | uniq
{"rtview":false,"ekg":false,"withresources":false}
{"rtview":false,"ekg":false,"withresources":true}
{"rtview":true,"ekg":false,"withresources":false}
{"rtview":true,"ekg":false,"withresources":true}
--}

tracerDefault :: Types.Tracer
tracerDefault = Types.Tracer False False False

{-- Used by:
wb profile all-profiles | jq 'map(select( .tracer.rtview == false and .tracer.withresources == true ))' | jq 'map(.name)'
[
  "trace-bench",
  "trace-bench-oldtracing",
  "trace-bench-notracer",
  "trace-full",
]
--}
tracerWithresources :: Types.Tracer
tracerWithresources = Types.Tracer {
  Types.rtview = False
, Types.ekg = False
, Types.withresources= True
}

{-- Used by:
wb profile all-profiles | jq 'map(select( .tracer.rtview == true and .tracer.withresources == true ))' | jq 'map(.name)'
[
  "trace-bench-rtview",
  "trace-full-rtview",
]
--}
tracerRtviewWithresources :: Types.Tracer
tracerRtviewWithresources = Types.Tracer {
  Types.rtview = True
, Types.ekg = False
, Types.withresources= True
}

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
