{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

--------------------------------------------------------------------------------

module Cardano.Benchmarking.Profile.Map (
  profiles
, byName
) where

--------------------------------------------------------------------------------

import           Prelude
import qualified Data.Map.Strict as Map
import qualified Data.Scientific as Scientific

import qualified Cardano.Benchmarking.Profile.Types as Types

--------------------------------------------------------------------------------

byName :: String -> Types.Profile
byName name = (Map.!) profiles name

--------------------------------------------------------------------------------

profiles :: Map.Map String Types.Profile
profiles = foldMap
  (\profile -> Map.fromList $
{--
> wb profile all-profiles | jq .[] | jq -r .name | sort | uniq | grep ci-test-notracer
ci-test-notracer-alra
ci-test-notracer-alzo
ci-test-notracer-bage
ci-test-notracer-coay
ci-test-notracer-mary
ci-test-notracer-shey
--}
    let
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

{--
  (Types.Profile {
      Types.name = "ci-test-bage"
    , Types.desc = Just "Miniature dataset, CI-friendly duration, test scale"
    , Types.composition = compositionCiTest
    , Types.era = Types.Babbage
--    , Types.genesis = Nothing
    , Types.scenario = Types.FixedLoaded
    , Types.node = nodeCiTest
    , Types.tracer = tracerCiTest
    , Types.generator = generatorCiTest
    , Types.analysis = analysisCiTest
--    , Types.overlay = mempty
  })
--}

dummy :: Types.Profile
dummy = Types.Profile {
    Types.name = "ci-test-bage"
  , Types.desc = Just "Miniature dataset, CI-friendly duration, test scale"
  , Types.composition = compCiTest
  , Types.era = Types.Babbage
--  , Types.genesis = Nothing
  , Types.scenario = Types.FixedLoaded
  , Types.node = nodeCiTest
  , Types.tracer = tracerCiTest
  , Types.generator = generatorCiTest
  , Types.analysis = analysisCiTest
--  , Types.overlay = mempty
}

-- TODO: forge-stress and forge-stress-light have the same .node.shutdown_on_slot_synced

profilesNoEra :: Map.Map String Types.Profile
-- Names:
-- wb profile all-profiles | jq .[] | jq -r .name | sort | uniq | grep "\-bage"
profilesNoEra = Map.fromList $ map (\p -> (Types.name p, p))
  [
    (dummy { Types.name = "10",                                                   Types.composition = compTenner          , Types.node = (nodeDefault Nothing (Just 15))                                    , Types.tracer = tracerDefault})
  , (dummy { Types.name = "10-notracer",                                          Types.composition = compTenner          , Types.node = nodeNoTracer (nodeDefault Nothing (Just 15))                       , Types.tracer = tracerDefault})
  , (dummy { Types.name = "10-p2p",                                               Types.composition = compTenner          , Types.node = nodeP2P (nodeDefault Nothing (Just 15))                            , Types.tracer = tracerDefault})
  , (dummy { Types.name = "10-plutus",                                            Types.composition = compTenner          , Types.node = (nodeDefault Nothing (Just 15))                                    , Types.tracer = tracerDefault})
  , (dummy { Types.name = "chainsync-early-alonzo",                               Types.composition = compSoloChainsync   , Types.node = nodeNoTracer (nodeDefault (Just 38901589) Nothing)                 , Types.tracer = tracerDefault})
  , (dummy { Types.name = "chainsync-early-alonzo-notracer",                      Types.composition = compSoloChainsync   , Types.node = nodeNoTracer (nodeDefault (Just 38901589) Nothing)                 , Types.tracer = tracerDefault})
  , (dummy { Types.name = "chainsync-early-alonzo-oldtracing",                    Types.composition = compSoloChainsync   , Types.node = nodeOldTracing $ nodeNoTracer (nodeDefault (Just 38901589) Nothing), Types.tracer = tracerDefault})
  , (dummy { Types.name = "chainsync-early-alonzo-p2p",                           Types.composition = compSoloChainsync   , Types.node = nodeNoTracer $ nodeP2P (nodeDefault (Just 38901589) Nothing)       , Types.tracer = tracerDefault})
  , (dummy { Types.name = "chainsync-early-byron",                                Types.composition = compSoloChainsync   , Types.node = nodeNoTracer (nodeDefault (Just 237599) Nothing)                   , Types.tracer = tracerDefault})
  , (dummy { Types.name = "chainsync-early-byron-notracer",                       Types.composition = compSoloChainsync   , Types.node = nodeNoTracer (nodeDefault (Just 237599) Nothing)                   , Types.tracer = tracerDefault})
  , (dummy { Types.name = "chainsync-early-byron-oldtracing",                     Types.composition = compSoloChainsync   , Types.node = nodeOldTracing $ nodeNoTracer (nodeDefault (Just 237599) Nothing)  , Types.tracer = tracerDefault})
  , (dummy { Types.name = "ci-bench",                                             Types.composition = compDoubletLoopback , Types.node = (nodeDefault Nothing (Just 15))                                    , Types.tracer = tracerDefault})
  , (dummy { Types.name = "ci-bench-nomadperf",                                   Types.composition = compDoubletNomadPerf, Types.node = nodeP2P (nodeDefault Nothing (Just 15))                            , Types.tracer = tracerDefault})
  , (dummy { Types.name = "ci-bench-nomadperf-nop2p",                             Types.composition = compDoubletNomadPerf, Types.node = (nodeDefault Nothing (Just 15))                                    , Types.tracer = tracerDefault})
  , (dummy { Types.name = "ci-bench-notracer",                                    Types.composition = compDoubletLoopback , Types.node = nodeNoTracer (nodeDefault Nothing (Just 15))                       , Types.tracer = tracerDefault})
  , (dummy { Types.name = "ci-bench-oldtracing-nomadperf",                        Types.composition = compDoubletNomadPerf, Types.node = nodeOldTracing (nodeDefault Nothing (Just 15))                     , Types.tracer = tracerDefault})
  , (dummy { Types.name = "ci-bench-p2p",                                         Types.composition = compDoubletLoopback , Types.node = nodeP2P (nodeDefault Nothing (Just 15))                            , Types.tracer = tracerDefault})
  , (dummy { Types.name = "ci-bench-plutus",                                      Types.composition = compDoubletLoopback , Types.node = (nodeDefault Nothing (Just 15))                                    , Types.tracer = tracerDefault})
  , (dummy { Types.name = "ci-bench-plutus-secp-ecdsa",                           Types.composition = compDoubletLoopback , Types.node = (nodeDefault Nothing (Just 15))                                    , Types.tracer = tracerDefault})
  , (dummy { Types.name = "ci-bench-plutus-secp-schnorr",                         Types.composition = compDoubletLoopback , Types.node = (nodeDefault Nothing (Just 15))                                    , Types.tracer = tracerDefault})
  , (dummy { Types.name = "ci-bench-rtview",                                      Types.composition = compDoubletLoopback , Types.node = (nodeDefault Nothing (Just 15))                                    , Types.tracer = tracerRtview})
  , (dummy { Types.name = "ci-test",                                              Types.composition = compDoubletLoopback , Types.node = (nodeDefault Nothing (Just 3))                                     , Types.tracer = tracerDefault})
  , (dummy { Types.name = "ci-test-dense10",                                      Types.composition = compSoloDense10     , Types.node = (nodeDefault Nothing (Just 3))                                     , Types.tracer = tracerDefault})
  , (dummy { Types.name = "ci-test-nomadperf",                                    Types.composition = compDoubletNomadPerf, Types.node = nodeP2P (nodeDefault Nothing (Just 3))                             , Types.tracer = tracerDefault})
  , (dummy { Types.name = "ci-test-nomadperf-nop2p",                              Types.composition = compDoubletNomadPerf, Types.node = (nodeDefault Nothing (Just 3))                                     , Types.tracer = tracerDefault})
  , (dummy { Types.name = "ci-test-notracer",                                     Types.composition = compDoubletLoopback , Types.node = nodeNoTracer (nodeDefault Nothing (Just 3))                        , Types.tracer = tracerDefault})
  , (dummy { Types.name = "ci-test-oldtracing-nomadperf",                         Types.composition = compDoubletNomadPerf, Types.node = nodeOldTracing (nodeDefault Nothing (Just 3))                      , Types.tracer = tracerDefault})
  , (dummy { Types.name = "ci-test-p2p",                                          Types.composition = compDoubletLoopback , Types.node = nodeP2P (nodeDefault Nothing (Just 3))                             , Types.tracer = tracerDefault})
  , (dummy { Types.name = "ci-test-plutus",                                       Types.composition = compDoubletLoopback , Types.node = (nodeDefault Nothing (Just 3))                                     , Types.tracer = tracerDefault})
  , (dummy { Types.name = "ci-test-rtview",                                       Types.composition = compDoubletLoopback , Types.node = (nodeDefault Nothing (Just 3))                                     , Types.tracer = tracerRtview})
  , (dummy { Types.name = "default",                                              Types.composition = compHexagon         , Types.node = (nodeDefault Nothing Nothing)                                      , Types.tracer = tracerDefault})
  , (dummy { Types.name = "default-nomadperf",                                    Types.composition = compHexagonNomadPerf, Types.node = nodeP2P (nodeDefault Nothing Nothing)                              , Types.tracer = tracerDefault})
  , (dummy { Types.name = "default-nomadperf-nop2p",                              Types.composition = compHexagonNomadPerf, Types.node = (nodeDefault Nothing Nothing)                                      , Types.tracer = tracerDefault})
  , (dummy { Types.name = "default-p2p",                                          Types.composition = compHexagon         , Types.node = nodeP2P (nodeDefault Nothing Nothing)                              , Types.tracer = tracerDefault})
  , (dummy { Types.name = "devops",                                               Types.composition = compHexagon         , Types.node = (nodeDefault Nothing Nothing)                                      , Types.tracer = tracerDefault})
  , (dummy { Types.name = "dish-10M",                                             Types.composition = compTriplet         , Types.node = (nodeDefault (Just 2400) Nothing)                                  , Types.tracer = tracerDefault})
  , (dummy { Types.name = "dish-10M-plutus",                                      Types.composition = compTriplet         , Types.node = (nodeDefault (Just 2400) Nothing)                                  , Types.tracer = tracerDefault})
  , (dummy { Types.name = "dish",                                                 Types.composition = compTriplet         , Types.node = (nodeDefault (Just 2400) Nothing)                                  , Types.tracer = tracerDefault})
  , (dummy { Types.name = "dish-plutus",                                          Types.composition = compTriplet         , Types.node = (nodeDefault (Just 2400) Nothing)                                  , Types.tracer = tracerDefault})
  , (dummy { Types.name = "epoch-transition",                                     Types.composition = compDoubletLoopback , Types.node = (nodeDefault (Just 900) Nothing)                                   , Types.tracer = tracerDefault})
  , (dummy { Types.name = "fast",                                                 Types.composition = compDoubletLoopback , Types.node = (nodeDefault Nothing (Just 1))                                     , Types.tracer = tracerDefault})
  , (dummy { Types.name = "fast-notracer",                                        Types.composition = compDoubletLoopback , Types.node = nodeNoTracer (nodeDefault Nothing (Just 1))                        , Types.tracer = tracerDefault})
  , (dummy { Types.name = "fast-oldtracing",                                      Types.composition = compDoubletLoopback , Types.node = nodeOldTracing (nodeDefault Nothing (Just 1))                      , Types.tracer = tracerDefault})
  , (dummy { Types.name = "fast-p2p",                                             Types.composition = compDoubletLoopback , Types.node = nodeP2P (nodeDefault Nothing (Just 1))                             , Types.tracer = tracerDefault})
  , (dummy { Types.name = "fast-plutus",                                          Types.composition = compDoubletLoopback , Types.node = (nodeDefault Nothing (Just 1))                                     , Types.tracer = tracerDefault})
  , (dummy { Types.name = "forge-stress",                                         Types.composition = compTriplet         , Types.node = (nodeDefault (Just 2400) Nothing)                                  , Types.tracer = tracerDefault})
  , (dummy { Types.name = "forge-stress-large",                                   Types.composition = compHexagon         , Types.node = (nodeDefault (Just 4800) Nothing)                                  , Types.tracer = tracerDefault})
  , (dummy { Types.name = "forge-stress-light",                                   Types.composition = compTriplet         , Types.node = (nodeDefault (Just 2400) Nothing)                                  , Types.tracer = tracerDefault})
  , (dummy { Types.name = "forge-stress-notracer",                                Types.composition = compTriplet         , Types.node = nodeNoTracer (nodeDefault (Just 2400) Nothing)                     , Types.tracer = tracerDefault})
  , (dummy { Types.name = "forge-stress-p2p",                                     Types.composition = compTriplet         , Types.node = (nodeDefault (Just 2400) Nothing)                                  , Types.tracer = tracerDefault})
  , (dummy { Types.name = "forge-stress-plutus",                                  Types.composition = compTriplet         , Types.node = (nodeDefault (Just 2400) Nothing)                                  , Types.tracer = tracerDefault})
  , (dummy { Types.name = "forge-stress-plutus-solo",                             Types.composition = compSolo            , Types.node = (nodeDefault (Just 2400) Nothing)                                  , Types.tracer = tracerDefault})
  , (dummy { Types.name = "forge-stress-pre",                                     Types.composition = compTriplet         , Types.node = (nodeDefault (Just 2400) Nothing)                                  , Types.tracer = tracerDefault})
  , (dummy { Types.name = "forge-stress-pre-notracer",                            Types.composition = compTriplet         , Types.node = nodeNoTracer (nodeDefault (Just 2400) Nothing)                     , Types.tracer = tracerDefault})
  , (dummy { Types.name = "forge-stress-pre-plutus",                              Types.composition = compTriplet         , Types.node = (nodeDefault (Just 2400) Nothing)                                  , Types.tracer = tracerDefault})
  , (dummy { Types.name = "forge-stress-pre-rtsA4m",                              Types.composition = compTriplet         , Types.node = nodeRtsA4m (nodeDefault (Just 2400) Nothing)                       , Types.tracer = tracerDefault})
  , (dummy { Types.name = "forge-stress-pre-rtsA4mN3",                            Types.composition = compTriplet         , Types.node = nodeRtsA4mN3 (nodeDefault (Just 2400) Nothing)                     , Types.tracer = tracerDefault})
  , (dummy { Types.name = "forge-stress-pre-rtsA64m",                             Types.composition = compTriplet         , Types.node = nodeRtsA64m (nodeDefault (Just 2400) Nothing)                      , Types.tracer = tracerDefault})
  , (dummy { Types.name = "forge-stress-pre-rtsA64mN3",                           Types.composition = compTriplet         , Types.node = nodeRtsA64mN3 (nodeDefault (Just 2400) Nothing)                    , Types.tracer = tracerDefault})
  , (dummy { Types.name = "forge-stress-pre-rtsN3",                               Types.composition = compTriplet         , Types.node = nodeRtsN3 (nodeDefault (Just 2400) Nothing)                        , Types.tracer = tracerDefault})
  , (dummy { Types.name = "forge-stress-pre-rtsxn",                               Types.composition = compTriplet         , Types.node = nodeRtsXn (nodeDefault (Just 2400) Nothing)                        , Types.tracer = tracerDefault})
  , (dummy { Types.name = "forge-stress-pre-solo",                                Types.composition = compSolo            , Types.node = (nodeDefault (Just 2400) Nothing)                                  , Types.tracer = tracerDefault})
  , (dummy { Types.name = "forge-stress-solo",                                    Types.composition = compSolo            , Types.node = (nodeDefault (Just 2400) Nothing)                                  , Types.tracer = tracerDefault})
  , (dummy { Types.name = "idle",                                                 Types.composition = compHexagon         , Types.node = (nodeDefault Nothing Nothing)                                      , Types.tracer = tracerDefault})
  , (dummy { Types.name = "k3-3ep-18kTx-10000kU-1300kD-64kbs-10tps-fixed-loaded", Types.composition = compTriplet         , Types.node = (nodeDefault Nothing Nothing)                                      , Types.tracer = tracerDefault})
  , (dummy { Types.name = "k3-3ep-22kTx-10000kU-1300kD-64kbs-fixed-loaded",       Types.composition = compTriplet         , Types.node = (nodeDefault Nothing Nothing)                                      , Types.tracer = tracerDefault})
  , (dummy { Types.name = "k3-3ep-5kTx-10000kU-1300kD-64kbs-fixed-loaded",        Types.composition = compTriplet         , Types.node = (nodeDefault Nothing Nothing)                                      , Types.tracer = tracerDefault})
  , (dummy { Types.name = "k3-3ep-9kTx-10000kU-1300kD-64kbs-5tps-fixed-loaded",   Types.composition = compTriplet         , Types.node = (nodeDefault Nothing Nothing)                                      , Types.tracer = tracerDefault})
  , (dummy { Types.name = "model-secp-ecdsa-double",                              Types.composition = compQuadruplet      , Types.node = (nodeDefault (Just 56000) Nothing)                                 , Types.tracer = tracerDefault})
  , (dummy { Types.name = "model-secp-ecdsa-half",                                Types.composition = compQuadruplet      , Types.node = (nodeDefault (Just 56000) Nothing)                                 , Types.tracer = tracerDefault})
  , (dummy { Types.name = "model-secp-ecdsa-plain",                               Types.composition = compQuadruplet      , Types.node = (nodeDefault (Just 56000) Nothing)                                 , Types.tracer = tracerDefault})
  , (dummy { Types.name = "model-value",                                          Types.composition = compQuadruplet      , Types.node = (nodeDefault (Just 56000) Nothing)                                 , Types.tracer = tracerDefault})
  , (dummy { Types.name = "model-value-test",                                     Types.composition = compQuadruplet      , Types.node = (nodeDefault (Just 56000) Nothing)                                 , Types.tracer = tracerDefault})
  , (dummy { Types.name = "oldtracing",                                           Types.composition = compHexagon         , Types.node = nodeOldTracing (nodeDefault Nothing Nothing)                       , Types.tracer = tracerDefault})
  , (dummy { Types.name = "oldtracing-nomadperf",                                 Types.composition = compHexagonNomadPerf, Types.node = nodeOldTracing $ nodeP2P (nodeDefault Nothing Nothing)             , Types.tracer = tracerDefault})
  , (dummy { Types.name = "oldtracing-nomadperf-nop2p",                           Types.composition = compHexagonNomadPerf, Types.node = nodeOldTracing (nodeDefault Nothing Nothing)                       , Types.tracer = tracerDefault})
  , (dummy { Types.name = "plutus",                                               Types.composition = compHexagon         , Types.node = (nodeDefault Nothing Nothing)                                      , Types.tracer = tracerDefault})
  , (dummy { Types.name = "plutuscall-loop-double",                               Types.composition = compHexagon         , Types.node = (nodeDefault (Just 9000) Nothing)                                  , Types.tracer = tracerDefault})
  , (dummy { Types.name = "plutuscall-loop-half",                                 Types.composition = compHexagon         , Types.node = (nodeDefault (Just 9000) Nothing)                                  , Types.tracer = tracerDefault})
  , (dummy { Types.name = "plutuscall-loop-plain",                                Types.composition = compHexagon         , Types.node = (nodeDefault (Just 9000) Nothing)                                  , Types.tracer = tracerDefault})
  , (dummy { Types.name = "plutuscall-secp-ecdsa-double",                         Types.composition = compHexagon         , Types.node = (nodeDefault (Just 9000) Nothing)                                  , Types.tracer = tracerDefault})
  , (dummy { Types.name = "plutuscall-secp-ecdsa-half",                           Types.composition = compHexagon         , Types.node = (nodeDefault (Just 9000) Nothing)                                  , Types.tracer = tracerDefault})
  , (dummy { Types.name = "plutuscall-secp-ecdsa-plain",                          Types.composition = compHexagon         , Types.node = (nodeDefault (Just 9000) Nothing)                                  , Types.tracer = tracerDefault})
  , (dummy { Types.name = "plutuscall-secp-schnorr-double",                       Types.composition = compHexagon         , Types.node = (nodeDefault (Just 9000) Nothing)                                  , Types.tracer = tracerDefault})
  , (dummy { Types.name = "plutuscall-secp-schnorr-half",                         Types.composition = compHexagon         , Types.node = (nodeDefault (Just 9000) Nothing)                                  , Types.tracer = tracerDefault})
  , (dummy { Types.name = "plutuscall-secp-schnorr-plain",                        Types.composition = compHexagon         , Types.node = (nodeDefault (Just 9000) Nothing)                                  , Types.tracer = tracerDefault})
  , (dummy { Types.name = "plutus-nomadperf",                                     Types.composition = compComposeFiftyTwo , Types.node = nodeP2P (nodeDefault (Just 72000) Nothing)                         , Types.tracer = tracerDefault})
  , (dummy { Types.name = "plutus-nomadperf-nop2p",                               Types.composition = compComposeFiftyTwo , Types.node = (nodeDefault (Just 72000) Nothing)                                 , Types.tracer = tracerDefault})
  , (dummy { Types.name = "plutus-secp-ecdsa",                                    Types.composition = compHexagon         , Types.node = (nodeDefault Nothing Nothing)                                      , Types.tracer = tracerDefault})
  , (dummy { Types.name = "plutus-secp-schnorr",                                  Types.composition = compHexagon         , Types.node = (nodeDefault Nothing Nothing)                                      , Types.tracer = tracerDefault})
  , (dummy { Types.name = "trace-bench",                                          Types.composition = compHexagonTorus    , Types.node = (nodeDefault Nothing (Just 15))                                    , Types.tracer = tracerWithresources})
  , (dummy { Types.name = "trace-bench-notracer",                                 Types.composition = compHexagonTorus    , Types.node = nodeNoTracer (nodeDefault Nothing (Just 15))                       , Types.tracer = tracerWithresources})
  , (dummy { Types.name = "trace-bench-oldtracing",                               Types.composition = compHexagonTorus    , Types.node = nodeOldTracing (nodeDefault Nothing (Just 15))                     , Types.tracer = tracerWithresources})
  , (dummy { Types.name = "trace-bench-rtview",                                   Types.composition = compHexagonTorus    , Types.node = (nodeDefault Nothing (Just 15))                                    , Types.tracer = tracerRtviewWithresources})
  , (dummy { Types.name = "trace-full",                                           Types.composition = compHexagonTorus    , Types.node = (nodeDefault (Just 1200) Nothing)                                  , Types.tracer = tracerWithresources})
  , (dummy { Types.name = "trace-full-rtview",                                    Types.composition = compHexagonTorus    , Types.node = (nodeDefault (Just 1200) Nothing)                                  , Types.tracer = tracerRtviewWithresources})
  , (dummy { Types.name = "tracer-only",                                          Types.composition = compHexagon         , Types.node = (nodeDefault Nothing Nothing)                                      , Types.tracer = tracerDefault})
  , (dummy { Types.name = "value-nomadperf",                                      Types.composition = compComposeFiftyTwo , Types.node = nodeP2P (nodeDefault (Just 64000) Nothing)                         , Types.tracer = tracerDefault})
  , (dummy { Types.name = "value-nomadperf-nop2p",                                Types.composition = compComposeFiftyTwo , Types.node = (nodeDefault (Just 64000) Nothing)                                 , Types.tracer = tracerDefault})
  , (dummy { Types.name = "value-oldtracing-nomadperf",                           Types.composition = compComposeFiftyTwo , Types.node = nodeOldTracing $ nodeP2P (nodeDefault (Just 64000) Nothing)        , Types.tracer = tracerDefault})
  , (dummy { Types.name = "value-oldtracing-nomadperf-nop2p",                     Types.composition = compComposeFiftyTwo , Types.node = nodeOldTracing (nodeDefault (Just 64000) Nothing)                  , Types.tracer = tracerDefault})
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
      { n_singular_hosts:               4
      , n_dense_hosts:                  0
      }
    } as $quadruplet
  |
    { composition:
      { n_singular_hosts:               6
      , n_dense_hosts:                  0
      }
    } as $hexagon
  |
    { composition:
      { n_singular_hosts:               10
      , n_dense_hosts:                  0
      }
    } as $tenner
  |
    { composition:
      { n_singular_hosts:               52
      , n_dense_hosts:                  0
      }
    } as $compose_fiftytwo
  |
    { composition:
      { topology:                       "torus"
      }
    } as $torus
  |
    { composition:
      { n_singular_hosts:               0
      , n_dense_hosts:                  0
      , with_chaindb_server:            true
      , with_explorer:                  true
      }
    } as $chainsync_cluster
  |
    # "qa" class Nodes of Cardano World Nomad cluster
    { composition:
      { locations:                      ["eu-central-1", "us-east-2"]
      , topology:                       "torus"
      , with_explorer:                  true
      }
    } as $nomad_cardano_world_qa
  |
    # P&T exclusive Nomad cluster Nodes
    { composition:
      { locations:                      ["eu-central-1", "us-east-1", "ap-southeast-2"]
      , topology:                       "torus"
      , with_explorer:                  true
      }
    } as $nomad_perf_torus
  |
    # nomad_perf using cardano-ops "dense" topology
    # Can only be used with the 52 + explorer value profile!
    { composition:
      { locations:                      ["eu-central-1", "us-east-1", "ap-southeast-2"]
      , topology:                       "torus-dense"
      , with_explorer:                  true
      }
    } as $nomad_perf_dense

--}

{-- Used by:
wb profile all-profiles | jq 'map(select(.composition.n_singular_hosts == 1))' | jq 'map(.name) | sort'
[
  "forge-stress-plutus-solo",
  "forge-stress-pre-solo",
  "forge-stress-solo",
]
wb profile all-profiles | jq 'map(select(.composition.n_singular_hosts == 1))' | jq .[] | jq -c .composition | sort | uniq
{"locations":["loopback"],"n_bft_hosts":0,"n_singular_hosts":1,"n_dense_hosts":0,"dense_pool_density":1,"with_proxy":false,"with_explorer":false,"topology":"uni-circle","n_hosts":1,"n_pools":1,"n_singular_pools":1,"n_dense_pools":0,"n_pool_hosts":1}
--}
compSolo :: Types.Composition
compSolo = Types.Composition {
    Types.locations = [Types.Loopback]
  , Types.n_bft_hosts = 0
  , Types.n_singular_hosts = 1
  , Types.n_dense_hosts = 0
  , Types.dense_pool_density = 1
  , Types.with_proxy = False
  , Types.with_explorer = False
  , Types.topology = Types.UniCircle
  , Types.with_chaindb_server = Nothing
  , Types.n_hosts = 1
  , Types.n_pools = 1
  , Types.n_singular_pools = 1
  , Types.n_dense_pools = 0
  , Types.n_pool_hosts = 1
}

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
wb profile all-profiles | jq 'map(select(.composition.n_singular_hosts == 2))' | jq 'map(select(.composition.locations != ["loopback"]))' | jq 'map(.name) | sort'
[
  "ci-bench-nomadperf",
  "ci-bench-nomadperf-nop2p",
  "ci-bench-oldtracing-nomadperf",
  "ci-test-nomadperf",
  "ci-test-nomadperf-nop2p",
  "ci-test-oldtracing-nomadperf",
]
wb profile all-profiles | jq 'map(select(.composition.n_singular_hosts == 2))' | jq 'map(select(.composition.locations != ["loopback"]))' | jq .[] | jq -c .composition | sort | uniq
{"locations":["eu-central-1","us-east-1","ap-southeast-2"],"n_bft_hosts":0,"n_singular_hosts":2,"n_dense_hosts":0,"dense_pool_density":1,"with_proxy":false,"with_explorer":true,"topology":"torus","n_hosts":2,"n_pools":2,"n_singular_pools":2,"n_dense_pools":0,"n_pool_hosts":2}
--}
compDoubletNomadPerf :: Types.Composition
compDoubletNomadPerf = Types.Composition {
    Types.locations = [
      Types.AWS Types.EU_CENTRAL_1
    , Types.AWS Types.US_EAST_1
    , Types.AWS Types.AP_SOUTHEAST_2
    ]
  , Types.n_bft_hosts = 0
  , Types.n_singular_hosts = 2
  , Types.n_dense_hosts = 0
  , Types.dense_pool_density = 1
  , Types.with_proxy = False
  , Types.with_explorer = True
  , Types.topology = Types.Torus
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
wb profile all-profiles | jq 'map(select(.composition.n_singular_hosts == 10))' | jq 'map(.name) | sort'
[
  "10",
  "10-notracer",
  "10-p2p",
  "10-plutus",
]
wb profile all-profiles | jq 'map(select(.composition.n_singular_hosts == 10))' | jq .[] | jq -c .composition | sort | uniq
{"locations":["loopback"],"n_bft_hosts":0,"n_singular_hosts":10,"n_dense_hosts":0,"dense_pool_density":1,"with_proxy":false,"with_explorer":false,"topology":"uni-circle","n_hosts":10,"n_pools":10,"n_singular_pools":10,"n_dense_pools":0,"n_pool_hosts":10}
--}
compTenner :: Types.Composition
compTenner = Types.Composition {
    Types.locations = [Types.Loopback]
  , Types.n_bft_hosts = 0
  , Types.n_singular_hosts = 10
  , Types.n_dense_hosts = 0
  , Types.dense_pool_density = 1
  , Types.with_proxy = False
  , Types.with_explorer = False
  , Types.topology = Types.UniCircle
  , Types.with_chaindb_server = Nothing
  , Types.n_hosts = 10
  , Types.n_pools = 10
  , Types.n_singular_pools = 10
  , Types.n_dense_pools = 0
  , Types.n_pool_hosts = 10
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

compCiTest :: Types.Composition
compCiTest = Types.Composition {
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

TODO: FIXME: "forge-stress-p2p" has no P2P enabled!
--}
nodeP2P :: Types.Node -> Types.Node
nodeP2P node = node {Types.verbatim = Types.NodeVerbatim (Just True)}

{-- RTS options to control the garbage collector

-A ⟨size⟩
Set the allocation area size used by the garbage collector. The allocation area
(actually generation 0 step 0) is fixed and is never resized (unless you use
-H [⟨size⟩], below).

Optimal settings depend on the actual machine, program, and other RTS options.
Increasing the allocation area size means worse cache behaviour but fewer
garbage collections and less promotion.

In general settings >= 4MB can reduce performance in some cases, in particular
for single threaded operation. However in a parallel setting increasing the
allocation area to 16MB, or even 64MB can increase gc throughput significantly.

With only 1 generation (e.g. -G1, see -G ⟨generations⟩) the -A option specifies
the minimum allocation area, since the actual size of the allocation area will
be resized according to the amount of data in the heap (see -F ⟨factor⟩, below).

When heap profiling using a smaller allocation area can increase accuracy as
more frequent major garbage collections also results in more frequent heap
snapshots

https://downloads.haskell.org/ghc/latest/docs/users_guide/runtime_control.html#rts-flag--A%20%E2%9F%A8size%E2%9F%A9
--}

{-- Used by:
wb profile all-profiles | jq 'map(select( .node.rts_flags_override == ["-A4m"] ))' | jq 'map(.name)'
[
  "forge-stress-pre-rtsA4m",
]
--}
nodeRtsA4m :: Types.Node -> Types.Node
nodeRtsA4m node = node {Types.rts_flags_override = ["-A4m"]}

{-- Used by:
wb profile all-profiles | jq 'map(select( .node.rts_flags_override == ["-A64m"] ))' | jq 'map(.name)'
[
  "forge-stress-pre-rtsA64m",
]
--}
nodeRtsA64m :: Types.Node -> Types.Node
nodeRtsA64m node = node {Types.rts_flags_override = ["-A64m"]}

{-- RTS options to control the garbage collector

--nonmoving-gc
Enable the concurrent mark-and-sweep garbage collector for old generation
collectors. Typically GHC uses a stop-the-world copying garbage collector for
all generations. This can cause long pauses in execution during major garbage
collections. --nonmoving-gc enables the use of a concurrent mark-and-sweep
garbage collector for oldest generation collections. Under this collection
strategy oldest-generation garbage collection can proceed concurrently with
mutation.

https://downloads.haskell.org/ghc/latest/docs/users_guide/runtime_control.html#rts-flag---nonmoving-gc
--}

{-- Used by:
wb profile all-profiles | jq 'map(select( .node.rts_flags_override == ["-xn"] ))' | jq 'map(.name)'
[
  "forge-stress-pre-rtsxn"
]
--}
nodeRtsXn :: Types.Node -> Types.Node
nodeRtsXn node = node {Types.rts_flags_override = ["-xn"]}

{-- RTS options for SMP parallelism
Use ⟨x⟩ simultaneous threads when running the program.

The runtime manages a set of virtual processors, which we call capabilities, the
number of which is determined by the -N option. Each capability can run one
Haskell thread at a time, so the number of capabilities is equal to the number
of Haskell threads that can run physically in parallel. A capability is animated
by one or more OS threads; the runtime manages a pool of OS threads for each
capability, so that if a Haskell thread makes a foreign call (see
Multi-threading and the FFI) another OS thread can take over that capability.

Normally ⟨x⟩ should be chosen to match the number of CPU cores on the machine
[1]. For example, on a dual-core machine we would probably use +RTS -N2 -RTS.

Omitting ⟨x⟩, i.e. +RTS -N -RTS, lets the runtime choose the value of ⟨x⟩ itself
based on how many processors are in your machine.

Omitting -N⟨x⟩ entirely means -N1.

With -maxN⟨x⟩, i.e. +RTS -maxN3 -RTS, the runtime will choose at most (x), also
limited by the number of processors on the system. Omitting (x) is an error, if
you need a default use option -N.

Be careful when using all the processors in your machine: if some of your
processors are in use by other programs, this can actually harm performance
rather than improve it. Asking GHC to create more capabilities than you have
physical threads is almost always a bad idea.

Setting -N also has the effect of enabling the parallel garbage collector (see
RTS options to control the garbage collector).

The current value of the -N option is available to the Haskell program via
Control.Concurrent.getNumCapabilities, and it may be changed while the program
is running by calling Control.Concurrent.setNumCapabilities.

https://downloads.haskell.org/ghc/latest/docs/users_guide/using-concurrent.html#rts-options-for-smp-parallelism
--}

{-- Used by:
wb profile all-profiles | jq 'map(select( .node.rts_flags_override == ["-N3"] ))' | jq 'map(.name)'
[
  "forge-stress-pre-rtsN3",
]
--}
nodeRtsN3 :: Types.Node -> Types.Node
nodeRtsN3 node = node {Types.rts_flags_override = ["-N3"]}

{-- Used by:
wb profile all-profiles | jq 'map(select( .node.rts_flags_override == ["-A4m", "-N3"] ))' | jq 'map(.name)'
[
  "forge-stress-pre-rtsA4mN3",
]
--}
nodeRtsA4mN3 :: Types.Node -> Types.Node
nodeRtsA4mN3 node = node {Types.rts_flags_override = ["-A4m", "-N3"]}

{-- Used by:
wb profile all-profiles | jq 'map(select( .node.rts_flags_override == ["-A64m", "-N3"] ))' | jq 'map(.name)'
[
  "forge-stress-pre-rtsA64mN3",
]
--}
nodeRtsA64mN3 :: Types.Node -> Types.Node
nodeRtsA64mN3 node = node {Types.rts_flags_override = ["-A64m", "-N3"]}

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
wb profile all-profiles | jq 'map(select( .tracer.rtview == true and .tracer.withresources == false ))' | jq 'map(.name)'
[
  "ci-test-rtview",
  "ci-bench-rtview",
]
--}
tracerRtview :: Types.Tracer
tracerRtview = Types.Tracer {
  Types.rtview = True
, Types.ekg = False
, Types.withresources= False
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

--------------------------------------------------------------------------------

nodeCiTest :: Types.Node
nodeCiTest = Types.Node {
    Types.rts_flags_override = []
  , Types.shutdown_on_slot_synced = Nothing
  , Types.shutdown_on_block_synced = Just 3
  , Types.tracing_backend = "trace-dispatcher"
  , Types.nodeTracer = True
  , Types.verbatim = Types.NodeVerbatim Nothing
}

tracerCiTest :: Types.Tracer
tracerCiTest = Types.Tracer {
    Types.rtview = False
  , Types.ekg = False
  , Types.withresources = False
}

generatorCiTest :: Types.Generator
generatorCiTest = Types.Generator {
    Types.add_tx_size = 100
  , Types.init_cooldown = 5
  , Types.inputs_per_tx = 2
  , Types.outputs_per_tx = 2
  , Types.tx_fee = 1000000
  , Types.epochs = 3
  , Types.tps = 15
  , Types.plutus = Nothing
  , Types.tx_count = 9000
}

analysisCiTest :: Types.Analysis
analysisCiTest = Types.Analysis {
    Types.analysisType = Just "standard"
  , Types.cluster_base_startup_overhead_s = 40
  , Types.start_log_spread_s = 120
  , Types.last_log_spread_s = 120
  , Types.silence_since_last_block_s = 120
  , Types.tx_loss_ratio = Scientific.fromFloatDigits (0.02 :: Double)
  , Types.finish_patience = 21
  , Types.filters = []
  , Types.filter_exprs = [
    (Types.AnalysisFilterExpression {
      Types.tag = "CBlock"
    , Types.contents = Types.AnalysisFilterExpressionContent {
        Types.innerTag = "BMinimumAdoptions"
      , Types.innerContents = 1
      }
    })
  ]
  , Types.minimum_chain_density = Scientific.fromFloatDigits (0.025 :: Double)
  , Types.cluster_startup_overhead_s = 40
}
