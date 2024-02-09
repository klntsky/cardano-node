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
  , Types.composition = compositionCiTest
  , Types.era = Types.Babbage
--  , Types.genesis = Nothing
  , Types.scenario = Types.FixedLoaded
  , Types.node = nodeCiTest
  , Types.tracer = tracerCiTest
  , Types.generator = generatorCiTest
  , Types.analysis = analysisCiTest
--  , Types.overlay = mempty
}

profilesNoEra :: Map.Map String Types.Profile
-- Names:
-- wb profile all-profiles | jq .[] | jq -r .name | sort | uniq | grep "\-bage"
profilesNoEra = Map.fromList $ map (\p -> (Types.name p, p))
  [
    (dummy { Types.name = "10",                                                   Types.composition = compositionTenner          , Types.tracer = tracerDefault})
  , (dummy { Types.name = "10-notracer",                                          Types.composition = compositionTenner          , Types.tracer = tracerDefault})
  , (dummy { Types.name = "10-p2p",                                               Types.composition = compositionTenner          , Types.tracer = tracerDefault})
  , (dummy { Types.name = "10-plutus",                                            Types.composition = compositionTenner          , Types.tracer = tracerDefault})
  , (dummy { Types.name = "chainsync-early-alonzo",                               Types.composition = compositionSoloChainsync   , Types.tracer = tracerDefault})
  , (dummy { Types.name = "chainsync-early-alonzo-notracer",                      Types.composition = compositionSoloChainsync   , Types.tracer = tracerDefault})
  , (dummy { Types.name = "chainsync-early-alonzo-oldtracing",                    Types.composition = compositionSoloChainsync   , Types.tracer = tracerDefault})
  , (dummy { Types.name = "chainsync-early-alonzo-p2p",                           Types.composition = compositionSoloChainsync   , Types.tracer = tracerDefault})
  , (dummy { Types.name = "chainsync-early-byron",                                Types.composition = compositionSoloChainsync   , Types.tracer = tracerDefault})
  , (dummy { Types.name = "chainsync-early-byron-notracer",                       Types.composition = compositionSoloChainsync   , Types.tracer = tracerDefault})
  , (dummy { Types.name = "chainsync-early-byron-oldtracing",                     Types.composition = compositionSoloChainsync   , Types.tracer = tracerDefault})
  , (dummy { Types.name = "ci-bench",                                             Types.composition = compositionDoubletLoopback , Types.tracer = tracerDefault})
  , (dummy { Types.name = "ci-bench-nomadperf",                                   Types.composition = compositionDoubletNomadPerf, Types.tracer = tracerDefault})
  , (dummy { Types.name = "ci-bench-nomadperf-nop2p",                             Types.composition = compositionDoubletNomadPerf, Types.tracer = tracerDefault})
  , (dummy { Types.name = "ci-bench-notracer",                                    Types.composition = compositionDoubletLoopback , Types.tracer = tracerDefault})
  , (dummy { Types.name = "ci-bench-oldtracing-nomadperf",                        Types.composition = compositionDoubletNomadPerf, Types.tracer = tracerDefault})
  , (dummy { Types.name = "ci-bench-p2p",                                         Types.composition = compositionDoubletLoopback , Types.tracer = tracerDefault})
  , (dummy { Types.name = "ci-bench-plutus",                                      Types.composition = compositionDoubletLoopback , Types.tracer = tracerDefault})
  , (dummy { Types.name = "ci-bench-plutus-secp-ecdsa",                           Types.composition = compositionDoubletLoopback , Types.tracer = tracerDefault})
  , (dummy { Types.name = "ci-bench-plutus-secp-schnorr",                         Types.composition = compositionDoubletLoopback , Types.tracer = tracerDefault})
  , (dummy { Types.name = "ci-bench-rtview",                                      Types.composition = compositionDoubletLoopback , Types.tracer = tracerRtview})
  , (dummy { Types.name = "ci-test",                                              Types.composition = compositionDoubletLoopback , Types.tracer = tracerDefault})
  , (dummy { Types.name = "ci-test-dense10",                                      Types.composition = compositionSoloDense10     , Types.tracer = tracerDefault})
  , (dummy { Types.name = "ci-test-nomadperf",                                    Types.composition = compositionDoubletNomadPerf, Types.tracer = tracerDefault})
  , (dummy { Types.name = "ci-test-nomadperf-nop2p",                              Types.composition = compositionDoubletNomadPerf, Types.tracer = tracerDefault})
  , (dummy { Types.name = "ci-test-notracer",                                     Types.composition = compositionDoubletLoopback , Types.tracer = tracerDefault})
  , (dummy { Types.name = "ci-test-oldtracing-nomadperf",                         Types.composition = compositionDoubletNomadPerf, Types.tracer = tracerDefault})
  , (dummy { Types.name = "ci-test-p2p",                                          Types.composition = compositionDoubletLoopback , Types.tracer = tracerDefault})
  , (dummy { Types.name = "ci-test-plutus",                                       Types.composition = compositionDoubletLoopback , Types.tracer = tracerDefault})
  , (dummy { Types.name = "ci-test-rtview",                                       Types.composition = compositionDoubletLoopback , Types.tracer = tracerRtview})
  , (dummy { Types.name = "default",                                              Types.composition = compositionHexagon         , Types.tracer = tracerDefault})
  , (dummy { Types.name = "default-nomadperf",                                    Types.composition = compositionHexagonNomadPerf, Types.tracer = tracerDefault})
  , (dummy { Types.name = "default-nomadperf-nop2p",                              Types.composition = compositionHexagonNomadPerf, Types.tracer = tracerDefault})
  , (dummy { Types.name = "default-p2p",                                          Types.composition = compositionHexagon         , Types.tracer = tracerDefault})
  , (dummy { Types.name = "devops",                                               Types.composition = compositionHexagon         , Types.tracer = tracerDefault})
  , (dummy { Types.name = "dish-10M",                                             Types.composition = compositionTriplet         , Types.tracer = tracerDefault})
  , (dummy { Types.name = "dish-10M-plutus",                                      Types.composition = compositionTriplet         , Types.tracer = tracerDefault})
  , (dummy { Types.name = "dish",                                                 Types.composition = compositionTriplet         , Types.tracer = tracerDefault})
  , (dummy { Types.name = "dish-plutus",                                          Types.composition = compositionTriplet         , Types.tracer = tracerDefault})
  , (dummy { Types.name = "epoch-transition",                                     Types.composition = compositionDoubletLoopback , Types.tracer = tracerDefault})
  , (dummy { Types.name = "fast",                                                 Types.composition = compositionDoubletLoopback , Types.tracer = tracerDefault})
  , (dummy { Types.name = "fast-notracer",                                        Types.composition = compositionDoubletLoopback , Types.tracer = tracerDefault})
  , (dummy { Types.name = "fast-oldtracing",                                      Types.composition = compositionDoubletLoopback , Types.tracer = tracerDefault})
  , (dummy { Types.name = "fast-p2p",                                             Types.composition = compositionDoubletLoopback , Types.tracer = tracerDefault})
  , (dummy { Types.name = "fast-plutus",                                          Types.composition = compositionDoubletLoopback , Types.tracer = tracerDefault})
  , (dummy { Types.name = "forge-stress",                                         Types.composition = compositionTriplet         , Types.tracer = tracerDefault})
  , (dummy { Types.name = "forge-stress-large",                                   Types.composition = compositionHexagon         , Types.tracer = tracerDefault})
  , (dummy { Types.name = "forge-stress-light",                                   Types.composition = compositionTriplet         , Types.tracer = tracerDefault})
  , (dummy { Types.name = "forge-stress-notracer",                                Types.composition = compositionTriplet         , Types.tracer = tracerDefault})
  , (dummy { Types.name = "forge-stress-p2p",                                     Types.composition = compositionTriplet         , Types.tracer = tracerDefault})
  , (dummy { Types.name = "forge-stress-plutus",                                  Types.composition = compositionTriplet         , Types.tracer = tracerDefault})
  , (dummy { Types.name = "forge-stress-plutus-solo",                             Types.composition = compositionSolo            , Types.tracer = tracerDefault})
  , (dummy { Types.name = "forge-stress-pre",                                     Types.composition = compositionTriplet         , Types.tracer = tracerDefault})
  , (dummy { Types.name = "forge-stress-pre-notracer",                            Types.composition = compositionTriplet         , Types.tracer = tracerDefault})
  , (dummy { Types.name = "forge-stress-pre-plutus",                              Types.composition = compositionTriplet         , Types.tracer = tracerDefault})
  , (dummy { Types.name = "forge-stress-pre-rtsA4m",                              Types.composition = compositionTriplet         , Types.tracer = tracerDefault})
  , (dummy { Types.name = "forge-stress-pre-rtsA4mN3",                            Types.composition = compositionTriplet         , Types.tracer = tracerDefault})
  , (dummy { Types.name = "forge-stress-pre-rtsA64m",                             Types.composition = compositionTriplet         , Types.tracer = tracerDefault})
  , (dummy { Types.name = "forge-stress-pre-rtsA64mN3",                           Types.composition = compositionTriplet         , Types.tracer = tracerDefault})
  , (dummy { Types.name = "forge-stress-pre-rtsN3",                               Types.composition = compositionTriplet         , Types.tracer = tracerDefault})
  , (dummy { Types.name = "forge-stress-pre-rtsxn",                               Types.composition = compositionTriplet         , Types.tracer = tracerDefault})
  , (dummy { Types.name = "forge-stress-pre-solo",                                Types.composition = compositionSolo            , Types.tracer = tracerDefault})
  , (dummy { Types.name = "forge-stress-solo",                                    Types.composition = compositionSolo            , Types.tracer = tracerDefault})
  , (dummy { Types.name = "idle",                                                 Types.composition = compositionHexagon         , Types.tracer = tracerDefault})
  , (dummy { Types.name = "k3-3ep-18kTx-10000kU-1300kD-64kbs-10tps-fixed-loaded", Types.composition = compositionTriplet         , Types.tracer = tracerDefault})
  , (dummy { Types.name = "k3-3ep-22kTx-10000kU-1300kD-64kbs-fixed-loaded",       Types.composition = compositionTriplet         , Types.tracer = tracerDefault})
  , (dummy { Types.name = "k3-3ep-5kTx-10000kU-1300kD-64kbs-fixed-loaded",        Types.composition = compositionTriplet         , Types.tracer = tracerDefault})
  , (dummy { Types.name = "k3-3ep-9kTx-10000kU-1300kD-64kbs-5tps-fixed-loaded",   Types.composition = compositionTriplet         , Types.tracer = tracerDefault})
  , (dummy { Types.name = "model-secp-ecdsa-double",                              Types.composition = compositionQuadruplet      , Types.tracer = tracerDefault})
  , (dummy { Types.name = "model-secp-ecdsa-half",                                Types.composition = compositionQuadruplet      , Types.tracer = tracerDefault})
  , (dummy { Types.name = "model-secp-ecdsa-plain",                               Types.composition = compositionQuadruplet      , Types.tracer = tracerDefault})
  , (dummy { Types.name = "model-value",                                          Types.composition = compositionQuadruplet      , Types.tracer = tracerDefault})
  , (dummy { Types.name = "model-value-test",                                     Types.composition = compositionQuadruplet      , Types.tracer = tracerDefault})
  , (dummy { Types.name = "oldtracing",                                           Types.composition = compositionHexagon         , Types.tracer = tracerDefault})
  , (dummy { Types.name = "oldtracing-nomadperf",                                 Types.composition = compositionHexagonNomadPerf, Types.tracer = tracerDefault})
  , (dummy { Types.name = "oldtracing-nomadperf-nop2p",                           Types.composition = compositionHexagonNomadPerf, Types.tracer = tracerDefault})
  , (dummy { Types.name = "plutus",                                               Types.composition = compositionHexagon         , Types.tracer = tracerDefault})
  , (dummy { Types.name = "plutuscall-loop-double",                               Types.composition = compositionHexagon         , Types.tracer = tracerDefault})
  , (dummy { Types.name = "plutuscall-loop-half",                                 Types.composition = compositionHexagon         , Types.tracer = tracerDefault})
  , (dummy { Types.name = "plutuscall-loop-plain",                                Types.composition = compositionHexagon         , Types.tracer = tracerDefault})
  , (dummy { Types.name = "plutuscall-secp-ecdsa-double",                         Types.composition = compositionHexagon         , Types.tracer = tracerDefault})
  , (dummy { Types.name = "plutuscall-secp-ecdsa-half",                           Types.composition = compositionHexagon         , Types.tracer = tracerDefault})
  , (dummy { Types.name = "plutuscall-secp-ecdsa-plain",                          Types.composition = compositionHexagon         , Types.tracer = tracerDefault})
  , (dummy { Types.name = "plutuscall-secp-schnorr-double",                       Types.composition = compositionHexagon         , Types.tracer = tracerDefault})
  , (dummy { Types.name = "plutuscall-secp-schnorr-half",                         Types.composition = compositionHexagon         , Types.tracer = tracerDefault})
  , (dummy { Types.name = "plutuscall-secp-schnorr-plain",                        Types.composition = compositionHexagon         , Types.tracer = tracerDefault})
  , (dummy { Types.name = "plutus-nomadperf",                                     Types.composition = compositionComposeFiftyTwo , Types.tracer = tracerDefault})
  , (dummy { Types.name = "plutus-nomadperf-nop2p",                               Types.composition = compositionComposeFiftyTwo , Types.tracer = tracerDefault})
  , (dummy { Types.name = "plutus-secp-ecdsa",                                    Types.composition = compositionHexagon         , Types.tracer = tracerDefault})
  , (dummy { Types.name = "plutus-secp-schnorr",                                  Types.composition = compositionHexagon         , Types.tracer = tracerDefault})
  , (dummy { Types.name = "trace-bench",                                          Types.composition = compositionHexagonTorus    , Types.tracer = tracerWithresources})
  , (dummy { Types.name = "trace-bench-notracer",                                 Types.composition = compositionHexagonTorus    , Types.tracer = tracerWithresources})
  , (dummy { Types.name = "trace-bench-oldtracing",                               Types.composition = compositionHexagonTorus    , Types.tracer = tracerWithresources})
  , (dummy { Types.name = "trace-bench-rtview",                                   Types.composition = compositionHexagonTorus    , Types.tracer = tracerRtviewWithresources})
  , (dummy { Types.name = "trace-full",                                           Types.composition = compositionHexagonTorus    , Types.tracer = tracerWithresources})
  , (dummy { Types.name = "trace-full-rtview",                                    Types.composition = compositionHexagonTorus    , Types.tracer = tracerRtviewWithresources})
  , (dummy { Types.name = "tracer-only",                                          Types.composition = compositionHexagon         , Types.tracer = tracerDefault})
  , (dummy { Types.name = "value-nomadperf",                                      Types.composition = compositionComposeFiftyTwo , Types.tracer = tracerDefault})
  , (dummy { Types.name = "value-nomadperf-nop2p",                                Types.composition = compositionComposeFiftyTwo , Types.tracer = tracerDefault})
  , (dummy { Types.name = "value-oldtracing-nomadperf",                           Types.composition = compositionComposeFiftyTwo , Types.tracer = tracerDefault})
  , (dummy { Types.name = "value-oldtracing-nomadperf-nop2p",                     Types.composition = compositionComposeFiftyTwo , Types.tracer = tracerDefault})
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
compositionSolo :: Types.Composition
compositionSolo = Types.Composition {
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
compositionSoloChainsync :: Types.Composition
compositionSoloChainsync = Types.Composition {
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
compositionSoloDense10 :: Types.Composition
compositionSoloDense10 = Types.Composition {
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
compositionDoubletLoopback :: Types.Composition
compositionDoubletLoopback = Types.Composition {
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
compositionDoubletNomadPerf :: Types.Composition
compositionDoubletNomadPerf = Types.Composition {
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
compositionTriplet :: Types.Composition
compositionTriplet = Types.Composition {
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
compositionQuadruplet :: Types.Composition
compositionQuadruplet = Types.Composition {
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
compositionHexagon :: Types.Composition
compositionHexagon = Types.Composition {
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
compositionHexagonTorus :: Types.Composition
compositionHexagonTorus = Types.Composition {
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
compositionHexagonNomadPerf :: Types.Composition
compositionHexagonNomadPerf = Types.Composition {
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
compositionTenner :: Types.Composition
compositionTenner = Types.Composition {
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
compositionComposeFiftyTwo :: Types.Composition
compositionComposeFiftyTwo = Types.Composition {
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

compositionCiTest :: Types.Composition
compositionCiTest = Types.Composition {
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

nodeCiTest :: Types.Node
nodeCiTest = Types.Node {
    Types.rts_flags_override = []
  , Types.shutdown_on_slot_synced = Nothing
  , Types.shutdown_on_block_synced = Just 3
  , Types.tracing_backend = "trace-dispatcher"
  , Types.nodeTracer = True
  , Types.verbatim = mempty
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
