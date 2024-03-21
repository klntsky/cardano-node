{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

--------------------------------------------------------------------------------

module Cardano.Benchmarking.Profile (
    Types.Profile (Profile)
  -- Name and description.
  , name, desc

  -- Composition topology.
  , uniCircle, torus, torusDense
  -- Composition location.
  , loopback, regions
  -- Composition size.
  , hosts, pools, hostsChainsync, withExplorerNode
  , withChaindbServer

 -- Genesis
  , utxo, delegators, epochLength, parameterK

  -- Scenario.
  , idle, tracerOnly, fixedLoaded, chainsync, latency

  -- Node's --shutdown-on-*-sync.
  , shutdownOnSlot, shutdownOnBlock, shutdownOnOff
  -- Node's p2p flag.
  , p2pOn, p2pOff
  -- Node's tracer flag.
  , tracerOn, tracerOff
  -- Node's tracer type.
  , newTracing, oldTracing
  -- Node's RTS params.
  , rtsGcNonMoving, rtsGcAllocSize, rtsThreads

  -- Tracer's params.
  , tracerRtview, tracerWithresources

  -- Generator params.
  , generatorTps

  -- Analysis params.
  , analysisOff, analysisStandard, analysisPerformance
  , analysisSizeSmall, analysisSizeModerate, analysisSizeFull
  , analysisUnitary, analysisEpoch3Plus
) where

import           Prelude hiding (id)
--import           Data.Function  ((&))
--import           Data.List      (tails, sortOn, uncons)
--import           Data.Maybe     (isJust)
import qualified Data.Scientific as Scientific

import qualified Cardano.Benchmarking.Profile.Types as Types

--------------------------------------------------------------------------------

name :: String -> Types.Profile -> Types.Profile
name str = \p -> p {Types.name = str}

desc :: String -> Types.Profile -> Types.Profile
desc str = \p -> p {Types.desc = Just str}

--------------------------------------------------------------------------------

comp :: (Types.Composition -> Types.Composition) -> Types.Profile -> Types.Profile
comp f p = p {Types.composition = f (Types.composition p)}

uniCircle :: Types.Profile -> Types.Profile
uniCircle = comp (\c -> c {Types.topology = Types.UniCircle})

torus :: Types.Profile -> Types.Profile
torus = comp (\c -> c {Types.topology = Types.Torus})

torusDense :: Types.Profile -> Types.Profile
torusDense = comp (\c -> c {Types.topology = Types.TorusDense})

loopback :: Types.Profile -> Types.Profile
loopback = comp (\c -> c {
  Types.locations = [Types.Loopback]
})

regions :: [Types.Location] -> Types.Profile -> Types.Profile
regions locs = comp (\c -> c {
  Types.locations = locs
})

hosts :: Int -> Types.Profile -> Types.Profile
hosts size = comp (\c -> c {
    Types.n_bft_hosts = 0
  , Types.n_singular_hosts = size
  , Types.n_dense_hosts = 0
  , Types.dense_pool_density = 1
  , Types.n_hosts = size
  , Types.n_pools = size
  , Types.n_singular_pools = size
  , Types.n_dense_pools = 0
  , Types.n_pool_hosts = size
})

pools :: Int -> Types.Profile -> Types.Profile
pools size = comp (\c -> c {
    Types.n_bft_hosts = 0
  , Types.n_singular_hosts = 0
  , Types.n_dense_hosts = 1
  , Types.dense_pool_density = size
  , Types.n_hosts = 1
  , Types.n_pools = size
  , Types.n_singular_pools = 0
  , Types.n_dense_pools = size
  , Types.n_pool_hosts = 1
})

hostsChainsync :: Int -> Types.Profile -> Types.Profile
hostsChainsync size = comp (\c -> c {
    Types.n_bft_hosts = 0
  , Types.n_singular_hosts = 0
  , Types.n_dense_hosts = 0
  , Types.dense_pool_density = size
  , Types.n_hosts = 0
  , Types.n_pools = 0
  , Types.n_singular_pools = 0
  , Types.n_dense_pools = 0
  , Types.n_pool_hosts = 0
})

withExplorerNode :: Types.Profile -> Types.Profile
withExplorerNode = comp (\c -> c {Types.with_explorer = True})

withChaindbServer :: Types.Profile -> Types.Profile
withChaindbServer = comp (\c -> c {Types.with_chaindb_server = Just True})

--------------------------------------------------------------------------------

genesis :: (Types.Genesis -> Types.Genesis) -> Types.Profile -> Types.Profile
genesis f p = p {Types.genesis = f (Types.genesis p)}

utxo :: Int -> Types.Profile -> Types.Profile
utxo i = genesis (\g -> g {Types.utxo = i})

delegators :: Int -> Types.Profile -> Types.Profile
delegators i = genesis (\g -> g {Types.delegators = i})

epochLength :: Int -> Types.Profile -> Types.Profile
epochLength i = genesis (\g -> g {Types.epoch_length = i})

parameterK :: Int -> Types.Profile -> Types.Profile
parameterK i = genesis (\g -> g {Types.parameter_k = i})

--------------------------------------------------------------------------------

idle :: Types.Profile -> Types.Profile
idle p = p {Types.scenario = Types.Idle}

tracerOnly :: Types.Profile -> Types.Profile
tracerOnly p = p {Types.scenario = Types.TracerOnly}

fixedLoaded :: Types.Profile -> Types.Profile
fixedLoaded p = p {Types.scenario = Types.FixedLoaded}

chainsync :: Types.Profile -> Types.Profile
chainsync p = p {Types.scenario = Types.Chainsync}

latency :: Types.Profile -> Types.Profile
latency p = p {Types.scenario = Types.Latency}

--------------------------------------------------------------------------------

node :: (Types.Node -> Types.Node) -> Types.Profile -> Types.Profile
node f p = p {Types.node = f (Types.node p)}

-- TODO: Validate with shutdownOnBlock
shutdownOnSlot :: Int -> Types.Profile -> Types.Profile
shutdownOnSlot slot = node (\n -> n {Types.shutdown_on_slot_synced = Just slot})

-- TODO: Validate with shutdownOnSlot
shutdownOnBlock :: Int -> Types.Profile -> Types.Profile
shutdownOnBlock block = node (\n -> n {Types.shutdown_on_block_synced = Just block})

shutdownOnOff :: Types.Profile -> Types.Profile
shutdownOnOff = node (\n -> n {
  Types.shutdown_on_slot_synced = Nothing
, Types.shutdown_on_block_synced = Nothing
})

p2pOn :: Types.Profile -> Types.Profile
p2pOn = node (\n -> n {Types.verbatim = Types.NodeVerbatim (Just True)})

p2pOff :: Types.Profile -> Types.Profile
p2pOff = node (\n -> n {Types.verbatim = Types.NodeVerbatim Nothing})

tracerOn :: Types.Profile -> Types.Profile
tracerOn = node (\n -> n {Types.nodeTracer = True})

tracerOff :: Types.Profile -> Types.Profile
tracerOff = node (\n -> n {Types.nodeTracer = False})

newTracing :: Types.Profile -> Types.Profile
newTracing = node (\n -> n {Types.tracing_backend = "trace-dispatcher"})

oldTracing :: Types.Profile -> Types.Profile
oldTracing = node (\n -> n {Types.tracing_backend = "iohk-monitoring"})

rtsAppend :: String -> Types.Profile -> Types.Profile
rtsAppend str = node (\n -> n {Types.rts_flags_override = (Types.rts_flags_override n) ++ [str]})

rtsGcNonMoving :: Types.Profile -> Types.Profile
rtsGcNonMoving = rtsAppend "-xn"

rtsGcAllocSize :: Int -> Types.Profile -> Types.Profile
rtsGcAllocSize size = rtsAppend $ "-A" ++ (show size) ++ "m"

rtsThreads :: Int -> Types.Profile -> Types.Profile
rtsThreads n = rtsAppend $ "-N" ++ (show n)

--------------------------------------------------------------------------------

tracer :: (Types.Tracer -> Types.Tracer) -> Types.Profile -> Types.Profile
tracer f p = p {Types.tracer = f (Types.tracer p)}

tracerRtview :: Types.Profile -> Types.Profile
tracerRtview = tracer (\t -> t {Types.rtview = True})

tracerWithresources :: Types.Profile -> Types.Profile
tracerWithresources = tracer (\t -> t {Types.withresources = True})

--------------------------------------------------------------------------------

generator :: (Types.Generator -> Types.Generator) -> Types.Profile -> Types.Profile
generator f p = p {Types.generator = f (Types.generator p)}

generatorTps :: Scientific.Scientific -> Types.Profile -> Types.Profile
generatorTps tps = generator (\g -> g {Types.tps = tps})

--------------------------------------------------------------------------------

analysis :: (Types.Analysis -> Types.Analysis) -> Types.Profile -> Types.Profile
analysis f p = p {Types.analysis = f (Types.analysis p)}

analysisOff :: Types.Profile -> Types.Profile
analysisOff = analysis (\a -> a {Types.analysisType = Nothing})

analysisStandard :: Types.Profile -> Types.Profile
analysisStandard = analysis (\a -> a {Types.analysisType = Just "standard"})

analysisPerformance :: Types.Profile -> Types.Profile
analysisPerformance = analysis (\a -> a {Types.analysisType = Just "performance"})

analysisFiltersAppend :: String -> Types.Profile -> Types.Profile
analysisFiltersAppend str = analysis (\a -> a {Types.filters = (Types.filters a) ++ [str]})

analysisSizeSmall :: Types.Profile -> Types.Profile
analysisSizeSmall = analysisFiltersAppend "size-small"

analysisSizeModerate :: Types.Profile -> Types.Profile
analysisSizeModerate = analysisFiltersAppend "size-moderate"

analysisSizeFull :: Types.Profile -> Types.Profile
analysisSizeFull = analysisFiltersAppend "size-full"

analysisUnitary :: Types.Profile -> Types.Profile
analysisUnitary = analysisFiltersAppend "unitary"

analysisEpoch3Plus :: Types.Profile -> Types.Profile
analysisEpoch3Plus = analysisFiltersAppend "epoch3+"

--------------------------------------------------------------------------------
