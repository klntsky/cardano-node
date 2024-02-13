{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

--------------------------------------------------------------------------------

module Cardano.Benchmarking.Profile (
    Types.Profile (Profile)
  , name, desc
  , loopback, nomadPerf
  , uniCircle, torus, torusDense
  , hosts, pools, chainsync, withExplorer
  , withChaindbServer
  , idle, fixedLoaded
  , shutdownOnSlot, shutdownOnBlock, shutdownOnOff
  , p2pOn, p2pOff
  , tracerOn, tracerOff
  , newTracing, oldTracing
  , rtsGcNonMoving, rtsGcAllocSize, rtsThreads
  , tracerRtview, tracerWithresources
) where

import           Prelude hiding (id)
--import           Data.Function  ((&))
--import           Data.List      (tails, sortOn, uncons)
--import           Data.Maybe     (isJust)

import qualified Cardano.Benchmarking.Profile.Types as Types

--------------------------------------------------------------------------------

name :: String -> Types.Profile -> Types.Profile
name str = \p -> p {Types.name = str}

desc :: String -> Types.Profile -> Types.Profile
desc str = \p -> p {Types.desc = Just str}

--------------------------------------------------------------------------------

comp :: (Types.Composition -> Types.Composition) -> Types.Profile -> Types.Profile
comp f p = p {Types.composition = f (Types.composition p)}

loopback :: Types.Profile -> Types.Profile
loopback = comp (\c -> c {
  Types.locations = [Types.Loopback]
})

nomadPerf :: Types.Profile -> Types.Profile
nomadPerf = comp (\c -> c {
  Types.locations = [
    Types.AWS Types.EU_CENTRAL_1
  , Types.AWS Types.US_EAST_1
  , Types.AWS Types.AP_SOUTHEAST_2
  ]
})

uniCircle :: Types.Profile -> Types.Profile
uniCircle = comp (\c -> c {Types.topology = Types.UniCircle})

torus :: Types.Profile -> Types.Profile
torus = comp (\c -> c {Types.topology = Types.Torus})

torusDense :: Types.Profile -> Types.Profile
torusDense = comp (\c -> c {Types.topology = Types.Torus})

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

chainsync :: Int -> Types.Profile -> Types.Profile
chainsync size = comp (\c -> c {
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

withExplorer :: Types.Profile -> Types.Profile
withExplorer = comp (\c -> c {Types.with_explorer = True})

withChaindbServer :: Types.Profile -> Types.Profile
withChaindbServer = comp (\c -> c {Types.with_chaindb_server = Just True})

--------------------------------------------------------------------------------

idle :: Types.Profile -> Types.Profile
idle p = p {Types.scenario = Types.Idle}

fixedLoaded :: Types.Profile -> Types.Profile
fixedLoaded p = p {Types.scenario = Types.FixedLoaded}

--------------------------------------------------------------------------------

node :: (Types.Node -> Types.Node) -> Types.Profile -> Types.Profile
node f p = p {Types.node = f (Types.node p)}

shutdownOnSlot :: Int -> Types.Profile -> Types.Profile
shutdownOnSlot slot = node (\n -> n {Types.shutdown_on_slot_synced = Just slot})

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
