## WARNING:  keep in sync with 'profile-cache-key-input' below this one: vvv
##
def fmt_decimal_10_5($x):
  ($x / 100000 | tostring) + "00000";

def profile_cli_args($p):
{ common:
  { createStakedArgs:
    ([ "--testnet-magic",          $p.genesis.network_magic
     , "--supply",                 fmt_decimal_10_5($p.genesis.funds_balance)
     , "--gen-utxo-keys",          1
     , "--gen-genesis-keys",       $p.composition.n_bft_hosts
     , "--supply-delegated",       fmt_decimal_10_5($p.derived.supply_delegated)
     , "--gen-pools",              $p.composition.n_pools
     , "--gen-stake-delegs",       $p.derived.delegators_effective
     , "--num-stuffed-utxo",       fmt_decimal_10_5($p.derived.utxo_stuffed)
     ] +
     if $p.composition.dense_pool_density != 1
     then
     [ "--bulk-pool-cred-files",   $p.composition.n_dense_hosts
     , "--bulk-pools-per-file",    $p.composition.dense_pool_density ]
     else [] end)
  , createTestnetDataArgs:
    ([ "--testnet-magic",          $p.genesis.network_magic
     , "--total-supply",           fmt_decimal_10_5($p.genesis.funds_balance + $p.derived.supply_delegated)
     , "--utxo-keys",              1
     , "--genesis-keys",           $p.composition.n_bft_hosts
     , "--delegated-supply",       fmt_decimal_10_5($p.derived.supply_delegated)
     , "--pools",                  $p.composition.n_pools
     , "--stake-delegators",       $p.derived.delegators_effective
     , "--drep-keys",              $p.genesis.dreps
     , "--stuffed-utxo",           fmt_decimal_10_5($p.derived.utxo_stuffed)
     ])
  , pools:
    [ "--argjson"
    , "initialPoolCoin",           fmt_decimal_10_5($p.genesis.pool_coin)
    ]
  }
}
| .common * (.[$p.era] // {})
;

## Remove parts of profile that don't invalidate
## the cryptographic material in genesis.  Note the opportunistic approach.
##
## Note also, that the genesis cache entry itself must still be updated
## to match these parameters, hence the distinction between parameters:
##
## WARNING:  keep in sync with 'profile_cli_args' above ^^^
##
def profile_genesis_cache_key($p; $profile_file):

  ($p.genesis * $p.composition * $p.derived)
  |
  { network_magic

  , funds_balance
  , per_pool_balance
  , pool_coin

  , n_pools
  , n_bft_hosts
  , n_dense_hosts
  , dense_pool_density

  , delegators
  , utxo_stuffed
  , dreps

  } as $genesis_crypto_affecting_data

  | $genesis_crypto_affecting_data | to_entries
  | map(if .value == null
        then error("FATAL: undefined key \(.key) in profile \(.profile_file)")
        else null end)

  | $genesis_crypto_affecting_data
;

def profile_genesis_cache_entry_name($p; $params_hash):

if $p.preset == null
then [ "k\(.composition.n_pools)" ]
     +
     if .composition.dense_pool_density == 1 then []
     else
     [ "d\(.composition.dense_pool_density)" ] end
     +
     [ "\(.genesis.delegators / 1000)kD" ]
     +
     if .genesis.dreps != 0 then ["\(.genesis.dreps)Dr"] else [] end
     +
     [ "\(.derived.utxo_stuffed / 1000)kU"
     , "\($params_hash)" ]
else [ "preset"
     , $profile[0].preset ]
end
| join("-")
;
