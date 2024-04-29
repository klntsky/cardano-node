## Testable with:
##
##   jq -n 'include "prof0-defaults" { search: "nix/workbench/profiles" }; era_defaults("babbage")'
##
def era_defaults($era):
{ common:
  { era:                              $era

  ## Choice of a cluster run scenario (wb scenario --help):
  , scenario:                         "fixed-loaded"

  ## Cluster topology and composition:
  , composition:
    { locations:                      ["loopback"]
    , n_bft_hosts:                    0
    , n_singular_hosts:               5
    , n_dense_hosts:                  1
    , dense_pool_density:             1
    , with_proxy:                     false
    , with_explorer:                  false
    , topology:                       "uni-circle"
    }

  , genesis:
    ## Trivia
    { network_magic:                  42

    ## Incrementality
    , single_shot:                    true

    ## UTxO & delegation
    , per_pool_balance:               1000000000000000
    , funds_balance:                  10000000000000
    , utxo:                           0

    ## DReps
    , dreps:                          0

    ## Blockchain time & block density
    , active_slots_coeff:             0.05
    , epoch_length:                   600   # Ought to be at least (10 * k / f).
    , parameter_k:                    3
    , slot_duration:                  1
    , extra_future_offset:            0

    ## Protocol parameters
    , pparamsEpoch:                   300   # See: pparams/epoch-timeline.jq
    , pparamsOverlays:                []
    }

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

  , node:
    { rts_flags_override:             []
    , heap_limit:                     null                ## optional: heap limit in MB (translates to RTS flag -M)
    , shutdown_on_slot_synced:        null
    , shutdown_on_block_synced:       null
    , tracing_backend:                "trace-dispatcher"  ## or "iohk-monitoring"
    , tracer:                         true
    , utxo_lmdb:                      false               ## use LMDB backend (instead of default in-mem) on a UTxO-HD node; will be ignored by non-UTxO-HD nodes
    , verbatim:
      {
      }
    }

  , analysis:
    { type:                           "standard"
    , cluster_base_startup_overhead_s: 40
    , start_log_spread_s:             120
    , last_log_spread_s:              120
    , silence_since_last_block_s:     120
    , tx_loss_ratio:                  0.02
    , finish_patience:                21
    , filters:                        ["unitary"]
    }

  , tracer:
    { rtview:                         false
    , ekg:                            false
    , withresources:                  false   # enable resource tracing for cardano-tracer
    }

  , cluster:
    { nomad:
      { namespace: "default"
      , class: ""
        # As they will be used in the "group.*.resources" of the Nomad Job JSON.
      , resources:
        { producer: {cores: 2, memory: 15000, memory_max: 16000}
        , explorer: {cores: 2, memory: 15000, memory_max: 16000}
        }
      # Volumes like {source: "ssd1", destination: "/ssd1", read_only: false}
      , host_volumes: null
      , fetch_logs_ssh: false
      }
    , aws:
      { instance_type:
        { producer: "c5.2xlarge"
        , explorer: "m5.4xlarge"
        }
      # "attr.unique.platform.aws.public-ipv4" to bind and service definition.
      , use_public_routing: false
      }
    , minimun_storage:
      { producer: 12582912 # 12×1024×1024
      , explorer: 14155776 # 13.5×1024×1024
      }
    , keep_running: false
    , ssd_directory: null
    }
  }

} | (.common * (.[$era] // {}));
