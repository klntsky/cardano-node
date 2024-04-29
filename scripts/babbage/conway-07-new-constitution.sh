#!/usr/bin/env bash

# This scripts uses set -x to show in terminal the commands executed by the script.
# The "exec 2>" below this comment helps the user to differenciate between the commands and its outputs by changing the color
# of the set -x output (the commands).

exec 2> >(while IFS= read -r line; do echo -e "\e[34m${line}\e[0m" >&2; done)

# Unofficial bash strict mode.
# See: http://redsymbol.net/articles/unofficial-bash-strict-mode/

set -euo pipefail

UNAME=$(uname -s) SED=
case $UNAME in
  Darwin )      SED="gsed";;
  Linux )       SED="sed";;
esac

sprocket() {
  if [ "$UNAME" == "Windows_NT" ]; then
    # Named pipes names on Windows must have the structure: "\\.\pipe\PipeName"
    # See https://docs.microsoft.com/en-us/windows/win32/ipc/pipe-names
    echo -n '\\.\pipe\'
    echo "$1" | sed 's|/|\\|g'
  else
    echo "$1"
  fi
}

set -x

CARDANO_CLI="${CARDANO_CLI:-cardano-cli}"
NETWORK_MAGIC=42
DREP_DIR=example/dreps
UTXO_DIR=example/utxo-keys
TRANSACTIONS_DIR=example/transactions
CC_DIR=example/cc

mkdir -p "$TRANSACTIONS_DIR"

# "QUERY GOVERNANCE STATE"

echo "DOWNLOAD A PROPOSAL FILE, THIS IS WHERE WE EXPLAIN WHY THIS PROPOSAL IS RELEVANT"

wget https://tinyurl.com/3wrwb2as -O "${TRANSACTIONS_DIR}/proposal.txt"

# "DOWNLOAD OUR SAMPLE CONSTITUTION FILE"

wget https://tinyurl.com/mr3ferf9 -O "${TRANSACTIONS_DIR}/constitution.txt"

echo "CALCULATE THE HASH OURSELVES"

hash=$(b2sum -l 256 ${TRANSACTIONS_DIR}/constitution.txt | cut -d' ' -f1)
echo "$hash"

# "QUERY THE CURRENT CONTSTITUTION HASH"

cardano-cli conway query constitution \
  --testnet-magic $NETWORK_MAGIC

# "CREATE A PROPOSAL TO UPDATE THE CONSTITUTION"
govActDeposit=$(cardano-cli conway query gov-state | jq .currentPParams.govActionDeposit)
constitutionHash="$(cardano-cli conway governance hash anchor-data --file-text ${TRANSACTIONS_DIR}/constitution.txt)"
proposalHash="$(cardano-cli conway governance hash anchor-data --file-text ${TRANSACTIONS_DIR}/proposal.txt)"


cardano-cli conway governance action create-constitution \
  --testnet \
  --governance-action-deposit "$govActDeposit" \
  --deposit-return-stake-verification-key-file "${UTXO_DIR}/stake1.vkey" \
  --anchor-url "https://tinyurl.com/3wrwb2as" \
  --anchor-data-hash "$proposalHash" \
  --constitution-url "https://tinyurl.com/2pahcy6z"  \
  --constitution-hash "$constitutionHash" \
  --out-file "${TRANSACTIONS_DIR}/constitution.action"

# "BUILD, SIGN AND SUBMIT THE CONSTITUTION"

cardano-cli conway transaction build \
  --testnet-magic $NETWORK_MAGIC \
  --tx-in "$(cardano-cli query utxo --address "$(cat "${UTXO_DIR}/payment1.addr")" --testnet-magic $NETWORK_MAGIC --out-file /dev/stdout | jq -r 'keys[0]')" \
  --change-address "$(cat ${UTXO_DIR}/payment1.addr)" \
  --proposal-file "${TRANSACTIONS_DIR}/constitution.action" \
  --witness-override 2 \
  --out-file "${TRANSACTIONS_DIR}/constitution-tx.raw"

cardano-cli conway transaction sign \
  --testnet-magic $NETWORK_MAGIC \
  --tx-body-file "${TRANSACTIONS_DIR}/constitution-tx.raw" \
  --signing-key-file "${UTXO_DIR}/payment1.skey" \
  --signing-key-file "${UTXO_DIR}/stake1.skey" \
  --out-file "${TRANSACTIONS_DIR}/constitution-tx.signed"

cardano-cli conway transaction submit \
  --testnet-magic $NETWORK_MAGIC \
  --tx-file "${TRANSACTIONS_DIR}/constitution-tx.signed"

sleep 5

ID="$(cardano-cli conway query gov-state --testnet-magic 42 | jq -r '.proposals[0].actionId.txId')"
IX="$(cardano-cli conway query gov-state --testnet-magic 42 | jq -r '.proposals[0].actionId.govActionIx')"

echo "VOTE AS DREPS AND AS SPO"

### ----------––––––––
# DREP VOTES
### ----------––––––––

for i in {1..3}; do
  cardano-cli conway governance vote create \
    --yes \
    --governance-action-tx-id "${ID}" \
    --governance-action-index "${IX}" \
    --drep-verification-key-file "${DREP_DIR}/drep${i}.vkey" \
    --out-file "${TRANSACTIONS_DIR}/${ID}-drep${i}.vote"
done

cardano-cli conway transaction build \
  --testnet-magic $NETWORK_MAGIC \
  --tx-in "$(cardano-cli query utxo --address "$(cat "${UTXO_DIR}/payment1.addr")" --testnet-magic $NETWORK_MAGIC --out-file /dev/stdout | jq -r 'keys[0]')" \
  --change-address "$(cat ${UTXO_DIR}/payment1.addr)" \
  --vote-file "${TRANSACTIONS_DIR}/${ID}-drep1.vote" \
  --vote-file "${TRANSACTIONS_DIR}/${ID}-drep2.vote" \
  --vote-file "${TRANSACTIONS_DIR}/${ID}-drep3.vote" \
  --witness-override 4 \
  --out-file "${TRANSACTIONS_DIR}/constitution-drep-votes-tx.raw"

cardano-cli conway transaction sign \
  --testnet-magic $NETWORK_MAGIC \
  --tx-body-file "${TRANSACTIONS_DIR}/constitution-drep-votes-tx.raw" \
  --signing-key-file "${UTXO_DIR}/payment1.skey" \
  --signing-key-file "${DREP_DIR}/drep1.skey" \
  --signing-key-file "${DREP_DIR}/drep2.skey" \
  --signing-key-file "${DREP_DIR}/drep3.skey" \
  --out-file "${TRANSACTIONS_DIR}/constitution-drep-votes-tx.signed"

cardano-cli conway transaction submit \
  --testnet-magic $NETWORK_MAGIC \
  --tx-file "${TRANSACTIONS_DIR}/constitution-drep-votes-tx.signed"

sleep 5


### ----------––––––––
# CC VOTES
### ----------––––––––

for i in {1..1}; do
  cardano-cli conway governance vote create \
    --yes \
    --governance-action-tx-id "${ID}" \
    --governance-action-index "${IX}" \
    --cc-hot-verification-key-file "${CC_DIR}/hot1-cc.vkey" \
    --out-file "${TRANSACTIONS_DIR}/cc${i}.vote"
done

# for i in {2..2}; do
#   cardano-cli conway governance vote create \
#     --no \
#     --governance-action-tx-id "${ID}" \
#     --governance-action-index "${IX}" \
#     --cc-hot-verification-key-file "${CC_DIR}/hot1-cc.vkey" \
#     --out-file "${TRANSACTIONS_DIR}/cc${i}.vote"
# done

cardano-cli conway transaction build \
  --testnet-magic $NETWORK_MAGIC \
  --tx-in "$(cardano-cli query utxo --address "$(cat "${UTXO_DIR}/payment1.addr")" --testnet-magic $NETWORK_MAGIC --out-file /dev/stdout | jq -r 'keys[0]')" \
  --change-address "$(cat ${UTXO_DIR}/payment1.addr)" \
  --vote-file "${TRANSACTIONS_DIR}/cc1.vote" \
  --witness-override 2 \
  --out-file "${TRANSACTIONS_DIR}/constitution-cc-votes-tx.raw"

cardano-cli conway transaction sign \
  --testnet-magic $NETWORK_MAGIC \
  --tx-body-file "${TRANSACTIONS_DIR}/constitution-cc-votes-tx.raw" \
  --signing-key-file "${UTXO_DIR}/payment1.skey" \
  --signing-key-file "${CC_DIR}/hot1-cc.skey" \
  --out-file "${TRANSACTIONS_DIR}/constitution-cc-votes-tx.signed"

cardano-cli conway transaction submit \
  --testnet-magic $NETWORK_MAGIC \
  --tx-file "${TRANSACTIONS_DIR}/constitution-cc-votes-tx.signed"

sleep 10

cardano-cli conway query gov-state | jq .proposals

# cardano-cli conway transaction build \
#   --testnet-magic $NETWORK_MAGIC \
#   --tx-in "$(cardano-cli query utxo --address "$(cat "${UTXO_DIR}/payment1.addr")" --testnet-magic $NETWORK_MAGIC --out-file /dev/stdout | jq -r 'keys[0]')" \
#   --change-address "$(cat ${UTXO_DIR}/payment1.addr)" \
#   --vote-file "${TRANSACTIONS_DIR}/cc2.vote" \
#   --witness-override 2 \
#   --out-file "${TRANSACTIONS_DIR}/constitution-cc-votes-tx.raw"

# cardano-cli conway transaction sign \
#   --testnet-magic $NETWORK_MAGIC \
#   --tx-body-file "${TRANSACTIONS_DIR}/constitution-cc-votes-tx.raw" \
#   --signing-key-file "${UTXO_DIR}/payment1.skey" \
#   --signing-key-file "${CC_DIR}/hot1-cc.skey" \
#   --out-file "${TRANSACTIONS_DIR}/constitution-cc-votes-tx.signed"

# cardano-cli conway transaction submit \
#   --testnet-magic $NETWORK_MAGIC \
#   --tx-file "${TRANSACTIONS_DIR}/constitution-cc-votes-tx.signed"

# sleep 10

