ADR 2: PAB and indexing solution integration
============================================

Date: 2022-06-28

Author(s)
---------

koslambrou <konstantinos.lambrou@iohk.io>

Status
------

Draft

Context
-------

Let's start with the problematic example (copy-paste of the current `PubKey` contract in `plutus-use-cases`).

.. code-block:: haskell

  -- | Lock some funds in a 'PayToPubKey' contract, returning the output's address
  --   and a 'TxIn' transaction input that can spend it.
  pubKeyContract
      :: forall w s e.
      ( AsPubKeyError e
      )
      => PaymentPubKeyHash
      -> Value
      -> Contract w s e (TxOutRef, Maybe ChainIndexTxOut, TypedValidator PubKeyContract)
  pubKeyContract pk vl = mapError (review _PubKeyError   ) $ do
      -- Step 1
      let inst = typedValidator pk
          address = Scripts.validatorAddress inst
          tx = Constraints.mustPayToTheScript () vl
      ledgerTx <- mkTxConstraints (Constraints.typedValidatorLookups inst) tx
                 >>= submitUnbalancedTx . Constraints.adjustUnbalancedTx

      -- Step 2
      _ <- awaitTxConfirmed (getCardanoTxId ledgerTx)

      -- Step 3
      let refs = Map.keys
                 $ Map.filter ((==) address . txOutAddress)
                 $ getCardanoTxUnspentOutputsTx ledgerTx
      case refs of
          []                   -> throwing _ScriptOutputMissing pk
          [outRef] -> do
              -- Step 4
              ciTxOut <- unspentTxOutFromRef outRef
              pure (outRef, ciTxOut, inst)
          _                    -> throwing _MultipleScriptOutputs pk


Here's an outline of the contract's steps:

1. Creates a transaction and submits it to the node
2. Waits for transaction to be confirmed
3. Finds the first UTXO of that transaction (return type `TxOutRef`)
4. Queries the plutus-chain-index to get the `ChainIndexTxOut` out of that `TxOutRef`

The problem is that the `ciTxOut` variable in step 4 will almost always result in `Nothing`.

Why? Here’s some context.

The PAB listens to the local node and stores blockchain information in memory such as the status of transactions, the status of transaction outputs, the last synced slot, the current slot, etc., in a variable of type `BlockchainEnv`.
The `awaitTxConfirmed` is actually querying the state of `BlockchainEnv` and waits until the status of the transaction transitions to `Confirmed`.

Meanwhile, plutus-chain-index (our main indexing solution at the time of this writing) is also listening to incoming blocks from the local node and indexes them into a database.
The indexed data can be queried using the REST API interface.

This brings up the main issue: the PAB and plutus-chain-index each listen to the same source of information (a local Cardano node), but each index the information at different speeds.
For a dApp developer writing off-chain code using the Contract API, there is no abstraction for handling multiple sources of truth.

Currently, in the best case scenario (fully synced PAB and plutus-chain-index), plutus-chain-index will always trail behind the in-memory storage of the PAB by a few seconds.
Therefore, even in this scenario, querying the plutus-chain-index with `unspentTxOutFromRef` in the above contract has a high probability of returning `Nothing`.

Decision
--------

TBD

Alternatives
------------

Make sure all components are in sync
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

We can make sure that all indexing solutions are syncing at the same speed.
For example, if PAB syncs from the local node and arrives at slot 100, then it needs to wait for the chain-index to also arrive at slot 100.
Only then can it respond to a Contract request.

To be able to do that, we would need to modify the block listener of PAB and plutus-chain-index to only get new blocks once they are at the same slot.

Pros:

- No modification to the Contract API
- All the indexed information is consistent with each other

Cons:

- As fast as the slowest indexing solution
- Can not be feasible with external indexing solutions for which we can’t control the indexing speed (e.g. Blockfrost_ or Scrolls_)

Add indexing specific functions in the Contract API
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

In this scenario, we would need to split Contract API requests which interact with an external indexing solution to the ones that use the PAB.
Currently, we have `awaitTxConfirmed` which uses the indexed information in the PAB to wait for a transaction status to change to `Confirmed`.
On top of that, we can have `awaitTxIndexed` or `awaitTxOutIndexed` which will wait for the information to be indexed in the external indexing solution.

Pros:

- Limits design change on the PAB
- More control given to the user of the Contract API

Cons:

- Adds an undesired complexity to the Contract API
- We'll need to add a bunch of functions (e.g., `awaitMarconiTxConfirmed or `awaitScrollsTxConfirmed`) for each new indexing solution we want to support

Ultimately, this solution is not an option given the added complexity added to the Contract API.

Query functions should interact with a single source of truth
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

In this scenario, we make the design decision that the Contract API should only interact with a single indexing solution.
Thus, any blockchain information currently stored in the PAB should be moved to the indexing solution.
Also, combining indexing solutions would need to be integrated in the single indexing solution that’s connected to the PAB.

Pros:

- Simplest in design to implement (other than manual work to move code)
- No modification to the Contract API

Cons:

- PAB won't be able to integrate with external indexing solutions (e.g. Blockfrost_ or Scrolls_), but these external solutions would need to be integrated into the single source of truth indexing solution. Therefore, this solution reduces the PAB's flexibility.

Alter indexing query APIs to include valid slots information
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

In this scenario, we can instead embrace the fact that the indexing solution is not always in sync with the PAB.
We can alter the indexing solution query APIs (like `utxosAt` or `txOutFromRef`) to include some sort of `ValidSlotRange` information.
Therefore, the user would need to specify the `SlotRange` from which he expects the indexing solution to be synced.
If the currently indexed slot of the PAB and indexing solution is not up to what we need, we can either wait for it or throw an error.

Pros:

- Would work accross different indexers as long as we can get an indication as to the slot number they are synced up to (which is not unreasonable to assume)
- Limits the changes to the Contract API

Cons:

- Makes the query APIs more complicated, which ultimately result in a more complex contract
- Makes the contract slower if it has to wait for the indexing solution to be synced to the desired `SlotRange`

Implications
------------

TBD

Notes
-----

This problem manifested itself in the Github issue `#473 <https://github.com/input-output-hk/plutus-apps/issues/473>`_ and there was a temporary fix in the PR `#496 <https://github.com/input-output-hk/plutus-apps/pull/496>`_.
However, the proper solution to the issue would be the implementation of this ADR.

.. _Blockfrost: https://blockfrost.io
.. _Scrolls: https://github.com/txpipe/scrolls
