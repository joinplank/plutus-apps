{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}

module Plutus.Blockfrost.Queries (
    getTipBlockfrost
    , getDatumBlockfrost
    , getValidatorBlockfrost
    , getTxOutBlockfrost
    , getUnspentTxOutBlockfrost
    , getIsUtxoBlockfrost
    , getUtxoAtAddressBlockfrost
    , getUnspentAtAddressBlockfrost
    , getTxoAtAddressBlockfrost
    , getUtxoSetWithCurrency
    , getTxFromTxIdBlockfrost
    , defaultGetUtxo
    , defaultGetList
    , defaultIsUtxo
    ) where

import Control.Concurrent.Async (mapConcurrently)
import Control.Monad.Except (throwError)
import Control.Monad.Freer.Extras.Pagination (PageQuery (..))
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value)
import Data.Functor ((<&>))
import Data.Map (Map (..), fromList)
import Data.Maybe (catMaybes)
import Data.Text (Text)

import Blockfrost.Client

import Plutus.Blockfrost.Types


-- ENDPOINTS

getTipBlockfrost :: MonadBlockfrost m => m Block
getTipBlockfrost = getLatestBlock

getDatumBlockfrost :: MonadBlockfrost m => DatumHash -> m Value
getDatumBlockfrost dHash = getScriptDatum dHash <&> _scriptDatumJsonValue

getValidatorBlockfrost :: MonadBlockfrost m => ScriptHash -> m ScriptCBOR
getValidatorBlockfrost = getScriptCBOR

getTxOutBlockfrost :: (TxHash, Integer) -> BlockfrostClient UtxoOutput
getTxOutBlockfrost (tHash, idx) = do
    txos <- getTxUtxos tHash <&> _transactionUtxosOutputs
    case filterByIndex txos of
        []  -> throwError BlockfrostNotFound
        [x] -> pure x
        _   -> throwError $ BlockfrostError "Multiple UTxOs with the same index found!!!"
  where
    filterByIndex :: [UtxoOutput] -> [UtxoOutput]
    filterByIndex = filter ((==) idx . _utxoOutputOutputIndex)


getUnspentTxOutBlockfrost :: (TxHash, Integer) -> BlockfrostClient UtxoOutput
getUnspentTxOutBlockfrost ref = do
    txo <- getTxOutBlockfrost ref
    isUtxo <- checkIsUtxo ref
    if isUtxo then pure txo else throwError BlockfrostNotFound

getIsUtxoBlockfrost :: MonadBlockfrost m => (TxHash, Integer) -> m (Block, Bool)
getIsUtxoBlockfrost ref = do
    tip <- getTipBlockfrost
    isUtxo <- checkIsUtxo ref
    return (tip, isUtxo)

-- TODO: Pagination Support
getUtxoAtAddressBlockfrost :: MonadBlockfrost m => PageQuery a -> Address -> m (Block, [AddressUtxo])
getUtxoAtAddressBlockfrost _ addr = do
    tip <- getTipBlockfrost
    utxos <- getAddressUtxos' addr (paged 100 1) def
    return (tip, utxos)

-- TODO: Pagination Support
getUnspentAtAddressBlockfrost :: MonadBlockfrost m => PageQuery a -> Address -> m [AddressUtxo]
getUnspentAtAddressBlockfrost _ addr = getAddressUtxos' addr (paged 100 1) def

-- TODO: Pagination Support
getTxoAtAddressBlockfrost :: MonadBlockfrost m => PageQuery a -> Address -> m [UtxoInput]
getTxoAtAddressBlockfrost _ a = do
    addTxs <- getAddressTransactions a
    txUtxos <- liftIO $ mapConcurrently (getTxUtxos . _addressTransactionTxHash) addTxs
    let txos = concat $ map _transactionUtxosInputs txUtxos
    return $ take 100 $ filter ((==) a . _utxoInputAddress) txos


-- TODO: Pagination Support
getUtxoSetWithCurrency :: MonadBlockfrost m => PageQuery a -> AssetId -> m (Block, [AddressUtxo])
getUtxoSetWithCurrency _ assetId = do
    tip <- getTipBlockfrost
    xs <- getAssetAddresses assetId
    utxos <- liftIO $ mapConcurrently (flip getAddressUtxosAsset assetId . _assetAddressAddress) xs
    let retUtxos = (take 100 . concat) utxos
    return (tip, retUtxos)


getTxFromTxIdBlockfrost :: MonadBlockfrost m => TxHash -> m TxResponse
getTxFromTxIdBlockfrost txHash = do
    specificTx <- getTx txHash
    txUtxos <- getTxUtxos txHash
    datumMap <- getAllTxDatums txUtxos
    redeemers <- getTxRedeemers txHash
    liftIO $ print $ "INPUTS: " ++ show (_transactionUtxosInputs txUtxos)
    let scriptHashes = map _transactionRedeemerScriptHash redeemers
        redeemerHashes = map _transactionRedeemerDatumHash redeemers
    redeemersList <- liftIO $ mapConcurrently (\hash -> (unDatumHash hash,) <$> getScriptDatum hash) redeemerHashes
    scriptsList <- liftIO $ mapConcurrently (\hash -> (unScriptHash hash,) <$> getScriptCBOR hash) scriptHashes
    return $ TxResponse { _txHash        = txHash
                        , _invalidBefore = _transactionInvalidBefore specificTx
                        , _invalidAfter  = _transactionInvalidHereafter specificTx
                        , _utxosInputs   = _transactionUtxosInputs txUtxos
                        , _utxosOutpus   = _transactionUtxosOutputs txUtxos
                        , _datumsMap     = datumMap
                        , _redeemersMap  = fromList redeemersList
                        , _scriptsMap    = fromList scriptsList
                        , _redeemers     = redeemers
                        }

-- UTIL FUNCTIONS

getAllTxDatums :: MonadBlockfrost m => TransactionUtxos -> m (Map Text ScriptDatum)
getAllTxDatums utxos = do
    let inputs = map _utxoInputDataHash (_transactionUtxosInputs utxos)
        outputs = map _utxoOutputDataHash (_transactionUtxosOutputs utxos)
        datumHashes = catMaybes (inputs ++ outputs)
    datumMap <- liftIO $ mapConcurrently (\hash -> (unDatumHash hash,) <$> getScriptDatum hash) datumHashes
    return $ fromList datumMap

getAddressFromReference :: MonadBlockfrost m => (TxHash, Integer) -> m (Maybe Address)
getAddressFromReference (tHash, idx) = getTxUtxos tHash <&> (getAddress . _transactionUtxosOutputs)
  where
    getAddress :: [UtxoOutput] -> Maybe Address
    getAddress outs = case filter ((==) idx . _utxoOutputOutputIndex) outs of
        [out] -> Just $ _utxoOutputAddress out
        _     -> Nothing

-- TODO: Support addresses with more than 100 utxos
checkIsUtxo :: MonadBlockfrost m => (TxHash, Integer) -> m Bool
checkIsUtxo ref@(tHash, idx) = getAddressFromReference ref >>= maybe (pure []) getAddressUtxos <&> any matchUtxo
  where
    matchUtxo :: AddressUtxo -> Bool
    matchUtxo AddressUtxo{..} = (tHash == _addressUtxoTxHash) && (idx == _addressUtxoOutputIndex)

-- DEFAULT RESPONSES

defaultGetUtxo :: MonadBlockfrost m => m (Block, [AddressUtxo])
defaultGetUtxo = do
    tip <- getTipBlockfrost
    return (tip, [])

defaultGetList :: MonadBlockfrost m => m [a]
defaultGetList = return []

defaultIsUtxo :: MonadBlockfrost m => m (Block, Bool)
defaultIsUtxo = do
    tip <- getTipBlockfrost
    return(tip, False)
