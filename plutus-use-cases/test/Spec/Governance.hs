{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE MonoLocalBinds      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
module Spec.Governance(tests, doVoting) where

import Control.Lens (view, (&))
import Control.Monad (void)
import Data.Foldable (traverse_)
import Data.Maybe (listToMaybe)

import Cardano.Node.Emulator.Params qualified as Params
import Cardano.Node.Emulator.TimeSlot qualified as TimeSlot
import Ledger qualified
import Ledger.Typed.Scripts qualified as Scripts
import Wallet.Emulator qualified as EM

import Plutus.Contract.Test
import Plutus.Contracts.Governance qualified as Gov
import Plutus.Script.Utils.Value (TokenName)
import Plutus.Trace.Emulator (EmulatorTrace)
import Plutus.Trace.Emulator qualified as Trace

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit qualified as HUnit

validatorAddress :: Ledger.CardanoAddress
validatorAddress
  = Scripts.validatorCardanoAddress Params.testnet
  $ Gov.typedValidator params

tests :: TestTree
tests =
    testGroup "governance tests"
    [ checkPredicateOptions (defaultCheckOptions & increaseTransactionLimits) "vote all in favor, 2 rounds - SUCCESS"
        (assertNoFailedTransactions
        .&&. dataAtAddress validatorAddress (maybe False ((== lawv3) . Gov.law) . listToMaybe))
        (doVoting 10 0 2)

    , checkPredicateOptions (defaultCheckOptions & increaseTransactionLimits) "vote 60/40, accepted - SUCCESS"
        (assertNoFailedTransactions
        .&&. dataAtAddress validatorAddress (maybe False ((== lawv2) . Gov.law) . listToMaybe))
        (doVoting 6 4 1)

    , checkPredicateOptions (defaultCheckOptions & increaseTransactionLimits) "vote 50/50, rejected - SUCCESS"
        (assertNoFailedTransactions
        .&&. dataAtAddress validatorAddress (maybe False ((== lawv1) . Gov.law) . listToMaybe ))
        (doVoting 5 5 1)

    -- TODO: turn this on again when reproducibility issue in core is fixed
    -- , goldenPir "test/Spec/governance.pir" $$(PlutusTx.compile [|| Gov.mkValidator ||])

    , HUnit.testCase "script size is reasonable"
                     ( reasonable (Scripts.validatorScript $ Gov.typedValidator params)
                                  23000
                     )
    ]

numberOfHolders :: Integer
numberOfHolders = 10

baseName :: TokenName
baseName = "TestLawToken"

-- | A governance contract that requires 6 votes out of 10
params :: Gov.Params
params = Gov.Params
    { Gov.initialHolders = Ledger.toPlutusAddress . EM.mockWalletAddress . knownWallet <$> [1..numberOfHolders]
    , Gov.requiredVotes = 6
    , Gov.baseTokenName = baseName
    }

lawv1, lawv2, lawv3 :: Gov.Law
lawv1 = Gov.Law "Law v1"
lawv2 = Gov.Law "Law v2"
lawv3 = Gov.Law "Law v3"

doVoting :: Int -> Int -> Integer -> EmulatorTrace ()
doVoting ayes nays rounds = do
    let activate wId = (Ledger.toPlutusAddress $ mockWalletAddress w, Gov.mkTokenName baseName wId,)
                 <$> Trace.activateContractWallet w (Gov.contract @Gov.GovError params)
           where
               w = knownWallet wId
    namesAndHandles <- traverse activate [1..numberOfHolders]
    let handle1 = (\(_,_,h) -> h) (head namesAndHandles)
    let token2 = (\(_,t,_) -> t) (namesAndHandles !! 1)
    let owner = Ledger.toPlutusAddress $ mockWalletAddress w2
    void $ Trace.callEndpoint @"new-law" handle1 lawv1
    void $ Trace.waitNSlots 10
    slotCfg <- Trace.getSlotConfig
    let votingRound (_, law) = do
            now <- view Trace.chainCurrentSlot <$> Trace.chainState
            void $ Trace.activateContractWallet w2
                (Gov.proposalContract @Gov.GovError params owner
                    Gov.Proposal { Gov.newLaw = law
                                 , Gov.votingDeadline = TimeSlot.slotToEndPOSIXTime slotCfg $ now + 20
                                 , Gov.tokenName = token2
                                 })
            void $ Trace.waitNSlots 1
            traverse_ (\(ow, nm, hdl) -> Trace.callEndpoint @"add-vote" hdl (ow, nm, True)  >> Trace.waitNSlots 1)
                      (take ayes namesAndHandles)
            traverse_ (\(ow, nm, hdl) -> Trace.callEndpoint @"add-vote" hdl (ow, nm, False) >> Trace.waitNSlots 1)
                      (take nays $ drop ayes namesAndHandles)
            Trace.waitNSlots 15

    traverse_ votingRound (zip [1..rounds] (cycle [lawv2, lawv3]))
