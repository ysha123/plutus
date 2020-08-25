{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE ExplicitForAll    #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns
-fno-warn-name-shadowing
-fno-warn-unused-do-bind
-fno-warn-unused-top-binds #-}
module Spec.Marlowe.Marlowe2
    ( tests
    )
where

import           Test.Tasty

import           Language.Marlowe.Semantics
import           Language.Marlowe.Util
import           Language.Marlowe.Client3
import           Language.Plutus.Contract.Test
import           Language.PlutusTx.Lattice
import           Ledger
import           Ledger.Ada                 (adaValueOf)


tests :: TestTree
tests = testGroup "token account"
    [ zeroCouponBondTest
    ]



zeroCouponBondTest :: TestTree
zeroCouponBondTest = checkPredicate @MarloweSchema @MarloweError "ZCB" marloweContract2
    (assertNoFailedTransactions
    -- /\ emulatorLog (const False) ""
    /\ assertDone alice (const True) "contract should close"
    /\ assertDone bob (const True) "contract should close"
    /\ walletFundsChange alice (adaValueOf (150))
    /\ walletFundsChange bob (adaValueOf (-150))
    ) $ do
    -- Init a contract
    let alicePk = PK $ (pubKeyHash $ walletPubKey alice)
        aliceAcc = AccountId 0 alicePk
        bobPk = PK $ (pubKeyHash $ walletPubKey bob)

    let params = defaultMarloweParams

    let zeroCouponBond = When [ Case
            (Deposit aliceAcc alicePk ada (Constant 850_000_000))
            (Pay aliceAcc (Party bobPk) ada (Constant 850_000_000)
                (When
                    [ Case (Deposit aliceAcc bobPk ada (Constant 1000_000_000)) Close] (Slot 200) Close
                ))] (Slot 100) Close
    callEndpoint @"create" alice (params, zeroCouponBond)
    handleBlockchainEvents alice
    addBlocks 1
    handleBlockchainEvents alice

    callEndpoint @"wait" bob (params)
    handleBlockchainEvents bob

    callEndpoint @"apply-inputs" alice (params, [IDeposit aliceAcc alicePk ada 850_000_000])
    handleBlockchainEvents alice
    addBlocks 1
    handleBlockchainEvents alice

    callEndpoint @"wait" alice (params)

    handleBlockchainEvents bob

    callEndpoint @"apply-inputs" bob (params, [IDeposit aliceAcc bobPk ada 1000_000_000])

    handleBlockchainEvents alice
    handleBlockchainEvents bob
    addBlocks 1
    handleBlockchainEvents alice
    handleBlockchainEvents bob


alice, bob, carol :: Wallet
alice = Wallet 1
bob = Wallet 2
carol = Wallet 3
