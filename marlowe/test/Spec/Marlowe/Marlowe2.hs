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
import           Language.Plutus.Contract hiding (Contract)
import           Language.Plutus.Contract.Test
import qualified Language.Plutus.Contract.Effects.OwnPubKey        as OwnPubKey
import qualified Wallet.Emulator.Wallet                            as EM
import           Language.PlutusTx.Lattice
import           Ledger
import           Ledger.Ada                 (adaValueOf)


tests :: TestTree
tests = testGroup "token account"
    [ {- checkPredicate @MarloweSchema @ContractError "Create a Marlowe Contract" marloweContract2
        (assertNoFailedTransactions
        /\ assertNotDone w1 "contract should not have any errors"
        -- /\ walletFundsChange w1 (Ada.adaValueOf (-1))
        )
        (  callEndpoint @"create" w1 (defaultMarloweParams, Close)
           >> handleBlockchainEvents w1 )
    ,  -}zeroCouponBondTest1
    ]

zeroCouponBondTest :: TestTree
zeroCouponBondTest = checkPredicate @MarloweSchema @MarloweError "ZCB" marloweContract2
    (assertNoFailedTransactions
    /\ assertDone w1 (const True) "contract should close"
    /\ walletFundsChange alice (adaValueOf (-850))
    /\ walletFundsChange bob (adaValueOf (850))
    ) $ do
    -- Init a contract
    let alicePk = PK $ (pubKeyHash $ walletPubKey alice)
        aliceAcc = AccountId 0 alicePk
        bobPk = PK $ (pubKeyHash $ walletPubKey bob)

    let params = defaultMarloweParams

    let zeroCouponBond = When [ Case
            (Deposit aliceAcc alicePk ada (Constant 850_000_000))
            (Pay aliceAcc (Party bobPk) ada (Constant 850_000_000)
                Close)] (Slot 100) Close
    callEndpoint @"create" alice (params, zeroCouponBond)
    notifySlot alice
    handleBlockchainEvents alice
    callEndpoint @"apply-inputs" alice (params, [IDeposit aliceAcc alicePk ada 850_000_000])
    notifySlot alice
    handleBlockchainEvents alice


zeroCouponBondTest1 :: TestTree
zeroCouponBondTest1 = checkPredicate @MarloweSchema @MarloweError "ZCB" marloweContract2
    (assertNoFailedTransactions
    /\ emulatorLog (const False) ""
    /\ assertDone w1 (const True) "contract should close"
    -- /\ assertDone w2 (const True) "contract should close"
    -- /\ walletFundsChange bob (adaValueOf (-150))
    -- /\ walletFundsChange alice (adaValueOf (-850))
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

    addBlocks 1
    notifySlot alice
    -- notifySlot bob
    handleBlockchainEvents alice
    -- handleBlockchainEvents bob
    notifyInterestingAddresses alice
    -- notifyInterestingAddresses bob
    addBlocks 1
    handleBlockchainEvents alice

    callEndpoint @"sub" bob (params)
    handleBlockchainEvents bob

    callEndpoint @"apply-inputs" alice (params, [IDeposit aliceAcc alicePk ada 850_000_000])

    handleBlockchainEvents alice
    addBlocks 1
    handleBlockchainEvents alice
    notifySlot alice
    notifySlot bob
    handleBlockchainEvents bob
    notifyInterestingAddresses alice
    notifyInterestingAddresses bob

    callEndpoint @"apply-inputs" bob (params, [IDeposit aliceAcc bobPk ada 1000_000_000])

    handleBlockchainEvents alice
    handleBlockchainEvents bob
    addBlocks 1
    notifySlot alice
    notifySlot bob
    handleBlockchainEvents alice
    handleBlockchainEvents bob
    notifyInterestingAddresses alice
    notifyInterestingAddresses bob


w1, w2 :: Wallet
w1 = Wallet 1
w2 = Wallet 2

alice, bob, carol :: Wallet
alice = Wallet 1
bob = Wallet 2
carol = Wallet 3
