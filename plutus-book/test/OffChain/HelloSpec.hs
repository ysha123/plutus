{-# LANGUAGE OverloadedStrings #-}

module OffChain.HelloSpec (spec) where

import Control.Monad (void)
import Data.Either (isRight)
import OffChain.Hello (hello)
import Test.Hspec
import Utils
import Wallet.Emulator

spec :: Spec
spec = describe "hello" $
  it "logs the expected message" $ do
    let res = getResult tr
    fst res `shouldSatisfy` isRight
    _emulatorLog (snd res) `shouldBe` [WalletInfo w "Hello from the wallet!"]
  where
    w :: Wallet
    w = Wallet 1

    tr :: Trace MockWallet ()
    tr = void $ walletAction w hello
