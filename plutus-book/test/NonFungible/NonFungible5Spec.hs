{-# LANGUAGE OverloadedStrings #-}

module NonFungible.NonFungible5Spec (spec) where

import Control.Monad (void)
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Either (isRight)
import Ledger
import qualified Ledger.Ada as A
import qualified Ledger.Value as V
import NonFungible.NonFungible5
import Test.Hspec
import Utils
import Wallet.Emulator

spec :: Spec
spec =
  describe "prank" $
    it "does not work" $
      fst (getResult tr) `shouldSatisfy` isRight
  where
    monaLisa, starryNight :: String
    monaLisa = "Mona Lisa"
    starryNight = "The Starry Night"

    tr :: Trace MockWallet ()
    tr = void $ do
      updateWallets
      void $ walletAction w1 start
      updateWallets
      void $ walletAction w1 $ forge monaLisa
      updateWallets
      void $ walletAction w2 $ prank $ Wallet 1
      updateWallets
      void $ walletAction w1 $ forge starryNight
      updateWallets
      assertOwnFundsEq w1 $
        A.toValue initialAda
          <> tokenValue monaLisa
          <> tokenValue starryNight
      assertOwnFundsEq w2 $ A.toValue initialAda

symbol :: CurrencySymbol
symbol = nonFungibleSymbol $ NonFungible {issuer = key1}

tokenValue :: String -> Value
tokenValue name = V.singleton symbol (V.TokenName $ C.pack name) 1
