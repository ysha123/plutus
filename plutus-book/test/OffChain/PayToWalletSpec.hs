module OffChain.PayToWalletSpec (spec) where

import Control.Monad (void)
import Data.Either (isRight)
import qualified Language.PlutusTx.Numeric as P
import Ledger
import Ledger.Ada
import qualified OffChain.PayToWallet as P1
import qualified OffChain.PayToWalletSimple as P2
import Test.Hspec
import Utils
import Wallet.Emulator

spec :: Spec
spec = do
  describe "payToWallet (version 1)" $ mkSpec P1.myPayToWallet
  describe "payToWallet (version 2)" $ mkSpec P2.myPayToWallet

mkSpec :: (Wallet -> Ada -> MockWallet ()) -> SpecWith ()
mkSpec payToWallet =
  it "transfers funds as expected" $
    fst (getResult tr) `shouldSatisfy` isRight
  where
    ada :: Ada
    ada = lovelaceOf 8

    tr :: Trace MockWallet ()
    tr = void $ do
      updateWallets
      void $ walletAction w1 $ payToWallet w2 ada
      updateWallets
      assertFunds2 (initialAda P.- ada) (initialAda P.+ ada)
