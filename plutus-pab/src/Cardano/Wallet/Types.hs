{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StrictData         #-}
{-# LANGUAGE TypeApplications   #-}

module Cardano.Wallet.Types where

import           Control.Monad.Freer.Error      (Error)
import           Control.Monad.Freer.Extras.Log (LogMsg)
import           Control.Monad.Freer.State      (State)
import           Data.Aeson                     (FromJSON, ToJSON)
import           Data.Text                      (Text)
import           Data.Text.Prettyprint.Doc      (Pretty (..), (<+>))
import           GHC.Generics                   (Generic)
import           Servant                        (ServerError (..))
import           Servant.Client                 (BaseUrl, ClientError)

import           Cardano.BM.Data.Tracer         (ToObject (..))
import           Cardano.BM.Data.Tracer.Extras  (Tagged (..), mkObjectStr)
import           Ledger                         (Address, TxOutRef, Value)
import           Plutus.PAB.Arbitrary           ()
import           Wallet.Effects                 (ChainIndexEffect, NodeClientEffect, WalletEffect)
import           Wallet.Emulator.Error          (WalletAPIError)
import           Wallet.Emulator.Wallet         (Wallet, WalletState)

type WalletEffects m = '[ WalletEffect
                        , NodeClientEffect
                        , ChainIndexEffect
                        , State WalletState
                        , LogMsg Text
                        , Error WalletAPIError
                        , Error ClientError
                        , Error ServerError
                        , m]


type NodeUrl = BaseUrl
type ChainIndexUrl = BaseUrl
type WalletId = Integer
type Port     = Int

data Amount =
    Amount
        { quantity :: Integer
        , unit     :: Text
        }
    deriving (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

data Coin =
    Coin
        { address :: Text
        , amount  :: Amount
        }
    deriving (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

newtype CoinSelectionRequest =
    CoinSelectionRequest
        { payments :: [Coin]
        }
    deriving (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

data CoinSelectionResponse =
    CoinSelectionResponse
        { inputs  :: [Coin]
        , outputs :: [Coin]
        }
    deriving (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

data Config =
    Config
        { baseUrl :: BaseUrl
        , wallet  :: Wallet
        }
    deriving (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

data WalletMsg = StartingWallet Port
               | ChainClientMsg Text
    deriving stock (Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

instance Pretty WalletMsg where
    pretty = \case
        StartingWallet port -> "Starting wallet server on port " <+> pretty port
        ChainClientMsg m    -> "Chain Client: " <+> pretty m

instance ToObject WalletMsg where
    toObject _ = \case
        StartingWallet port -> mkObjectStr "Starting wallet server" (Tagged @"port" port)
        ChainClientMsg m    -> mkObjectStr "Chain Client: " (Tagged @"msg" m)

data MockWalletMsg =
    CallWallets
    | CallValueAt
    | ValueAtResponse Address Value
    | CallSelectCoin WalletId Value
    | SelectCoinResult (Either WalletAPIError ([(TxOutRef, Value)], Value))
    | CallAllocateAddress

instance Pretty MockWalletMsg where
    pretty = \case
        CallWallets                    -> "wallets"
        CallValueAt                    -> "valueAt"
        ValueAtResponse addr vl        -> "valueAt" <+> pretty addr <> ":" <+> pretty vl
        CallSelectCoin walletID target -> "selectCoin" <+> pretty walletID <+> pretty target
        SelectCoinResult result        -> "selectCoin result:" <+> pretty result
        CallAllocateAddress            -> "allocateAddress"
