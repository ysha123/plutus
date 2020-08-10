{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Wallet.Emulator.Error where

import Control.Monad.Freer (Eff, Member)
import Control.Monad.Freer.Error (Error, throwError)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Text.Prettyprint.Doc
import GHC.Generics (Generic)
import IOTS (IotsType)
import Ledger (PubKeyHash)

-- | An error thrown by wallet interactions.
data WalletAPIError
  = -- | There were insufficient funds to perform the desired operation.
    InsufficientFunds Text
  | -- | The private key of this public key hahs is not known to the wallet.
    PrivateKeyNotFound PubKeyHash
  | -- | Some other error occurred.
    OtherError Text
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (IotsType)

instance Pretty WalletAPIError where
  pretty = \case
    InsufficientFunds t ->
      "Insufficient funds:" <+> pretty t
    PrivateKeyNotFound pk ->
      "Private key not found:" <+> viaShow pk
    OtherError t ->
      "Other error:" <+> pretty t

instance FromJSON WalletAPIError

instance ToJSON WalletAPIError

throwInsufficientFundsError :: Member (Error WalletAPIError) effs => Text -> Eff effs a
throwInsufficientFundsError = throwError . InsufficientFunds

throwOtherError :: Member (Error WalletAPIError) effs => Text -> Eff effs a
throwOtherError = throwError . OtherError
