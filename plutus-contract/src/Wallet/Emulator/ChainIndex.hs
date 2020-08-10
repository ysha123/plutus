{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Wallet.Emulator.ChainIndex where

import Control.Lens
import Control.Monad.Freer
import Control.Monad.Freer.State
import Control.Monad.Freer.TH
import Control.Monad.Freer.Writer
import Data.Aeson (FromJSON, ToJSON)
import Data.Foldable (traverse_)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Semigroup (Max (..))
import Data.Semigroup.Generic (GenericSemigroupMonoid (..))
import Data.Text.Prettyprint.Doc
import GHC.Generics (Generic)
import Ledger.Address (Address)
import Ledger.AddressMap (AddressMap)
import qualified Ledger.AddressMap as AM
import Ledger.Blockchain (Blockchain)
import Ledger.Slot (Slot)
import Ledger.Tx (txId)
import Ledger.TxId (TxId)
import Wallet.Effects
  ( AddressChangeRequest (..),
    AddressChangeResponse (..),
    ChainIndexEffect (..),
  )
import Wallet.Emulator.ChainIndex.Index (ChainIndex, ChainIndexItem (..))
import qualified Wallet.Emulator.ChainIndex.Index as Index
import Wallet.Emulator.NodeClient (ChainClientNotification (..))

data ChainIndexControlEffect r where
  ChainIndexNotify :: ChainClientNotification -> ChainIndexControlEffect ()

makeEffect ''ChainIndexControlEffect

data ChainIndexEvent
  = AddressStartWatching Address
  | ReceiveBlockNotification Int
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance Pretty ChainIndexEvent where
  pretty (AddressStartWatching addr) = "StartWatching:" <+> pretty addr
  pretty (ReceiveBlockNotification i) = "ReceiveBlockNotification:" <+> pretty i <+> " transactions."

data ChainIndexState = ChainIndexState
  { -- | Utxo set annotated with datums
    _idxWatchedAddresses :: AddressMap,
    -- | Transactions with confirmation date
    _idxConfirmedTransactions :: Map TxId Slot,
    -- | The blockchain
    _idxConfirmedBlocks :: Blockchain,
    -- | The current slot
    _idxCurrentSlot :: Maybe (Max Slot),
    -- | Transactions indexed by time and address
    _idxIdx :: ChainIndex
  }
  deriving stock (Eq, Show, Generic)
  deriving (Semigroup, Monoid) via (GenericSemigroupMonoid ChainIndexState)

makeLenses ''ChainIndexState

type ChainIndexEffs = '[State ChainIndexState, Writer [ChainIndexEvent]]

handleChainIndexControl ::
  (Members ChainIndexEffs effs) =>
  Eff (ChainIndexControlEffect ': effs) ~> Eff effs
handleChainIndexControl = interpret $ \case
  ChainIndexNotify (SlotChanged sl) -> modify (idxCurrentSlot .~ Just (Max sl))
  ChainIndexNotify (BlockValidated txns) -> do
    tell [ReceiveBlockNotification (length txns)]
    modify (idxConfirmedBlocks <>~ pure txns)
    (cs, addressMap) <- (,) <$> gets _idxCurrentSlot <*> gets _idxWatchedAddresses
    let currentSlot = maybe 0 getMax cs
    flip traverse_ txns $ \txn -> do
      let i = txId txn
          itm = ChainIndexItem {ciSlot = currentSlot, ciTx = txn, ciTxId = i}
      modify $ \s ->
        s & idxWatchedAddresses %~ AM.updateAllAddresses txn
          & idxConfirmedTransactions %~ Map.insert i currentSlot
          & idxIdx %~ Index.insert addressMap itm

handleChainIndex ::
  (Members ChainIndexEffs effs) =>
  Eff (ChainIndexEffect ': effs) ~> Eff effs
handleChainIndex = interpret $ \case
  StartWatching addr ->
    tell [AddressStartWatching addr]
      >> ( modify $ \s ->
             s & idxWatchedAddresses %~ AM.addAddress addr
         )
  WatchedAddresses -> gets _idxWatchedAddresses
  ConfirmedBlocks -> gets _idxConfirmedBlocks
  TransactionConfirmed txid ->
    Map.member txid <$> gets _idxConfirmedTransactions
  NextTx AddressChangeRequest {acreqSlot, acreqAddress} -> do
    idx <- gets _idxIdx
    let txns = Index.transactionsAt idx acreqSlot acreqAddress
    pure $ AddressChangeResponse {acrAddress = acreqAddress, acrSlot = acreqSlot, acrTxns = txns}
