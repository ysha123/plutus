{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Wallet.Mock where

import           Control.Lens                   (view)
import qualified Control.Monad.Except           as MonadError
import           Control.Monad.Freer
import           Control.Monad.Freer.Error
import           Control.Monad.Freer.Extras.Log (LogMsg, logInfo)
import           Control.Monad.Freer.State      (runState)
import           Control.Monad.IO.Class         (MonadIO, liftIO)
import           Data.Bifunctor                 (Bifunctor (..))
import qualified Data.ByteString.Lazy           as BSL
import qualified Data.ByteString.Lazy.Char8     as BSL8
import qualified Data.ByteString.Lazy.Char8     as Char8
import           Data.Function                  ((&))
import qualified Data.Map                       as Map
import           Data.Text.Encoding             (encodeUtf8)
import           Servant                        (Handler (Handler), ServerError (..), err400, err401, err404)
import           Servant.Client                 (ClientEnv)
import           Test.QuickCheck                (arbitrary, generate)

import           Cardano.BM.Data.Trace          (Trace)
import qualified Cardano.ChainIndex.Client      as ChainIndexClient
import qualified Cardano.Node.Client            as NodeClient
import           Cardano.Wallet.Types           (MockWalletMsg (..), WalletEffects, WalletId, WalletMsg (..))
import           Control.Concurrent.MVar        (MVar, putMVar, takeMVar)
import           Control.Error
import           Language.Plutus.Contract.Trace (allWallets)
import           Ledger                         (Address, PubKey, TxOut (..), TxOutRef, TxOutTx (..), Value)
import           Ledger.AddressMap              (UtxoMap)
import qualified Ledger.AddressMap              as AddressMap
import           Plutus.PAB.Arbitrary           ()
import           Plutus.PAB.Monitoring          (convertLog, handleLogMsgTrace)
import           Servant                        (runHandler)
import           Servant.Server                 (err500)
import           Wallet.API                     (WalletAPIError (InsufficientFunds, OtherError, PrivateKeyNotFound))
import           Wallet.Effects                 (ChainIndexEffect)
import qualified Wallet.Effects                 as W
import           Wallet.Emulator.Wallet         (WalletState)
import qualified Wallet.Emulator.Wallet         as EM
import qualified Wallet.Emulator.Wallet         as Wallet


asHandler ::
    Trace IO WalletMsg
    -> ClientEnv
    -> ClientEnv
    -> MVar WalletState
    -> Eff (WalletEffects IO) a
    -> Handler a
asHandler trace nodeClientEnv chainIndexEnv mVarState action =
    Handler $ do
        oldState <- liftIO $ takeMVar mVarState
        result <- liftIO $ runWalletEffects trace nodeClientEnv chainIndexEnv oldState action
        case result of
            Left e -> do
                liftIO $ putMVar mVarState oldState
                MonadError.throwError $ err400 { errBody = Char8.pack (show e) }
            Right (result_, newState) -> do
                liftIO $ putMVar mVarState newState
                pure result_

runWalletEffects ::
     MonadIO m
    => Trace m WalletMsg
    -> ClientEnv
    -> ClientEnv
    -> WalletState
    -> Eff (WalletEffects m) a
    -> m (Either ServerError (a, WalletState))
runWalletEffects trace nodeClientEnv chainIndexEnv walletState action =
    Wallet.handleWallet action
    & NodeClient.handleNodeClientClient nodeClientEnv
    & ChainIndexClient.handleChainIndexClient chainIndexEnv
    & runState walletState
    & handleLogMsgTrace (toWalletMsg trace)
    & handleWalletApiErrors
    & handleClientErrors
    & runError
    & runM
        where
            handleWalletApiErrors = flip handleError (throwError . fromWalletAPIError)
            handleClientErrors = flip handleError (\e -> throwError $ err500 { errBody = Char8.pack (show e) })
            toWalletMsg = convertLog ChainClientMsg


wallets :: (Member (LogMsg MockWalletMsg) effs) => Eff effs [Wallet.Wallet]
wallets = do
    logInfo CallWallets
    pure allWallets

fromWalletAPIError :: WalletAPIError -> ServerError
fromWalletAPIError (InsufficientFunds text) =
    err401 {errBody = BSL.fromStrict $ encodeUtf8 text}
fromWalletAPIError e@(PrivateKeyNotFound _) =
    err404 {errBody = BSL8.pack $ show e}
fromWalletAPIError (OtherError text) =
    err500 {errBody = BSL.fromStrict $ encodeUtf8 text}

valueAt ::
    ( Member (LogMsg MockWalletMsg) effs
    , Member ChainIndexEffect effs
    )
    => Address
    -> Eff effs Value
valueAt address = do
    logInfo CallValueAt
    value <- foldMap (txOutValue . txOutTxOut) . view (AddressMap.fundsAt address) <$> W.watchedAddresses
    logInfo $ ValueAtResponse address value
    pure value

selectCoin ::
    ( Member (LogMsg MockWalletMsg) effs
    , Member ChainIndexEffect effs
    , Member (Error ServerError) effs
    )
    => WalletId
    -> Value
    -> Eff effs ([(TxOutRef, Value)], Value)
selectCoin walletId target = do
    logInfo $ CallSelectCoin walletId target
    let address = EM.walletAddress (Wallet.Wallet walletId)
    utxos :: UtxoMap <- view (AddressMap.fundsAt address) <$> W.watchedAddresses
    let funds :: [(TxOutRef, Value)]
        funds = fmap (second (txOutValue . txOutTxOut)) . Map.toList $ utxos
    result <- runM $ runError $ EM.selectCoin funds target
    logInfo $ SelectCoinResult result
    case result of
        Right value -> pure value
        Left e      -> throwError $ fromWalletAPIError e

allocateAddress ::
    ( LastMember m effs
    , Member (LogMsg MockWalletMsg) effs
    , MonadIO m
    )
    => WalletId
    -> Eff effs PubKey
allocateAddress _ = do
    logInfo CallAllocateAddress
    sendM $ liftIO $ generate arbitrary
