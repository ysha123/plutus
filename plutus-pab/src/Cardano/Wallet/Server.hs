{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeApplications      #-}

module Cardano.Wallet.Server
    ( main
    , Config(..)
    ) where

import           Control.Monad                   ((>=>))
import           Control.Monad.Freer             (runM)
import           Control.Monad.Freer.Error       (handleError)
import           Control.Monad.Freer.Extras.Log  (logInfo)
import           Control.Monad.IO.Class          (liftIO)
import           Data.Function                   ((&))
import           Data.Proxy                      (Proxy (Proxy))
import           Network.HTTP.Client             (defaultManagerSettings, newManager)
import qualified Network.Wai.Handler.Warp        as Warp
import           Servant                         (Application, NoContent (..), hoistServer, serve, (:<|>) ((:<|>)))
import           Servant.Client                  (BaseUrl (baseUrlPort), ClientEnv, ClientError, mkClientEnv)


import           Cardano.BM.Data.Trace           (Trace)
import qualified Cardano.ChainIndex.Client       as ChainIndexClient
import           Cardano.Wallet.API              (API)
import           Cardano.Wallet.Mock
import           Cardano.Wallet.Types            (ChainIndexUrl, Config (..), NodeUrl, WalletMsg (..))
import           Control.Concurrent.Availability (Availability, available)
import           Control.Concurrent.MVar         (MVar, newMVar)
import           Plutus.PAB.Arbitrary            ()
import           Plutus.PAB.Monitoring           (runLogEffects)
import           Wallet.Effects                  (ownOutputs, ownPubKey, startWatching, submitTxn,
                                                  updatePaymentWithChange, walletSlot)
import           Wallet.Emulator.Wallet          (WalletState, emptyWalletState)
import qualified Wallet.Emulator.Wallet          as Wallet




app :: Trace IO WalletMsg -> ClientEnv -> ClientEnv -> MVar WalletState -> Application
app trace nodeClientEnv chainIndexEnv mVarState =
    serve (Proxy @API) $
    hoistServer
        (Proxy @API)
        (asHandler trace nodeClientEnv chainIndexEnv mVarState)
         ((submitTxn >=> const (pure NoContent)) :<|> ownPubKey :<|> uncurry updatePaymentWithChange :<|>
          walletSlot :<|> ownOutputs)

main :: Trace IO WalletMsg -> Config -> NodeUrl -> ChainIndexUrl -> Availability -> IO ()
main trace Config {..} nodeBaseUrl chainIndexBaseUrl availability = runLogEffects trace $ do
    nodeClientEnv <- buildEnv nodeBaseUrl defaultManagerSettings
    chainIndexEnv <- buildEnv chainIndexBaseUrl defaultManagerSettings
    mVarState <- liftIO $ newMVar state
    runClient chainIndexEnv
    logInfo $ StartingWallet servicePort
    liftIO $ Warp.runSettings warpSettings $ app trace nodeClientEnv chainIndexEnv mVarState
    where
        servicePort = baseUrlPort baseUrl
        state = emptyWalletState wallet
        warpSettings = Warp.defaultSettings & Warp.setPort servicePort & Warp.setBeforeMainLoop (available availability)

        buildEnv url settings = liftIO
            $ newManager settings >>= \mgr -> pure $ mkClientEnv mgr url

        runClient env = liftIO
             $ runM
             $ flip handleError (error . show @ClientError)
             $ ChainIndexClient.handleChainIndexClient env
             $ startWatching (Wallet.ownAddress state)
