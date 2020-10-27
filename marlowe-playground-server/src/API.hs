{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE TypeOperators      #-}

module API where

import qualified Auth
import           Data.Aeson                                       (FromJSON, ToJSON, Value)
import           Data.Text                                        (Text)
import           GHC.Generics                                     (Generic)
import qualified Language.Marlowe.ACTUS.Definitions.ContractTerms as CT
import           Servant.API                                      ((:<|>), (:>), Capture, Get, Header, JSON, NoContent,
                                                                   PlainText, Post, Raw, ReqBody)

data OracleResponse = OracleResponse { price :: String }
  deriving (Generic, FromJSON, ToJSON)


type API
     = "oracle" :> Capture "exchange" String :> Capture "pair" String :> Get '[JSON] OracleResponse
       :<|> "version" :> Get '[ PlainText, JSON] Text
       :<|> "actus" :> ("generate" :> ReqBody '[ JSON] CT.ContractTerms :> Post '[ JSON] String
                        :<|> "generate-static" :> ReqBody '[ JSON] CT.ContractTerms :> Post '[ JSON] String)
