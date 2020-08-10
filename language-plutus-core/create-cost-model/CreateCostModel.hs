module Main where

import CostModelCreation
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy as BSL

{- See Note [Creation of the Cost Model]
-}
main :: IO ()
main = do
  model <- createCostModel
  BSL.writeFile "language-plutus-core/src/costModel.json" $ encodePretty' (defConfig {confCompare = \_ _ -> EQ}) model
