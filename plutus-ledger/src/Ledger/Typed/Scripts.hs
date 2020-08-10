{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-specialise #-}

module Ledger.Typed.Scripts
  ( ScriptType (..),
    Validator,
    ScriptInstance,
    MonetaryPolicy,
    validator,
    scriptHash,
    scriptAddress,
    validatorScript,
    wrapValidator,
    wrapMonetaryPolicy,
    monetaryPolicy,
    monetaryPolicyHash,
    ValidatorType,
    WrappedValidatorType,
    WrappedMonetaryPolicyType,
    fromValidator,
    Any,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Kind
import GHC.Generics (Generic)
import qualified Language.PlutusCore as PLC
import Language.PlutusTx
import qualified Ledger.Address as Addr
import Ledger.Scripts hiding (monetaryPolicyHash)
import qualified Ledger.Scripts
import Ledger.Typed.Scripts.Validators

-- | A typed validator script with its 'ValidatorScript' and 'Address'.
data ScriptInstance (a :: Type) = Validator
  { instanceScript :: Validator,
    instanceHash :: ValidatorHash,
    -- | The hash of the monetary policy that checks whether the validator
    --   is run in this transaction
    instanceMPSHash :: MonetaryPolicyHash,
    instanceMPS :: MonetaryPolicy
  }
  deriving (Eq, Generic, ToJSON, FromJSON)

-- | The 'ScriptInstance' of a validator script and its wrapper.
validator ::
  -- | Validator script (compiled)
  CompiledCode PLC.DefaultUni (ValidatorType a) ->
  -- | A wrapper for the compiled validator
  CompiledCode PLC.DefaultUni (ValidatorType a -> WrappedValidatorType) ->
  ScriptInstance a
validator vc wrapper =
  let val = mkValidatorScript $ wrapper `applyCode` vc
      hsh = validatorHash val
      mps = mkMonetaryPolicy hsh
   in Validator
        { instanceScript = val,
          instanceHash = hsh,
          instanceMPS = mps,
          instanceMPSHash = Ledger.Scripts.monetaryPolicyHash mps
        }

-- | The script's 'ValidatorHash'
scriptHash :: ScriptInstance a -> ValidatorHash
scriptHash = instanceHash

-- | Get the address for a script instance.
scriptAddress :: ScriptInstance a -> Addr.Address
scriptAddress = Addr.scriptHashAddress . scriptHash

-- | Get the validator script for a script instance.
validatorScript :: ScriptInstance a -> Validator
validatorScript = instanceScript

-- | Script instance for a validator whose type is unknown
fromValidator :: Validator -> ScriptInstance Any
fromValidator vl =
  let vh = validatorHash vl
      mps = mkMonetaryPolicy vh
   in Validator
        { instanceScript = vl,
          instanceHash = vh,
          instanceMPS = mps,
          instanceMPSHash = Ledger.Scripts.monetaryPolicyHash mps
        }

-- | The monetary policy that forwards all checks to the instance's
--   validator
monetaryPolicy :: ScriptInstance a -> MonetaryPolicy
monetaryPolicy = instanceMPS

-- | Hash of the monetary policy that forwards all checks to the instance's
--   validator
monetaryPolicyHash :: ScriptInstance a -> MonetaryPolicyHash
monetaryPolicyHash = instanceMPSHash
