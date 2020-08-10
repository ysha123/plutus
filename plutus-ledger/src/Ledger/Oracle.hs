{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-strictness #-}

module Ledger.Oracle
  ( -- * Signed messages
    -- $oracles
    Observation (..),
    SignedMessage (..),

    -- * Checking signed messages
    SignedMessageCheckError (..),
    checkSignature,
    checkHashConstraints,
    checkHashOffChain,
    verifySignedMessageOffChain,
    verifySignedMessageOnChain,
    verifySignedMessageConstraints,

    -- * Signing messages
    signMessage,
    signObservation,
  )
where

import qualified Data.ByteString.Lazy as BSL
import GHC.Generics (Generic)
import Language.PlutusTx
import Language.PlutusTx.Prelude
import Ledger.Constraints (TxConstraints)
import qualified Ledger.Constraints as Constraints
import Ledger.Crypto (PrivateKey, PubKey (..), Signature (..))
import qualified Ledger.Crypto as Crypto
import Ledger.Scripts (Datum (..), DatumHash (..))
import qualified Ledger.Scripts as Scripts
import Ledger.Slot (Slot)
import Ledger.Validation (ValidatorCtx)
import LedgerBytes
import qualified Prelude as Haskell

-- $oracles
-- This module provides a way to verify signed messages, and a type for
--  observations (for example, the price of a commodity on a given date).
--  Together, the two can be used to implement trusted oracles:
--
--  * The oracle observes a value, for example 'Price' and constructs a value
--    @o = @ 'Observation' @Price@.
--  * The oracle then calls 'signMessage' @o pk@ with its own private key to
--    produce a 'SignedMessage' @(@'Observation' @Price)@.
--  * The signed message is passed to the contract as the redeemer of some
--    unspent output. __Important:__ The redeeming transaction must include the
--    message 'o' as a datum. This is because we can't hash anything in
--    on-chain code, and therefore have to rely on the node to do it for us
--    via the pending transaction's map of datum hashes to datums.
--    (The constraints resolution mechanism takes care of including the message)
--  * The contract then calls 'checkSignature' to check the signature, and
--    produces a constraint ensuring that the signed hash is really the hash
--    of the datum.

-- | A value that was observed at a specific point in time
data Observation a = Observation
  { -- | The value
    obsValue :: a,
    -- | The time at which the value was observed
    obsSlot :: Slot
  }
  deriving (Generic, Haskell.Show)

instance Eq a => Eq (Observation a) where
  l == r =
    obsValue l == obsValue r
      && obsSlot l == obsSlot r

-- | @SignedMessage a@ contains the signature of a hash of a 'Datum'.
--   The 'Datum' can be decoded to a value of type @a@.
data SignedMessage a = SignedMessage
  { -- | Signature of the message
    osmSignature :: Signature,
    -- | Hash of the message
    osmMessageHash :: DatumHash,
    osmDatum :: Datum
  }
  deriving (Generic, Haskell.Show)

data SignedMessageCheckError
  = -- | The signature did not match the public key
    SignatureMismatch Signature PubKey DatumHash
  | -- | The datum was missing from the pending transaction
    DatumMissing DatumHash
  | -- | The datum had the wrong shape
    DecodingError
  | -- | The datum that correponds to the hash is wrong
    DatumNotEqualToExpected
  deriving (Generic, Haskell.Show)

{-# INLINEABLE checkSignature #-}

-- | Verify the signature on a signed datum hash
checkSignature ::
  -- | The hash of the message
  DatumHash ->
  -- | The public key of the signatory
  PubKey ->
  -- | The signed message
  Signature ->
  Either SignedMessageCheckError ()
checkSignature datumHash pubKey signature_ =
  let PubKey (LedgerBytes pk) = pubKey
      Signature sig = signature_
      DatumHash h = datumHash
   in if verifySignature pk h sig
        then Right ()
        else Left $ SignatureMismatch signature_ pubKey datumHash

{-# INLINEABLE checkHashConstraints #-}

-- | Extrat the contents of the message and produce a constraint that checks
--   that the hash is correct. In off-chain code, where we check the hash
--   straightforwardly, 'checkHashOffChain' can be used instead of this.
checkHashConstraints ::
  (IsData a) =>
  -- | The signed message
  SignedMessage a ->
  Either SignedMessageCheckError (a, TxConstraints i o)
checkHashConstraints SignedMessage {osmMessageHash, osmDatum = Datum dt} =
  maybe
    (trace "DecodingError" $ Left DecodingError)
    (\a -> pure (a, Constraints.mustHashDatum osmMessageHash (Datum dt)))
    (fromData dt)

{-# INLINEABLE verifySignedMessageConstraints #-}

-- | Check the signature on a 'SignedMessage' and extract the contents of the
--   message, producing a 'TxConstraint' value that ensures the hashes match
--   up.
verifySignedMessageConstraints ::
  (IsData a) =>
  PubKey ->
  SignedMessage a ->
  Either SignedMessageCheckError (a, TxConstraints i o)
verifySignedMessageConstraints pk s@SignedMessage {osmSignature, osmMessageHash} =
  checkSignature osmMessageHash pk osmSignature
    >> checkHashConstraints s

{-# INLINEABLE verifySignedMessageOnChain #-}

-- | Check the signature on a 'SignedMessage' and extract the contents of the
--   message, using the pending transaction in lieu of a hash function. See
--   'verifySignedMessageConstraints' for a version that does not require a
--   'ValidatorCtx' value.
verifySignedMessageOnChain ::
  (IsData a) =>
  ValidatorCtx ->
  PubKey ->
  SignedMessage a ->
  Either SignedMessageCheckError a
verifySignedMessageOnChain ptx pk s@SignedMessage {osmSignature, osmMessageHash} = do
  checkSignature osmMessageHash pk osmSignature
  (a, constraints) <- checkHashConstraints s
  unless
    (Constraints.checkValidatorCtx @() @() constraints ptx)
    (Left $ DatumMissing osmMessageHash)
  pure a

-- | The off-chain version of 'checkHashConstraints', using the hash function
--   directly instead of obtaining the hash from a 'ValidatorCtx' value
checkHashOffChain ::
  (IsData a) =>
  SignedMessage a ->
  Either SignedMessageCheckError a
checkHashOffChain SignedMessage {osmMessageHash, osmDatum = dt} = do
  unless (osmMessageHash == Scripts.datumHash dt) (Left DatumNotEqualToExpected)
  let Datum dv = dt
  maybe (Left DecodingError) pure (fromData dv)

-- | Check the signature on a 'SignedMessage' and extract the contents of the
--   message.
verifySignedMessageOffChain ::
  (IsData a) =>
  PubKey ->
  SignedMessage a ->
  Either SignedMessageCheckError a
verifySignedMessageOffChain pk s@SignedMessage {osmSignature, osmMessageHash} =
  checkSignature osmMessageHash pk osmSignature
    >> checkHashOffChain s

-- | Encode a message of type @a@ as a @Data@ value and sign the
--   hash of the datum.
signMessage :: IsData a => a -> PrivateKey -> SignedMessage a
signMessage msg pk =
  let dt = Datum (toData msg)
      DatumHash msgHash = Scripts.datumHash dt
      sig = Crypto.sign (BSL.toStrict msgHash) pk
   in SignedMessage
        { osmSignature = sig,
          osmMessageHash = DatumHash msgHash,
          osmDatum = dt
        }

-- | Encode an observation of a value of type @a@ that was made at the given slot
signObservation :: IsData a => Slot -> a -> PrivateKey -> SignedMessage (Observation a)
signObservation sl vl = signMessage Observation {obsValue = vl, obsSlot = sl}

makeLift ''SignedMessage
makeIsData ''SignedMessage

makeLift ''Observation
makeIsData ''Observation
