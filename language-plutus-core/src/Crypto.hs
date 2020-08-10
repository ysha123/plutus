module Crypto (verifySignature) where

import Control.Applicative
import Crypto.ECC.Ed25519Donna
import Crypto.Error (maybeCryptoError)
import qualified Data.ByteString.Lazy as BSL

verifySignature ::
  Alternative f =>
  -- | Public Key
  BSL.ByteString ->
  -- | Message
  BSL.ByteString ->
  -- | Signature
  BSL.ByteString ->
  f Bool
verifySignature pubKey msg sig =
  maybe empty pure . maybeCryptoError $
    verify
      <$> publicKey (BSL.toStrict pubKey)
      <*> pure (BSL.toStrict msg)
      <*> signature (BSL.toStrict sig)
