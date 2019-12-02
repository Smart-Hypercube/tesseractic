{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE StandaloneDeriving #-}

module Tesseractic.OrphanInstance () where

import RIO
import RIO.Partial (fromJust)
import RIO.Time (UTCTime)

import Crypto.Hash
    ( Digest, HashAlgorithm
    , digestFromByteString, hashDigestSize
    )
import Data.ByteArray (convert)
import Data.Serialize (Serialize (get, put), getBytes, putByteString)
import System.Posix.Types
    ( CDev (CDev), CGid (CGid), CMode (CMode), CUid (CUid)
    )

import Tesseractic.Utils (posixToUtc, utcToPosix)


instance Serialize Text where
    put = put . encodeUtf8
    get = decodeUtf8' <$> get >>= either (fail . show) pure

instance Serialize UTCTime where
    put = put . utcToPosix
    get = posixToUtc <$> get

deriving instance Serialize CDev

deriving instance Serialize CGid

deriving instance Serialize CMode

deriving instance Serialize CUid

instance HashAlgorithm a => Serialize (Digest a) where
    put = putByteString . convert
    get = fromJust . digestFromByteString <$> getBytes size
      where
        size = hashDigestSize (undefined :: a)
