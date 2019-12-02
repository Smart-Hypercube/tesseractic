module Tesseractic.Utils
    ( decodeIO
    , posixToUtc
    , utcToPosix
    ) where

import RIO
import RIO.Time
    ( Day (ModifiedJulianDay), UTCTime (UTCTime)
    , diffTimeToPicoseconds, picosecondsToDiffTime
    )

import Data.Fixed (mod')
import Data.Serialize (Serialize, decode)


decodeIO :: (MonadIO m, Serialize a) => ByteString -> m a
decodeIO = either fail pure . decode

utcToPosix :: UTCTime -> (Int64, Word32)
utcToPosix (UTCTime (ModifiedJulianDay d) t) = (s, ns)
  where
    days = d - 40587
    seconds = min 86400 $ floor t
    picoseconds = diffTimeToPicoseconds $ t `mod'` 1
    s = fromInteger $ days * 86400 + seconds
    ns = fromInteger $ picoseconds `div` 1000

posixToUtc :: (Int64, Word32) -> UTCTime
posixToUtc (s, ns) = UTCTime (ModifiedJulianDay d) t
  where
    days = toInteger $ s `div` 86400
    seconds = toInteger $ s `mod` 86400
    picoseconds = toInteger $ ns * 1000
    d = days + 40587
    t = fromInteger seconds + picosecondsToDiffTime picoseconds
