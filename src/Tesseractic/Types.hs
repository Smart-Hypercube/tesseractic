module Tesseractic.Types
    ( BlockHash
    , Cache
    , CacheItem (..)
    , Cube (..)
    , Hash
    , Index (..)
    , Meta (..)
    , Node (..)
    , NodeHash
    , Size (..)
    , Snapshot (..)
    , SnapshotHash (..)
    ) where

import RIO
import RIO.Partial (fromJust)
import RIO.Time (UTCTime)
import qualified RIO.ByteString as B

import Crypto.Hash (Digest, SHA3_256, digestFromByteString, hashDigestSize)
import Data.Serialize
    ( Get, Serialize
    , get, getBytes, getWord8, isolate, lookAheadM
    , put, putByteString, putNested, putWord8
    )
import System.Posix.Types (CGid, CMode, CUid, DeviceID)

import Tesseractic.OrphanInstance ()


data Cube = Cube
    { cubeName :: !Text
    , cubePath :: !FilePath
    , cubeStorage :: !FilePath
    } deriving (Generic, Show)
instance Serialize Cube

data Index = Index
    { indexSnapshot :: !SnapshotHash
    , indexCache :: !Cache
    } deriving (Generic, Show)
instance Serialize Index

data CacheItem = CacheItem
    { cacheItemTime :: !UTCTime
    , cacheItemSize :: !Size
    , cacheItemBlocks :: ![BlockHash]
    } deriving (Generic, Show)
instance Serialize CacheItem

data SnapshotHash
    = SnapshotHash !Hash
    | SnapshotEmpty
instance Show SnapshotHash where
    show (SnapshotHash h) = show h
    show SnapshotEmpty = "empty"
instance Serialize SnapshotHash where
    put (SnapshotHash h) = put h
    put SnapshotEmpty = putByteString $ B.replicate size 0
      where
        size = hashDigestSize (undefined :: SHA3_256)
    get = f <$> getBytes size
      where
        f s | B.all (== 0) s = SnapshotEmpty
            | otherwise = SnapshotHash $ h s
        h s = fromJust $ digestFromByteString s
        size = hashDigestSize (undefined :: SHA3_256)

data Snapshot = Snapshot
    { snapshotParent :: !SnapshotHash
    , snapshotRoot :: !NodeHash
    , snapshotTimeRange :: !(UTCTime, UTCTime)
    } deriving (Generic, Show)
instance Serialize Snapshot

type NodeHash = Hash

data Node
    = File !Size ![BlockHash] !Meta
    | Dir !(Map FilePath NodeHash) !Meta
    | Link !FilePath !Meta
    | BlockDevice !DeviceID !Meta
    | CharDevice !DeviceID !Meta
    | Pipe !Meta
    | Socket !Meta
    deriving (Generic, Show)
instance Serialize Node

type BlockHash = Hash

type Hash = Digest SHA3_256
type Cache = Map FilePath CacheItem

newtype Size = Size Word64
    deriving (Bounded, Eq, Num, Ord, Serialize, Show)

data Meta = Meta
    { metaMTime :: !(Maybe UTCTime)
    , metaMode :: !(Maybe CMode)
    , metaOwner :: !(Maybe CUid)
    , metaGroup :: !(Maybe CGid)
    } deriving (Show)
instance Serialize Meta where
    put meta = do
        f $ metaMTime meta
        f $ metaMode meta
        f $ metaOwner meta
        f $ metaGroup meta
        putWord8 255
      where
        putWord8Ranged x
            | 0 < x && x < 255 = putWord8 $ fromIntegral x
            | otherwise = fail "Illegal Word8 value"
        f (Just value) = putNested putWord8Ranged (put value)
        f Nothing = putWord8 0
    get = Meta <$> f <*> f <*> f <*> f
      where
        f :: Serialize a => Get (Maybe a)
        f = (maybe Nothing id <$>) $ lookAheadM $ getWord8 >>= \case
            255 -> return Nothing
            0 -> return $ Just Nothing
            x -> Just <$> Just <$> isolate (fromIntegral x) get
