module Tesseractic.Snapshot
    ( snapshot
    ) where

import RIO
import RIO.Directory (getModificationTime, listDirectory)
import RIO.FilePath ((</>))
import RIO.Time (UTCTime, getCurrentTime)
import qualified RIO.ByteString as B
import qualified RIO.Map as Map

import System.PosixCompat.Files
    ( fileGroup
    , fileMode
    , fileOwner
    , fileSize
    , getSymbolicLinkStatus
    , isBlockDevice
    , isCharacterDevice
    , isDirectory
    , isNamedPipe
    , isRegularFile
    , isSocket
    , isSymbolicLink
    , readSymbolicLink
    , specialDeviceID
    )

import Tesseractic
import Tesseractic.Objects
import Tesseractic.Types


snapshot :: App ()
snapshot = do
    parent <- getSnapshot
    time <- getCurrentTime
    nodeHash <- getPath >>= snapshotNode
    time' <- getCurrentTime
    snapshotHash' <- storeObject $ Snapshot
        { snapshotParent = parent
        , snapshotRoot = nodeHash
        , snapshotTimeRange = (time, time')
        }
    commit $ SnapshotHash snapshotHash'

snapshotNode :: FilePath -> App NodeHash
snapshotNode path = do
    status <- liftIO $ getSymbolicLinkStatus path
    mTime <- getModificationTime path  -- hi-res mtime
    let size = fromIntegral $ fileSize status
        meta = Meta
            { metaMTime = Just mTime
            , metaMode = Just $ fileMode status
            , metaOwner = Just $ fileOwner status
            , metaGroup = Just $ fileGroup status
            }
    if  | isRegularFile status -> do
            blocks <- getBlocks path size mTime
            storeObject $ File size blocks meta
        | isDirectory status -> do
            items <- listDirectory path
            nodeHashes <- forM items $ snapshotNode . (path </>)
            storeObject $ Dir (Map.fromList $ zip items nodeHashes) meta
        | isSymbolicLink status -> do
            content <- liftIO $ readSymbolicLink path
            storeObject $ Link content meta
        | isBlockDevice status ->
            storeObject $ BlockDevice (specialDeviceID status) meta
        | isCharacterDevice status ->
            storeObject $ CharDevice (specialDeviceID status) meta
        | isNamedPipe status ->
            storeObject $ Pipe meta
        | isSocket status ->
            storeObject $ Socket meta
        | otherwise -> fail $ "Unknown file type: " <> path

getBlocks :: FilePath -> Size -> UTCTime -> App [BlockHash]
getBlocks path size time = do
    Map.lookup path <$> readCache >>= \case
        Just cacheItem -> do
            if  cacheItemSize cacheItem == size &&
                cacheItemTime cacheItem == time
            then return $ cacheItemBlocks cacheItem
            else loadBlocks
        Nothing -> loadBlocks
  where
    loadBlocks = do
        blocks <- withBinaryFile path ReadMode f
        modifyCache $ Map.insert path $ CacheItem
            { cacheItemSize = size
            , cacheItemTime = time
            , cacheItemBlocks = blocks
            }
        return blocks
    f h = B.hGet h 1048576 >>= \case
        "" -> return []
        s -> (:) <$> storeObject s <*> f h
