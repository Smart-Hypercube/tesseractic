module Tesseractic
    ( App
    , Env (..)
    , commit
    , getName
    , getPath
    , getSnapshot
    , getStorage
    , modifyCache
    , readCache
    , runApp
    , storageToIndexPath
    , storageToObjectsDir
    ) where

import RIO
import RIO.File (writeBinaryFileDurableAtomic)
import RIO.FilePath ((</>))

import Data.Serialize (encode)

import Tesseractic.Types
import Tesseractic.Utils (decodeIO)


data Env = Env
    { envLogFunc :: !LogFunc
    , envCube :: !Cube
    , envSnapshot :: !SnapshotHash
    , envCache :: !(SomeRef Cache)
    }
instance HasLogFunc Env where
    logFuncL = lens envLogFunc (\x y -> x { envLogFunc = y })

type App = RIO Env

runApp :: App a -> Cube -> IO a
runApp app cube = do
    index <- readFileBinary indexPath >>= decodeIO
    cacheRef <- newSomeRef $ indexCache index
    logOptions <- logOptionsHandle stdout False
    withLogFunc logOptions $ \logFunc -> flip runRIO app $ Env
        { envLogFunc = logFunc
        , envCube = cube
        , envSnapshot = indexSnapshot index
        , envCache = cacheRef
        }
  where
    indexPath = storageToIndexPath $ cubeStorage cube

getName :: App Text
getName = cubeName . envCube <$> ask

getPath :: App FilePath
getPath = cubePath . envCube <$> ask

getStorage :: App FilePath
getStorage = cubeStorage . envCube <$> ask

getSnapshot :: App SnapshotHash
getSnapshot = envSnapshot <$> ask

readCache :: App Cache
readCache = envCache <$> ask >>= readSomeRef

modifyCache :: (Cache -> Cache) -> App ()
modifyCache f = envCache <$> ask >>= \ref -> modifySomeRef ref f

commit :: SnapshotHash -> App ()
commit snapshotHash = do
    indexPath <- storageToIndexPath <$> getStorage
    cache <- readCache
    writeBinaryFileDurableAtomic indexPath $ encode $ Index
        { indexSnapshot = snapshotHash
        , indexCache = cache
        }

storageToIndexPath :: FilePath -> FilePath
storageToIndexPath = (</> "index")

storageToObjectsDir :: FilePath -> FilePath
storageToObjectsDir = (</> "objects")
