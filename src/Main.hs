module Main (main) where

import RIO
import RIO.Directory (createDirectoryIfMissing, makeAbsolute)
import RIO.File (writeBinaryFileDurableAtomic)
import qualified RIO.Map as Map

import Data.Serialize (encode)
import System.Environment (getArgs)

import Tesseractic
import Tesseractic.Snapshot (snapshot)
import Tesseractic.Types (Cube (..), Index (..), SnapshotHash (..))
import Tesseractic.Utils (decodeIO)


main :: IO ()
main = getArgs >>= \case
    config:"init"      :args -> init config args
    config:"status"    :args -> withCube (runApp status) config args
    config:"snapshot"  :args -> withCube (runApp snapshot) config args
    config:"purgecache":args -> withCube (runApp purgecache) config args
    _:s:_ -> fail $ "Unknown command: " <> s
    _:_   -> fail "Missing command"
    _     -> fail "Missing config file path"

withCube :: (Cube -> IO ()) -> FilePath -> [String] -> IO ()
withCube f p [] = readFileBinary p >>= decodeIO >>= f
withCube _ _ _  = fail "Wrong arguments"

init :: FilePath -> [String] -> IO ()
init p [name,path,storage] = do
    path' <- makeAbsolute path
    storage' <- makeAbsolute storage
    writeBinaryFileDurableAtomic p . encode $ Cube
        { cubeName = fromString name
        , cubePath = path'
        , cubeStorage = storage'
        }
    createDirectoryIfMissing True $ storageToObjectsDir storage
    writeBinaryFileDurableAtomic indexPath . encode $ Index
        { indexSnapshot = SnapshotEmpty
        , indexCache = Map.empty
        }
  where
    indexPath = storageToIndexPath storage
init _ _ = fail "Wrong arguments"

status :: App ()
status = do
    getName >>= logInfo . ("Name: " <>) . display
    getPath >>= logInfo . ("Path: " <>) . fromString
    getStorage >>= logInfo . ("Storage: " <>) . fromString
    getSnapshot >>= logInfo . ("Snapshot: " <>) . displayShow

purgecache :: App ()
purgecache = modifyCache (const Map.empty) >> getSnapshot >>= commit
