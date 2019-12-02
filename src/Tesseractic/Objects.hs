module Tesseractic.Objects
    ( loadObject
    , storeObject
    ) where

import RIO
import RIO.Directory (createDirectory, doesPathExist, listDirectory)
import RIO.File (writeBinaryFileDurableAtomic)
import RIO.FilePath ((</>))
import qualified RIO.ByteString as B

import Crypto.Hash (hash)
import Data.Serialize (Serialize, encode)

import Tesseractic (App, getStorage, storageToObjectsDir)
import Tesseractic.Types (Hash)
import Tesseractic.Utils (decodeIO)


loadObject :: Serialize a => Hash -> App a
loadObject = resolveObjectPath >=> B.readFile >=> decodeIO

storeObject :: Serialize a => a -> App Hash
storeObject object = do
    let encoded = encode object
        objectHash = hash encoded
    path <- resolveObjectPath objectHash
    writeBinaryFileDurableAtomic path encoded
    return objectHash

resolveObjectPath :: Hash -> App FilePath
resolveObjectPath h = do
    d <- storageToObjectsDir <$> getStorage
    resolveObjectPath' d $ show h

resolveObjectPath' :: FilePath -> String -> App FilePath
resolveObjectPath' d s = doesPathExist f >>= \case
    True -> return f
    False -> doesPathExist d' >>= \case
        True -> resolveObjectPath' d' s'
        False -> (< 256) . length <$> listDirectory d >>= \case
            True -> return f
            False -> createDirectory d' >> return f'
  where
    f = d </> s
    d' = d </> take 2 s
    s' = drop 2 s
    f' = d' </> s'
