{- |
This module let you create caches where keys are file names, and values are automatically expired when the file is modified for any reason.

This is usually done in the following fashion :

> cache <- newFileCache
> o <- query cache "/path/to/file" computation

The computation will be used to populate the cache if this call results in a miss.
-}
module Data.FileCache (FileCache, FileCacheR, newFileCache, killFileCache, invalidate, query, getCache, lazyQuery) where

import qualified Data.HashMap.Strict as HM
import System.INotify
import Control.Concurrent.STM
import qualified Data.Either.Strict as S
import Control.Monad.Catch
import Control.Exception.Lens
import Control.Applicative
import Control.Monad (join)
import Data.String

-- | The main FileCache type, for queries returning 'Either r a'. The r
-- type must be an instance of 'Error'.
data FileCacheR r a = FileCache !(TVar (HM.HashMap FilePath (S.Either r a, WatchDescriptor))) !INotify

-- | A default type synonym, for String errors.
type FileCache = FileCacheR String

-- | Generates a new file cache. The opaque type is for use with other
-- functions.
newFileCache :: IO (FileCacheR r a)
newFileCache = FileCache <$> newTVarIO HM.empty <*> initINotify

-- | Destroys the thread running the FileCache. Pretty dangerous stuff.
killFileCache :: FileCacheR r a -> IO ()
killFileCache (FileCache _ ino) = killINotify ino

-- | Manually invalidates an entry.
invalidate :: FilePath -> FileCacheR e a -> IO ()
invalidate fp (FileCache q _) = join $ atomically $ do
    mp <- readTVar q
    case HM.lookup fp mp of
        Nothing -> return (return ())
        Just (_,desc) -> do
            writeTVar q (HM.delete fp mp)
            return (removeWatch desc)

-- | Queries the cache, populating it if necessary, returning a strict
-- 'Either' (from "Data.Either.Strict").
--
-- Queries that fail with an 'IOExeception' will not create a cache entry.
-- Also please note that there is a race condition between the potential
-- execution of the computation and the establishment of the watch.
query :: IsString e
      => FileCacheR e a
      -> FilePath -- ^ Path of the file entry
      -> IO (S.Either e a) -- ^ The computation that will be used to populate the cache
      -> IO (S.Either e a)
query f@(FileCache q ino) fp action = do
    mp <- getCache f
    case HM.lookup fp mp of
        Just (x,_) -> return x
        Nothing -> do
            let addw value = do
                    wm <- addWatch ino [CloseWrite,Delete,Move,Attrib,Create] fp (const $ invalidate fp f)
                    change (HM.insert fp (value,wm))
                withWatch value = do
                    catching_ id (addw value) nochange
                    return value
                change = atomically . modifyTVar q
                nochange = return ()
            catches (action >>= withWatch)
                [ handler _IOException (return . S.Left . fromString . show)
                , handler id           (withWatch . S.Left . fromString . show)
                ]
-- | Just like `query`, but with the standard "Either" type.
lazyQuery :: IsString r
          => FileCacheR r a
          -> FilePath -- ^ Path of the file entry
          -> IO (Either r a) -- ^ The computation that will be used to populate the cache
          -> IO (Either r a)
lazyQuery q fp generate = fmap unstrict (query q fp (fmap strict generate))
    where
        strict (Left x) = S.Left x
        strict (Right x) = S.Right x
        unstrict (S.Left x) = Left x
        unstrict (S.Right x) = Right x

-- | Gets a copy of the cache.
getCache :: FileCacheR e a -> IO (HM.HashMap FilePath (S.Either e a, WatchDescriptor))
getCache (FileCache q _) = atomically (readTVar q)

