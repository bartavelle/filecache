module Data.FileCache (FileCache, FileCacheR, newFileCache, killFileCache, invalidate, query, getCache, lazyQuery) where

import qualified Data.HashMap.Strict as HM
import System.INotify
import Control.Concurrent
import qualified Data.Either.Strict as S
import Control.Exception
import Control.Monad
import Control.Monad.Error.Class
import Control.Exception.Lens

data Messages r a = Invalidate !FilePath
                  | Query !FilePath !(IO (S.Either r a)) !(MVar (S.Either r a))
                  | GetCopy !(MVar (HM.HashMap FilePath (S.Either r a, WatchDescriptor)))
                  | Stop

data FileCacheR r a = FileCache !(Chan (Messages r a))
type FileCache = FileCacheR String

-- | Generates a new file cache. The opaque type is for use with other
-- functions.
newFileCache :: Error r => IO (FileCacheR r a)
newFileCache = do
    q <- newChan
    ino <- initINotify
    void $ forkIO (mapMaster HM.empty q ino)
    return (FileCache q)

-- | Destroys the thread running the FileCache. Pretty dangerous stuff.
killFileCache :: FileCacheR r a -> IO ()
killFileCache (FileCache q) = writeChan q Stop

mapMaster :: Error r => HM.HashMap FilePath (S.Either r a, WatchDescriptor) -> Chan (Messages r a) -> INotify -> IO ()
mapMaster mp q ino = do
    let nochange = return (Just mp)
        change x = return (Just (x mp))
    msg <- readChan q
    nmp <- case msg of
            Stop -> killINotify ino >> return Nothing
            Invalidate fp ->
                case HM.lookup fp mp of
                    Nothing -> nochange
                    Just (_,desc) -> catching_ id (removeWatch desc) (return ()) >> change (HM.delete fp)
            Query fp action respvar ->
                case HM.lookup fp mp of
                    Just (x,_) -> putMVar respvar x >> nochange
                    Nothing -> do
                        let addw value = do
                                wm <- addWatch ino [CloseWrite,Delete,Move,Attrib,Create] fp (const $ invalidate fp (FileCache q))
                                change (HM.insert fp (value,wm))
                            withWatch value = do
                                putMVar respvar value
                                catching_ id (addw value) nochange
                            noWatch x = putMVar respvar x >> nochange
                        catches (action >>= withWatch)
                            [ handler _IOException (\io -> noWatch   (S.Left (strMsg $ show io)))
                            , handler id           (\e  -> withWatch (S.Left (strMsg $ show e)))
                            ]
            GetCopy mv -> putMVar mv mp >> nochange
    case nmp of
        Just x -> mapMaster x q ino
        Nothing -> return ()

-- | Manually invalidates an entry.
invalidate :: Error r => FilePath -> FileCacheR r a -> IO ()
invalidate fp (FileCache q) = writeChan q (Invalidate fp)

-- | Queries the cache, populating it if necessary.
query :: Error r
      => FileCacheR r a
      -> FilePath -- ^ Path of the file entry
      -> IO (S.Either r a) -- ^ The computation that will be used to populate the cache
      -> IO (S.Either r a)
query (FileCache q) fp generate = do
    v <- newEmptyMVar
    writeChan q (Query fp generate v)
    readMVar v

-- | Just like `query`, but with the standard "Either" type.
lazyQuery :: Error r
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
getCache :: Error r => FileCacheR r a -> IO (HM.HashMap FilePath (S.Either r a, WatchDescriptor))
getCache (FileCache q) = do
    v <- newEmptyMVar
    writeChan q (GetCopy v)
    readMVar v

