module Data.FileCache (FileCache, newFileCache, invalidate, query, getCache) where

import Control.Monad.STM
import Control.Concurrent.STM
import qualified Data.HashMap.Strict as HM
import System.INotify
import Control.Concurrent

data Messages a = Invalidate !FilePath
                | Query !FilePath !(IO a) !(TMVar a)
                | GetCopy !(TMVar (HM.HashMap FilePath (a, WatchDescriptor)))

data FileCache a = FileCache !(TQueue (Messages a))

-- | Generates a new file cache. The opaque type is for use with other
-- functions.
newFileCache :: IO (FileCache a)
newFileCache = do
    q <- newTQueueIO
    forkIO (mapMaster HM.empty q)
    return (FileCache q)

mapMaster :: HM.HashMap FilePath (a, WatchDescriptor) -> TQueue (Messages a) -> IO ()
mapMaster mp q = do
    msg <- atomically (readTQueue q)
    case msg of
        Invalidate fp ->
            case HM.lookup fp mp of
                Nothing -> mapMaster mp q
                Just (_,desc) -> removeWatch desc >> mapMaster (HM.delete fp mp) q
        Query fp action respvar ->
            case HM.lookup fp mp of
                Just (x,_) -> atomically (putTMVar respvar x)
                Nothing -> do
                    val <- action
                    wm <- withINotify (\ino -> addWatch ino [CloseWrite,Delete,Move] fp (const $ invalidate fp (FileCache q)))
                    atomically (putTMVar respvar val)
                    mapMaster (HM.insert fp (val,wm) mp) q
        GetCopy mv -> atomically (putTMVar mv mp) >> mapMaster mp q

-- | Manually invalidates an entry.
invalidate :: FilePath -> FileCache a -> IO ()
invalidate fp (FileCache q) = atomically (writeTQueue q (Invalidate fp))

-- | Query the cache, populating it if necessary.
query :: FileCache a
      -> FilePath -- ^ Path of the file entry
      -> IO a -- ^ The computation that will be used to populate the cache
      -> IO a
query (FileCache q) fp generate = atomically $ do
    v <- newEmptyTMVar
    writeTQueue q (Query fp generate v)
    readTMVar v

-- | Get a copy of the cache.
getCache :: FileCache a -> IO (HM.HashMap FilePath (a, WatchDescriptor))
getCache (FileCache q) = atomically $ do
    v <- newEmptyTMVar
    writeTQueue q (GetCopy v)
    readTMVar v

