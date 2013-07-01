module Data.FileCache (FileCache, newFileCache, killFileCache, invalidate, query, getCache) where

import Control.Monad.STM
import Control.Concurrent.STM
import qualified Data.HashMap.Strict as HM
import System.INotify
import Control.Concurrent
import qualified Data.Either.Strict as S
import Control.Exception

data Messages a = Invalidate !FilePath
                | Query !FilePath !(IO (S.Either String a)) !(TMVar (S.Either String a))
                | GetCopy !(TMVar (HM.HashMap FilePath (a, WatchDescriptor)))

data FileCache a = FileCache !(TQueue (Messages a)) ThreadId

-- | Generates a new file cache. The opaque type is for use with other
-- functions.
newFileCache :: IO (FileCache a)
newFileCache = do
    q <- newTQueueIO
    i <- forkIO (mapMaster HM.empty q)
    return (FileCache q i)

-- | Destroys the thread running the FileCache. Pretty dangerous stuff.
killFileCache :: FileCache a -> IO ()
killFileCache (FileCache _ i) = killThread i

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
                Just (x,_) -> atomically (putTMVar respvar (S.Right x))
                Nothing -> do
                    valr <- action `catch` (\e -> return (S.Left ("Exception: " ++ show (e :: SomeException))))
                    case valr of
                        S.Right val -> do
                            wm <- withINotify (\ino -> addWatch ino [CloseWrite,Delete,Move] fp (const $ invalidate fp (FileCache q undefined)))
                            atomically (putTMVar respvar (S.Right val))
                            mapMaster (HM.insert fp (val,wm) mp) q
                        S.Left rr -> atomically (putTMVar respvar (S.Left rr))
        GetCopy mv -> atomically (putTMVar mv mp) >> mapMaster mp q

-- | Manually invalidates an entry.
invalidate :: FilePath -> FileCache a -> IO ()
invalidate fp (FileCache q _) = atomically (writeTQueue q (Invalidate fp))

-- | Queries the cache, populating it if necessary.
query :: FileCache a
      -> FilePath -- ^ Path of the file entry
      -> IO (S.Either String a) -- ^ The computation that will be used to populate the cache
      -> IO (S.Either String a)
query (FileCache q _) fp generate = atomically $ do
    v <- newEmptyTMVar
    writeTQueue q (Query fp generate v)
    readTMVar v

-- | Gets a copy of the cache.
getCache :: FileCache a -> IO (HM.HashMap FilePath (a, WatchDescriptor))
getCache (FileCache q _) = atomically $ do
    v <- newEmptyTMVar
    writeTQueue q (GetCopy v)
    readTMVar v

