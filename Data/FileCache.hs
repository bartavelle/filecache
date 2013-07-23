module Data.FileCache (FileCache, newFileCache, killFileCache, invalidate, query, getCache) where

import qualified Data.HashMap.Strict as HM
import System.INotify
import Control.Concurrent
import qualified Data.Either.Strict as S
import Control.Exception
import Control.Monad

data Messages a = Invalidate !FilePath
                | Query !FilePath !(IO (S.Either String a)) !(MVar (S.Either String a))
                | GetCopy !(MVar (HM.HashMap FilePath (a, WatchDescriptor)))
                | Stop

data FileCache a = FileCache !(Chan (Messages a))

-- | Generates a new file cache. The opaque type is for use with other
-- functions.
newFileCache :: IO (FileCache a)
newFileCache = do
    q <- newChan
    ino <- initINotify
    void $ forkIO (mapMaster HM.empty q ino)
    return (FileCache q)

-- | Destroys the thread running the FileCache. Pretty dangerous stuff.
killFileCache :: FileCache a -> IO ()
killFileCache (FileCache q) = writeChan q Stop

mapMaster :: HM.HashMap FilePath (a, WatchDescriptor) -> Chan (Messages a) -> INotify -> IO ()
mapMaster mp q ino = do
    msg <- readChan q
    case msg of
        Stop -> killINotify ino
        Invalidate fp ->
            case HM.lookup fp mp of
                Nothing -> mapMaster mp q ino
                Just (_,desc) -> removeWatch desc >> mapMaster (HM.delete fp mp) q ino
        Query fp action respvar ->
            case HM.lookup fp mp of
                Just (x,_) -> putMVar respvar (S.Right x)
                Nothing -> do
                    valr <- action `catch` (\e -> return (S.Left ("Exception: " ++ show (e :: SomeException))))
                    case valr of
                        S.Right val -> do -- this is blocking for some reason
                            wm <- addWatch ino [CloseWrite,Delete,Move] fp (const $ invalidate fp (FileCache q))
                            putMVar respvar (S.Right val)
                            mapMaster (HM.insert fp (val,wm) mp) q ino
                        S.Left rr -> putMVar respvar (S.Left rr)
        GetCopy mv -> putMVar mv mp >> mapMaster mp q ino

-- | Manually invalidates an entry.
invalidate :: FilePath -> FileCache a -> IO ()
invalidate fp (FileCache q) = writeChan q (Invalidate fp)

-- | Queries the cache, populating it if necessary.
query :: FileCache a
      -> FilePath -- ^ Path of the file entry
      -> IO (S.Either String a) -- ^ The computation that will be used to populate the cache
      -> IO (S.Either String a)
query (FileCache q) fp generate = do
    v <- newEmptyMVar
    writeChan q (Query fp generate v)
    readMVar v

-- | Gets a copy of the cache.
getCache :: FileCache a -> IO (HM.HashMap FilePath (a, WatchDescriptor))
getCache (FileCache q) = do
    v <- newEmptyMVar
    writeChan q (GetCopy v)
    readMVar v

