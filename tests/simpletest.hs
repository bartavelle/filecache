module Main where

import System.IO.Temp
import Data.FileCache
import Control.Monad
import Control.Exception
import System.Directory
import Control.Concurrent
import qualified Data.HashMap.Strict as HM

computation :: String -> IO (Either String Int)
computation f = do
    c <- readFile f
    if length c `mod` 2 == 0
        then return (Right (length c))
        else return (Left "odd length")

main :: IO ()
main = withSystemTempDirectory "filecacheXXX.tmp" $ \tempdir -> do
    cache <- newFileCache :: IO (FileCache Int)
    let indexes = [1,10,100,1000]
        tofilename :: Int -> String
        tofilename i = tempdir ++ "/temp" ++ show i
    forM_ indexes $ \l -> writeFile (tofilename l) (show l)
    let q l = lazyQuery cache (tofilename l) (computation (tofilename l))
        qf l = lazyQuery cache (tofilename l) (throwIO (AssertionFailed "fail"))
        check x m = unless x (error m)
    lst1 <- mapM q indexes
    lst2 <- mapM qf indexes
    check (lst1 == lst2) "Request result was not cached"
    removeFile (tofilename 10)
    threadDelay (1*10^(6 :: Int))
    cacheInfo <- getCache cache
    check (HM.size cacheInfo == 3) "Invalidation didn't work"

