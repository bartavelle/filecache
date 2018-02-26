# filecache

[![Build Status](https://travis-ci.org/bartavelle/filecache.svg?branch=master)](https://travis-ci.org/bartavelle/filecache)
[![filecache on Stackage LTS 3](http://stackage.org/package/filecache/badge/lts-3)](http://stackage.org/lts-3/package/filecache)
[![filecache on Stackage Nightly](http://stackage.org/package/filecache/badge/nightly)](http://stackage.org/nightly/package/filecache)

A multi-platform (Linux, Mac, Windows) cache system associating values to files. The values are automatically discarded when the files are modified.

Example usage:

```
module Main where

import Data.FileCache

countlines :: FilePath -> IO (Either String Int)
countlines = fmap (Right . length . lines) . readFile

main :: IO ()
main = do
    cache <- newFileCache :: IO (FileCache Int)
    let countlines' p = lazyQuery cache p (countlines p)
    countlines' "/tmp/somefile" >>= print
```

This should either print `Left "/tmp/somefile: openFile: does not exist (No such file or directory)"` or `Right n`, where `n` is the line count.

Subsequent calls to `countlines'` will read the data from the cache if the file hasn't been modified, or will run the original computation again.
