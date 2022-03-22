{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Text.Pandoc.Include.Common.IO where

import System.Process
import System.Directory
import System.FilePath
import Control.Monad
import Text.Pandoc.Class
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import Text.Pandoc.Logging

withDir :: FilePath -> IO a -> IO a
withDir path f = do
  dir <- getCurrentDirectory
  setCurrentDirectory path
  result <- f
  setCurrentDirectory dir
  return result

inDir :: FilePath -> (FilePath -> IO a) -> IO a
inDir path f = do
  let (dir, file) = splitFileName path
  withDir dir $ f file

system_ s = void $ system s
runIO' :: PandocIO a -> IO a
runIO' f = do
  (res, reports) <- runIOorExplode $ do
    x <- f
    rs <- getLog
    return (x, rs)
  TIO.putStrLn $ T.unlines $ map showLogMessage reports
  return res


