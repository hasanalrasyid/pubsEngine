{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Text.Pandoc.Include.Utils where

import Text.Pandoc.Options
import System.Process
import System.Directory
import System.FilePath
import Control.Monad

import Text.Pandoc
import qualified Data.Map as M

import System.Process
import Text.Pandoc.Definition
import Data.Maybe
import System.Directory
import System.FilePath
import Data.Hashable
import Diagrams.Builder (hashToHexStr)
import qualified Data.Text as T
import Text.Pandoc.Process
import Text.Pandoc.UTF8 as UTF8
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as B8

import qualified Data.Text.IO as TIO

updateMeta (Meta m) key v = Meta $ M.alter (\_ -> Just v) key m

getHash mp = let mpHas = hashWithSalt 0 $ "_build/auto" <> mp
              in hashToHexStr mpHas

inDir :: FilePath -> IO a -> IO a
inDir path f = do
  dir <- getCurrentDirectory
  setCurrentDirectory path
  result <- f
  setCurrentDirectory dir
  return result

extractSource text opts0 =
  case lookup "src" opts0 of
    Just a -> do
      t <- TIO.readFile $ T.unpack a
      let o = M.toList $ M.alter (\_ -> Just text) "caption" $ M.fromList opts0
      return (t,o)
    _ -> return (text,opts0)

