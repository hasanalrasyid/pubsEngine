{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Text.Pandoc.Include.Common where

import Text.Pandoc.Options
import System.Process
import System.Directory
import System.FilePath
import Control.Monad

pandocExtSetting = [ Ext_tex_math_dollars
                   , Ext_raw_tex
                   , Ext_implicit_figures
                   , Ext_yaml_metadata_block
                   ]

mdOption = (def{readerExtensions = foldr enableExtension pandocExtensions pandocExtSetting})

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
