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

updateMeta (Meta m) key v = Meta $ M.alter (\_ -> Just v) key m
