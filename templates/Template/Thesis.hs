{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Template.Thesis where

import Data.Tuple
import qualified Data.Text.IO as T
import qualified Data.Text as T
import qualified Data.ByteString as BS
import Data.FileEmbed
import System.Process (callCommand)
import Text.Pandoc.Options

templateLatex :: IO (String, String, TopLevelDivision)
templateLatex = do
  BS.writeFile "_build/extra.tbz" extraZip
  writeFile "_build/default.tpl" mainTemplate
  callCommand $ unlines [ "pushd _build"
                        , "tar -xf extra.tbz"
                        , "popd"
                        ]
  return ("_build/default.tpl", mainTemplate, TopLevelChapter)

mainTemplate :: String
mainTemplate = $(embedStringFile "templates/thesis/template.tex")

extraZip :: BS.ByteString
extraZip = $(embedFile "templates/thesis/extra.tbz")

