{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Template.Article where

import Data.Tuple
import Text.RawString.QQ
import qualified Data.Text.IO as T
import qualified Data.Text as T
import qualified Data.ByteString as BS
import Data.FileEmbed
import System.Process (callCommand)
import Text.Pandoc.Options

templateLatex :: IO (String, String, TopLevelDivision)
templateLatex = do
  BS.writeFile "_build/extra.7z" extraZip
  writeFile "_build/default.tpl" mainTemplate
  callCommand $ unlines [ "pushd _build"
                        , "7za x -aoa extra.7z"
                        , "popd"
                        ]
  return ("_build/default.tpl", mainTemplate, TopLevelSection)

mainTemplate :: String
mainTemplate = $(embedStringFile "templates/article/template.tex")

extraZip :: BS.ByteString
extraZip = $(embedFile "templates/article/extra.7z")
