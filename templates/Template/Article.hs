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

templateLatex :: IO (String, String)
templateLatex = do
  BS.writeFile "_build/extra.tbz" extraZip
  writeFile "_build/default.tpl" mainTemplate
  callCommand $ unlines [ "pushd _build"
                        , "tar -xf extra.tbz"
                        , "popd"
                        ]
  return ("_build/default.tpl", mainTemplate)

mainTemplate :: String
mainTemplate = $(embedStringFile "templates/article/template.tex")

extraZip :: BS.ByteString
extraZip = $(embedFile "templates/article/extra.tbz")
