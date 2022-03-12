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

templateLatex :: IO String
templateLatex = do
  BS.writeFile "_build/additional.zip" additionalZip
  callCommand $ unlines [ "pushd _build"
                        , "unzip -o additional.zip"
                        , "popd"
                        ]
  return mainTemplate

mainTemplate :: String
mainTemplate = $(embedStringFile "templates/article/template.tex")

additionalZip :: BS.ByteString
additionalZip = $(embedFile "templates/article/aa-package.zip")

