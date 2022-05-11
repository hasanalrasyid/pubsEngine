{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Text.Pandoc.Include.Template (setTemplate) where

import Data.Tuple
import Text.RawString.QQ
import qualified Data.Text.IO as T
import qualified Data.Text as T
import qualified Data.ByteString as BS
import Data.FileEmbed
import System.Process (callCommand)
import Text.Pandoc.Options

setTemplate :: String -> IO (String, String, TopLevelDivision)
setTemplate nameTemplate = do
  BS.writeFile "_build/extra.7z" $ extraZip nameTemplate
  writeFile "_build/default.tpl" $ mainTemplate nameTemplate
  callCommand $ unlines [ "pushd _build"
                        , "7za x -aoa extra.7z"
                        , "popd"
                        ]
  let topLevel = case nameTemplate of
                  "article" -> TopLevelSection
                  _ -> TopLevelChapter
  return ("_build/default.tpl", mainTemplate nameTemplate, topLevel)

mainTemplate :: String -> String
mainTemplate "article" = $(embedStringFile $ "templates/article/template.tex")
mainTemplate "thesis" = $(embedStringFile $ "templates/thesis/template.tex")
mainTemplate "revealjs" = $(embedStringFile $ "templates/revealjs/template.tex")
mainTemplate _ = $(embedStringFile $ "templates/plain/template.tex")

extraZip :: String -> BS.ByteString
extraZip "article" = $(embedFile "templates/article/extra.7z")
extraZip "thesis" = $(embedFile "templates/thesis/extra.7z")
extraZip "revealjs" = $(embedFile "templates/revealjs/extra.7z")
extraZip _ = $(embedFile "templates/plain/extra.7z")
