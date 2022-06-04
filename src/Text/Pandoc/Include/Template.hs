{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Text.Pandoc.Include.Template
  ( setTemplate
  , mainTemplate
  , commonTemplate
  ) where

import Data.Tuple
import Text.RawString.QQ
import qualified Data.Text.IO as T
import qualified Data.Text as T
import qualified Data.ByteString as BS
import Data.FileEmbed
import System.Process (callCommand)
import Text.Pandoc.Options

setTemplate :: String -> String -> IO (String, TopLevelDivision)
setTemplate nameTemplate fileName = do
  BS.writeFile "_build/temp/extra.7z" $ extraZip nameTemplate
  callCommand $ unlines [ "cd _build"
                        , "7za x -aoa temp/extra.7z"
                        , "cd .."
                        ]
  let topLevel = case nameTemplate of
                  "article" -> TopLevelSection
                  "snat" -> TopLevelSection
                  "plain" -> TopLevelSection
                  _ -> TopLevelChapter
  return ("_build/default.tpl", topLevel)

commonTemplate = T.pack $(embedStringFile $ "templates/common/template.tex")

mainTemplate :: String -> String
mainTemplate "book" = $(embedStringFile $ "templates/book/template.tex")
mainTemplate "article" = $(embedStringFile $ "templates/article/template.tex")
mainTemplate "snat" = $(embedStringFile $ "templates/snat/template.tex")
mainTemplate "thesis" = $(embedStringFile $ "templates/thesis/template.tex")
mainTemplate "revealjs" = $(embedStringFile $ "templates/revealjs/template.tex")
mainTemplate _ = $(embedStringFile $ "templates/plain/template.tex")

extraZip :: String -> BS.ByteString
extraZip "book" = $(embedFile "templates/book/extra.7z")
extraZip "article" = $(embedFile "templates/article/extra.7z")
extraZip "snat" = $(embedFile "templates/snat/extra.7z")
extraZip "thesis" = $(embedFile "templates/thesis/extra.7z")
extraZip "revealjs" = $(embedFile "templates/revealjs/extra.7z")
extraZip _ = $(embedFile "templates/plain/extra.7z")
