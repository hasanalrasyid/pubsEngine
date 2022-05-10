{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Text.Pandoc.Include.Variable where

import qualified Text.Pandoc.Include.Table as InTable
import Text.Pandoc.Include.Thesis (linkTex)
import qualified Text.Pandoc.Include.Markdown as Markdown
import qualified Text.Pandoc.Include.MultiMarkdown as MultiMarkdown
import qualified Text.Pandoc.Include.Delegate as Delegate
import qualified Text.Pandoc.Include.FeynMP as FeynMP
import qualified Text.Pandoc.Include.Mermaid as Mermaid
import           Text.Pandoc.JSON
import Text.Pandoc.Walk
import qualified Text.Pandoc.Class as PIO
import qualified Text.Pandoc.Templates as PT

import Data.Monoid ((<>))

import Data.Maybe
import System.Environment (getArgs)

import Text.Pandoc
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Text.Pandoc.Include.Common
import qualified Data.Map as M
import System.Process (callCommand, readProcess)

import qualified Control.Exception as E
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import Text.Pandoc.App
import System.FilePath -- (replaceDirectory)
import System.Directory -- (listDirectory)
import Data.List -- (delete)


saveLibrary :: [T.Text] -> FilePath -> String -> IO [Block]
saveLibrary (_:lib:_) fileName script = do
  let libName = T.unpack lib
  writeFile ("_build/temp/lib/" </> libName </> fileName <.> libName) script
  let (cmd,description,_) = getCommand libName []
  r <- readProcess cmd [] $ unlines [script, description]
  res <- runIO $ readMarkdown mdOption $ T.pack r
  return $ case res of
      Left e -> [Para [Str $ "ERROR: saveLibrary: readMarkdown: " <> (T.pack $ show e)]]
      Right (Pandoc _ b) -> b

getCommand c@"py"      files = ( "python","print(description)"
                               , unlines [ "import sys, os"
                                         , unlines $ map (\f -> unwords ["from _build.temp.lib.py." <> f,"import *"] ) $ map takeBaseName files
                                         ]
                               )
getCommand c@"sh"      files = ("zsh","echo description"
                               ,  unlines $ map (\f -> ". '_build/temp/lib"</> c </> f <> "'") files )
getCommand c@"gnuplot" files = ("gnuplot",unlines ["set print '-'", "print description"]
                               ,  unlines $ map (\f -> "load '_build/temp/lib"</> c </> f <> "'") files )
getCommand a         _ = ("zsh","echo ERROR: getDescription: saveLibrary: do not know how to call the library for class: " <> a, "")

writeScriptResult res (_, ["script",cmd,"md"], _) = do
  r <- runIO $ readMarkdown mdOption $ T.pack res
  return $ Div nullAttr $ case r of
    Left e -> [Para [Str $ "ERROR: script."<> cmd <>".md: cannot parse the markdown output: " <> (T.pack $ show e)]]
    Right (Pandoc _ b) -> b
writeScriptResult _ (label, ["script",c,"img"], opts) = do
  let fileName = fromMaybe "defaultImg" $ lookup "file" opts
  ltx <- runIO $ do
    let c = fromMaybe "" $ lookup "caption" opts
    (readMarkdown mdOption $ c) >>= writeLaTeX def
  let caption = case ltx of
                  Left e -> [Str $ "ERROR: script."<> c <>".md: cannot parse the markdown output: " <> (T.pack $ show e)]
                  Right p -> [RawInline (Format "latex") p]
  return $ Div nullAttr [Para [Image (label,[],opts) caption (fileName, label) ]]
writeScriptResult res c = return $ Div nullAttr [Para [Str $ T.pack $ "ERROR: unacceptable script class headers, we got: " <> show c ]]

extractSource text opts0 =
  case lookup "src" opts0 of
    Just a -> do
      t <- TIO.readFile $ T.unpack a
      let o = M.toList $ M.alter (\_ -> Just text) "caption" $ M.fromList opts0
      return (t,o)
    _ -> return (text,opts0)

includeScript :: Block -> IO Block
-----------------------------------------Library----------------------------------------
includeScript cb@(CodeBlock (label, classes@("script":_:"lib":_), opts0) text) = do
  (script,opts) <- extractSource text opts0
  let fileName = T.unpack $ fromMaybe "libDefault" $ lookup "file" opts
  let upperPart = if elem "show" classes then [CodeBlock (label,[],[]) $ T.pack $ unlines $ deleteBy isPrefixOf "description"  $ lines $ T.unpack script]
                                         else []
  lowerPart <- saveLibrary classes fileName $ T.unpack script
  return $ Div nullAttr $ upperPart <> lowerPart

-----------------------------------------Program----------------------------------------
includeScript cb@(CodeBlock (label, a@["script",c,outType], opts0) text) = do
  (script,opts) <- extractSource text opts0
  putStrLn $ "===========" <> show opts
  let command = T.unpack c
  files <- listDirectory $ "_build/temp/lib/" <> command
  let (cmd,_,header) = getCommand command files
      s = unlines [header,T.unpack script]
  putStrLn s
  res <- readProcess cmd [] s
  writeScriptResult res (label,a,opts)
includeScript cb@(CodeBlock (a, ("script":_), opts) t) =
  includeScript $ CodeBlock (a,["script","py","md"], opts) t
includeScript cb = return cb

