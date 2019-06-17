#!/usr/bin/env stack
--stack --resolver lts-11.9  --install-ghc runghc --package diagrams-pandoc --stack-yaml /home/aku/kanazawa/report/pubsEngine/diagrams-pandoc/stack.yaml

import qualified Text.Pandoc.Include.Table as IT
import qualified Text.Pandoc.Include.Thesis as IH
import qualified Text.Pandoc.Include.CrossRef as IC
import qualified Text.Pandoc.Include.Markdown as IM
import qualified Text.Pandoc.Include.Diagrams as ID
import           Text.Pandoc.JSON
import Text.Pandoc.Walk

import Text.Pandoc.CrossRef
import Data.Monoid ((<>))

{-
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

import           Control.Monad
import           Data.List
import           System.Directory

import           Text.Pandoc
import           Text.Pandoc.Error
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Text.Pandoc.JSON
--import Text.Pandoc.Definition (fromString)
import Data.Version (showVersion)

doInclude :: Block -> IO [Block]
doInclude cb@(CodeBlock cx@(id, classes, namevals) captionContent)
  | "inputTable" `elem` classes = do
    readTableFile
  where
    readTableFile = case lookup "file" namevals of
       Nothing    -> return [cb]
       Just f     -> processMdTable cx captionContent =<< T.readFile f
doInclude x = return [x]

processMdTable :: Attr -> String -> T.Text -> IO [Block]
processMdTable cx caption text = do
  c <- runIOorExplode $ readMarkdown param $ T.pack caption
  t <- runIOorExplode $ readMarkdown param text
  let x = takeBlocks $ walk (renewCap c) t
  return x
    where
      param = def{ readerExtensions = foldr enableExtension pandocExtensions
                    [ Ext_tex_math_dollars
                    , Ext_raw_tex
                    , Ext_table_captions
                    ]
                 }
      renewCap (Pandoc _ ((Para nc):_)) (Table _ a b c d) = Table nc a b c d
      renewCap _ tx = tx
      takeBlocks (Pandoc _ b) = b
-}

doThemAll (Pandoc mt blks) = do
  blks' <- walkM doBlock blks
  p <- doPandoc (Pandoc mt blks')
  return p

doPandoc p = doCrossRef =<< ID.addPackagePGF =<< IH.linkTex p

doCrossRef p@(Pandoc meta blocks) = do
--  runCrossRefIO meta (Just $ Format "latex") defaultCrossRefAction p
  b <- runCrossRefIO meta' (Just $ Format "latex") crossRefBlocks blocks
  return $ Pandoc meta b
    where
      meta' = autoEqnLabels True <> meta

doBlock :: Block -> IO Block
doBlock cb@(CodeBlock (_, classes, _) _)
  | "inputTable" `elem` classes = IT.doInclude cb
  | "include" `elem` classes = IM.doInclude cb
doBlock x = return x

main :: IO ()
main = toJSONFilter doThemAll
