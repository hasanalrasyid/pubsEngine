{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Text.Pandoc.Include.Table (doInclude)
  where
import           Control.Monad
import           Data.List
import           System.Directory

import           Text.Pandoc
import           Text.Pandoc.Error
import           Text.Pandoc.JSON
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Text.Pandoc.JSON
import Text.Pandoc.Walk (walk)
import Text.Pandoc.Include.Common

doInclude :: Block -> IO Block
doInclude cb@(CodeBlock cx@(id, classes, namevals) captionContent)
  | "inputTable" `elem` classes = do
    readTableFile
  where
    readTableFile = case lookup "file" namevals of
       Nothing    -> return cb
       Just f     -> processMdTable cx (T.unpack captionContent) =<< (T.readFile $ T.unpack f)
doInclude x = return x

processMdTable :: Attr -> String -> T.Text -> IO Block
processMdTable cx caption text = do
  c <- runIOorExplode $ readMarkdown param $ T.pack caption
  t <- runIOorExplode $ readMarkdown param text
  let x = takeBlocks $ walk (renewCaption c) t
  return $ Div nullAttr x
    where
      param = def{ readerExtensions = foldr enableExtension pandocExtensions
                     pandocExtSetting
                 }
      renewCaption :: Pandoc -> Block -> Block
      renewCaption (Pandoc _ (nc@(Para _):_)) (Table attr _ b c d e) = Table attr (Caption Nothing [nc]) b c d e
      renewCaption _ tx = tx
      takeBlocks (Pandoc _ b) = b
