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
--import Text.Pandoc.Definition (fromString)
import Data.Version (showVersion)
import Text.Pandoc.Walk (walk)

doInclude :: Block -> IO Block
doInclude cb@(CodeBlock cx@(id, classes, namevals) captionContent)
  | "inputTable" `elem` classes = do
    readTableFile
  where
    readTableFile = case lookup "file" namevals of
       Nothing    -> return cb
       Just f     -> processMdTable cx captionContent =<< T.readFile f
doInclude x = return x

processMdTable :: Attr -> String -> T.Text -> IO Block
processMdTable cx caption text = do
  c <- runIOorExplode $ readMarkdown param $ T.pack caption
  t <- runIOorExplode $ readMarkdown param text
  let x = takeBlocks $ walk (renewCap c) t
  return $ Div nullAttr x
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
