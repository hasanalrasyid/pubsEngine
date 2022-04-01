{-
The MIT License (MIT)

Copyright (c) 2015 Dániel Stein <daniel@stein.hu>

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be included
in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
-}

{-|
A Pandoc filter that replaces include labeled Code Blocks with the contents of
the referenced files. Even nested, recursive includes.

Based on the scripting tutorial for Pandoc:
http://pandoc.org/scripting.html#include-files

The Code Blocks like the following will include every file in a new line. The
reference paths should be either absolute or relative to the folder where the
pandoc command will be executed.

> ```include
> /absolute/file/path.md
> relative/to/the/command/root.md
> #do/not/include/this.md
> ```

If the file does not exist, it will be skipped completely. No warnings, no
residue, nothing. Putting an # as the first character in the line will make the
filter skip that file.

For now the nested includes only work for two levels, after that the source
will be inserted and not parsed.

Note: the metadata from the included source files are discarded.

-}

module Text.Pandoc.Include.Markdown

  where

import           Control.Monad
import           Data.List
import           System.Directory

import qualified Data.Text.IO as T
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import           Text.Pandoc
import           Text.Pandoc.Error
import           Text.Pandoc.JSON

import           Text.Pandoc.Walk
import qualified Text.Pandoc.Class as PIO
import Text.Pandoc.Include.Common
import Data.Maybe

getContent :: FilePath -> IO [Block]
getContent file = do
  c <- T.readFile file
  genPandoc c

genPandoc :: T.Text -> IO [Block]
genPandoc c = do
  (Pandoc _ b) <- runIOorExplode $ readMarkdown param c
  --let b1 = walk divBlocks b
  let b1 = b
  return b1
    where
      meta' = nullMeta -- autoEqnLabels True
      param = def { readerExtensions = foldr enableExtension pandocExtensions
                    pandocExtSetting
                  }

getProcessableFileList :: T.Text -> IO [FilePath]
getProcessableFileList list = do
  let f = lines $ T.unpack list
  let files = filter (\x -> not $ "#" `isPrefixOf` x) f
  filterM doesFileExist files

processFiles :: [FilePath] -> IO Block
processFiles toProcess = do
  b <- fmap concat (mapM getContent toProcess)
  return $ Div nullAttr b

doInclude :: Block -> IO Block
doInclude (CodeBlock (_, classes, _) list)
  | "include" `elem` classes = do
    let toProcess = getProcessableFileList list
    processFiles =<< toProcess

doInclude x = return x

includeMarkdown :: Block -> PandocIO Block
includeMarkdown cb@(CodeBlock (label, ["include"], _) t) =
  includeMarkdownImpl $ lines $ T.unpack t
includeMarkdown cb@(Para [Cite [c] _]) =
  includeMarkdownImpl $ catMaybes [ fmap T.unpack $ T.stripPrefix "include:" $ citationId c ]
includeMarkdown x = return x

includeMarkdownImpl [] = error $ "includeMarkdown: we have no file to include"
includeMarkdownImpl fileList = do
  (inMd :: [[Block]]) <- flip mapM fileList $ \f -> do
    fMd <- TE.decodeUtf8 <$> PIO.readFileStrict (f <> ".md")
    r <- readMarkdown mdOption fMd
    (Pandoc _ s) <- walkM includeMarkdown r
    return s
  return $ Div nullAttr $ concat inMd
