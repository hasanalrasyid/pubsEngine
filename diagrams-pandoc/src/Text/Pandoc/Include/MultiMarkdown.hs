{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Text.Pandoc.Include.MultiMarkdown (doInclude)
  where
--import Text.Pandoc.JSON
--import Text.Pandoc.Shared (addMetaField)
--import Text.Pandoc.Builder (fromList)
--import System.Process (callCommand)
--import Data.Maybe
--import System.FilePath.Posix (takeFileName)
import Text.Pandoc
--import qualified Data.Text as T
--import qualified Data.Text.IO as T
--import Text.Pandoc.Include.Common
import System.Process
import Text.Pandoc.Definition

doInclude :: Block -> IO Block
doInclude (CodeBlock (_, classes, _) md)
  | "multiTable" `elem` classes = do
    tex <- readProcess "multimarkdown" ["-t","latex"] md
    return $ Div nullAttr $ [RawBlock (Format "latex") tex]

doInclude x = return x

