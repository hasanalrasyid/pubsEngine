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
import qualified Text.Pandoc.Include.Delegate as IDel

doInclude :: Block -> IO Block
doInclude (CodeBlock (a, classes, b) md)
  | "multiTable" `elem` classes =
      IDel.doInclude (CodeBlock (a, ("delegate":"multimarkdown":classes), b) md)

doInclude x = return x

