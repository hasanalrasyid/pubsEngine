{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Text.Pandoc.Include.MultiMarkdown (doInclude)
  where
import Text.Pandoc
import System.Process
import Text.Pandoc.Definition
import qualified Text.Pandoc.Include.Delegate as IDel

doInclude :: Block -> IO Block
doInclude (CodeBlock (a, classes, b) md)
  | "multiTable" `elem` classes =
      IDel.doInclude (CodeBlock (a, ("delegate":"multimarkdown":classes), b) md)

doInclude x = return x

