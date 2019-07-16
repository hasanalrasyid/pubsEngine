{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Text.Pandoc.Include.Delegate (doInclude)
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
import Data.Maybe

doInclude :: Block -> IO Block
doInclude (CodeBlock (_, (classes:runner:_), opts) md)
  | "delegate" == classes = do
    let run = case runner of
                [] -> "multimarkdown"
                b  -> b
    let opt = words $ fromMaybe "-t latex" $ lookup "o" opts
    tex <- readProcess run opt md
    return $ Div nullAttr $ [RawBlock (Format "latex") tex]

doInclude x = return x

