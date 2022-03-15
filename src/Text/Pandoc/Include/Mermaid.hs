{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Text.Pandoc.Include.Mermaid (doInclude)
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
doInclude (CodeBlock (label, ["mermaid"], opts) md) = do
  let fileName = case lookup "file" opts of
                    Nothing -> "_build/output"
                    Just a -> a
      width  = fromMaybe "800" $ lookup "width" opts
      height = fromMaybe "600" $ lookup "height" opts
  writeFile (fileName ++ ".mmd") md
  _ <- readProcess "mermaid" ["-w", width,"-H", height,"-i", fileName ++ ".mmd", "-o", fileName ++ ".png" ] []
  let caption = fromMaybe "No caption" $ lookup "caption" opts
  return $ Div nullAttr [ Para [Image (label,[],opts) [Str caption] (fileName ++ ".png",label)]]

--  | "delegate" == classes = do
--    --let opt = words $ fromMaybe "-t latex" $ lookup "o" opts
--    --tex <- readProcess "mermaid" opt md
--    return $ Div nullAttr $ [RawBlock (Format "latex") tex]

doInclude x = return x

