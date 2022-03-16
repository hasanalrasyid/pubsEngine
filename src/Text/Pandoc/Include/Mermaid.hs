{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Text.Pandoc.Include.Mermaid (doInclude)
  where
import Text.Pandoc
import System.Process
import Text.Pandoc.Definition
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as T

doInclude :: Block -> IO Block
doInclude (CodeBlock (label, ["mermaid"], opts) md) = do
  let fileName = case lookup "file" opts of
                    Nothing -> "_build/output"
                    Just a -> T.unpack a
      width  = fromMaybe "800" $ lookup "width" opts
      height = fromMaybe "600" $ lookup "height" opts
  T.writeFile (fileName ++ ".mmd") md
  _ <- readProcess "mermaid" ["-w", T.unpack width,"-H", T.unpack height,"-i", fileName ++ ".mmd", "-o", fileName ++ ".png" ] []
  let caption = fromMaybe "No caption" $ lookup "caption" opts
  return $ Div nullAttr [ Para [Image (label,[],opts) [Str caption] ((T.pack $ fileName ++ ".png"),label)]]

doInclude x = return x

