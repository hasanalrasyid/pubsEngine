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
import qualified Data.Text.Read as T
import Data.Either

doInclude :: Block -> IO Block
doInclude (CodeBlock (label, ["mermaid"], opts) md) = do
  let fileName = fromMaybe "mermaid" $ lookup "file" opts
      size = fromRight 0.6  $ fmap fst $ fromMaybe (Right (0.6,"")) $ T.double <$> lookup "size" opts
      width = floor $ 2480.0 * size
      height = floor $ (fromIntegral $ 600 * width) / 800.0
  T.writeFile (T.unpack $ "_build/temp/" <> fileName <> ".mmd") md
  callCommand $ unwords [ "mermaid -f -w", show width,"-H", show height," -i _build/temp/" <> T.unpack fileName <> ".mmd -o _build/auto/" <> T.unpack fileName <> ".pdf" ]
  let caption = case lookup "caption" opts of
                  Nothing -> []
                  Just a -> [Str a]
  return $ Div nullAttr [ Para [Image (label,[],opts) caption (fileName,label)]]

doInclude x = return x

