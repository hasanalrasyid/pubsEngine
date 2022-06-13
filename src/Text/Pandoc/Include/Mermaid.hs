{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Text.Pandoc.Include.Mermaid (doInclude)
  where
import Text.Pandoc
import System.Process
import Text.Pandoc.Definition
import Text.Pandoc.Walk
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Read as T
import Data.Either
import Text.Pandoc.Include.Utils
import System.FilePath

doBlock :: Block -> IO Block
doBlock (CodeBlock (label, ["mermaid"], opts) md) = do
  let caption = case lookup "caption" opts of
                  Nothing -> []
                  Just a -> [Str a]
  img <- doMermaid label opts md caption
  return $ Div nullAttr [ Para [img]]
doBlock x = walkM doInline x

doInline (Image (label, ["mermaid"], opts0) caption _) = do
  (md,opts) <- extractSource "ERROR: No source" opts0
  doMermaid label opts md caption
doInline i = pure i

doInclude b = walkM doBlock b

doMermaid label opts md caption = do
  let mpHash = getHash md
      fileName = (takeBaseName $ T.unpack $ fromMaybe "mermaid" $ lookup "src" opts) <> mpHash
      size = fromRight 0.6  $ fmap fst $ fromMaybe (Right (0.6,"")) $ T.double <$> lookup "size" opts
      width = floor $ 2480.0 * size
      height = floor $ (fromIntegral $ 600 * width) / 800.0
  putStrLn $ "===========" <> show fileName
  T.writeFile ("_build/temp/" <> fileName <> ".mmd") md
  callCommand $ unwords [ "mermaid -f -w", show width,"-H", show height," -i _build/temp/" <> fileName <> ".mmd -o _build/auto/" <> fileName <> ".pdf" ]
  pure $ Image (label,[],opts) caption (T.pack fileName,label)
