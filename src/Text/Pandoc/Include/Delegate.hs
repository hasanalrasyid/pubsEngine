module Text.Pandoc.Include.Delegate (doInclude)
  where
import Text.Pandoc
import System.Process
import Text.Pandoc.Definition
import Data.Maybe
import qualified Data.Text as T
import Data.List (deleteBy)

doInclude :: Block -> IO Block
doInclude (CodeBlock (label, ("delegate":runner:_), opts) md) = do
    let opt = words $ T.unpack $ fromMaybe "--nolabels -t latex" $ lookup "o" opts
    (beginTable:tailTable) <- fmap (T.lines . T.pack) $ readProcess "multimarkdown" opt $ T.unpack md
    let tex = T.unlines $ (beginTable <> "\\label{" <> label <> "}") : (deleteBy T.isPrefixOf "\\label{" tailTable)
    return $ Div nullAttr $ [RawBlock (Format "latex") tex]

doInclude x = return x

