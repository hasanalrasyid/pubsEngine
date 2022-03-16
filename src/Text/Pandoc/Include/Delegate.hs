module Text.Pandoc.Include.Delegate (doInclude)
  where
import Text.Pandoc
import System.Process
import Text.Pandoc.Definition
import Data.Maybe
import qualified Data.Text as T

doInclude :: Block -> IO Block
doInclude (CodeBlock (_, ("delegate":runner:_), opts) md) = do
    let opt = words $ T.unpack $ fromMaybe "-t latex" $ lookup "o" opts
    tex <- readProcess "multimarkdown" opt $ T.unpack md
    return $ Div nullAttr $ [RawBlock (Format "latex") $ T.pack tex]

doInclude x = return x

