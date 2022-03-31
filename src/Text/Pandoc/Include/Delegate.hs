module Text.Pandoc.Include.Delegate (doInclude)
  where
import Text.Pandoc
import System.Process
import Text.Pandoc.Definition
import Data.Maybe
import qualified Data.Text as T
import Data.List (deleteBy)
import Text.Pandoc.Include.Common

doInclude :: Block -> IO Block
doInclude (CodeBlock (label, ("delegate":runner:_), opts) md) = do
    let opt = words $ T.unpack $ fromMaybe "--nolabels -t latex" $ lookup "o" opts
    let notestr = lookup "notes" opts
    notes <- case notestr of
               Nothing -> return Null
               Just s -> do
                 (Pandoc _ r) <- runIOorExplode $ readMarkdown mdOption s
                 return $ Div nullAttr $
                  [ RawBlock (Format "latex") $ T.unlines [ "\\begin{minipage}{\\linewidth}"
                                                          , "\\scriptsize"
                                                          ]
                  ] <> r
                    <> [RawBlock (Format "latex") "\\end{minipage}"]
    (beginTable:tailTable) <- fmap (T.lines . T.pack) $ readProcess "multimarkdown" opt $ T.unpack md
    let tex0 = T.unlines $ (beginTable <> "\\label{" <> label <> "}") : (deleteBy T.isPrefixOf "\\label{" tailTable)
    let (texUp,texDown) = T.breakOnEnd "\\end{tabular}" tex0
    return $ Div nullAttr
      [ RawBlock (Format "latex") texUp
      , notes
      , RawBlock (Format "latex") texDown]

doInclude x = return x

