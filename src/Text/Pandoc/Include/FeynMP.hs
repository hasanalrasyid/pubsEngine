{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Text.Pandoc.Include.FeynMP (doInclude)
  where
import System.Process
import Text.Pandoc.Definition
import Data.Maybe
import System.Directory
import System.FilePath
import Data.Hashable
import Diagrams.Builder (hashToHexStr)
import qualified Data.Text as T
import Text.Pandoc.Process
import Text.Pandoc.UTF8 as UTF8
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as B8
import Text.Pandoc.Include.Utils

doInclude :: Block -> IO Block
doInclude (CodeBlock (label, classes, opts) mp)
  | "feynmp" `elem` classes = do
      let mpHash = getHash mp
          caption = lookup "caption" opts
      let tex = BL.fromStrict $ UTF8.fromString $ unlines [ "\\documentclass{standalone}"
                        , "\\usepackage{amsmath}"
                        , "\\usepackage{feynmp-auto}"
                        , "\\begin{document}"
                        , "\\unitlength = 1mm"
                        , "\\begin{fmffile}{"++ mpHash ++ "}"
                        , "\\begin{math}"
                        , T.unpack mp
                        , "\\end{math}"
                        , "\\end{fmffile}"
                        , "\\end{document}"
                        ]
      let outFile = "_build/auto" </> mpHash <.> "pdf"
      isCompiled <- doesFileExist outFile
      B8.putStrLn tex
      if isCompiled then return ()
        else inDir ("_build/temp") $ do
          B8.writeFile (mpHash <.> "tex") tex
          callCommand $ unlines [ "pdflatex " <> mpHash
                                , "mpost " <> mpHash
                                , "pdflatex " <> mpHash
                                , "mv " <> mpHash <> ".pdf ../auto" </> mpHash <.> "pdf"
                                ]
      return $ Div nullAttr
                $ [Para
                    [Image (label,[],opts)
                      [Str $ fromMaybe "FenymanDiagram" caption] (T.pack mpHash, label)
                    ]
                  ]

doInclude x = return x

