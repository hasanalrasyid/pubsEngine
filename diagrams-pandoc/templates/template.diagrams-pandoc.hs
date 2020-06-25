#!/usr/bin/env stack
--stack --resolver lts-11.9  -v --install-ghc runghc --package diagrams-pandoc --stack-yaml xxxLocxxx/stack.yaml


{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

import Diagrams.Prelude
import Diagrams.Backend.PGF.CmdLine

import Text.Pandoc.JSON
import Data.List (delete)
import Diagrams.Backend.PGF

import Text.Pandoc.Shared (addMetaField)
import Text.Pandoc.Builder (fromList)
import Text.Pandoc.Walk (walkM)

import qualified Data.ByteString.Lazy.Char8   as LB
import Diagrams.Backend.PGF.Render
import Diagrams.Size
import Data.ByteString.Builder

import System.Process (readProcess)

main :: IO ()
main = toJSONFilter addPackagePGF

addPackagePGF :: Pandoc -> IO Pandoc
addPackagePGF (Pandoc mt@(Meta mtn) blks) = do
  blks' <- blks''
  return $ Pandoc mt' blks'
  where
    mt' = addMetaField "header-includes" (fromList [RawInline (Format "tex") "\\usepackage{tikz}"]) mt
    blks'' = walkM processDiagram blks

processDiagram :: Block -> IO Block
processDiagram cb@(CodeBlock (ident,classes,namevals) contents)
  | elem "diagram" classes = do
    i <- img
    return i
  where
    capt = case lookup "caption" namevals of
             Just f -> f
             Nothing -> "{\\color{red}====No Caption Provided, add caption on diagram statement in md file, i.e. \\{.diagram width=100 caption=\"any caption\"\\}}"
    width = case lookup "width" namevals of
              Just w -> w
              Nothing -> show 10
    img = do
          d <- compileDiagram contents width
          return $ case d of
                     Left _err -> Null
                     Right rawPGFimage -> RawBlock (Format "latex") $
                       unlines  [ "\\begin{figure}"
                                , "\\centering"
                                , rawPGFimage
                                , "\\caption{" ++ capt ++ "}"
                                , "\\end{figure}"
                                ]
    --bl' = CodeBlock (ident, delete "diagram" classes, namevals) contents

processDiagram block = return block

--compileDiagram x = return $ Right "this is from Right have to moved to diagrams"
--compileDiagram x = return $ Right "\\input{pgfpicture.pgf}"
compileDiagram xDia sWidth = do
  texnya <- readProcess "diagram-pandoc.sh" ["-w", sWidth ] xDia
  --texnya <- texDiaString def example
  return $ Right texnya

{-
texDiaString :: (TypeableFloat n, Monoid' m)
              => Options PGF V2 n -> QDiagram PGF V2 n m -> IO String
texDiaString opts d =
  return $ LB.unpack $ toLazyByteString $ renderDia PGF opts d

example :: Diagram PGF
example = square 30 # lwG 0.05
-}
