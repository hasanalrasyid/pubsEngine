{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Text.Pandoc.Include.Diagrams (addPackagePGF)
  where
import Diagrams.Prelude
import Diagrams.Backend.PGF.CmdLine

import Text.Pandoc.JSON
import Data.List (delete)
import Diagrams.Backend.PGF

import Text.Pandoc.Shared (addMetaField)
import Text.Pandoc.Builder (fromList)
import Text.Pandoc.Walk

import qualified Data.ByteString.Lazy.Char8   as LB
import Diagrams.Backend.PGF.Render
import Diagrams.Size
import Data.ByteString.Builder

import System.Process (readProcess)

import Diagrams.Builder

--addPackagePGF pd = addPackagePGF' processDiagram pd

--addPackagePGF :: Pandoc -> IO Pandoc
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
              Just w -> read w :: Double
              Nothing -> 10
    img = do
          d <- compileDiagram contents width
          let imgBlock = RawBlock (Format "latex") $
                           case d of
                             ParseErr err -> "ParseErr : " ++ err
                             InterpErr err -> "InterpErr : " ++ ppInterpError err
                             Skipped hash -> "Skipped : " ++ hashToHexStr hash
                             OK _ texnya -> LB.unpack $ toLazyByteString texnya
                                            {- unlines  [ "\\begin{figure}"
                                                     , "\\centering"
                                                     , LB.unpack $ toLazyByteString texnya
                                                     , "\\caption{" ++ capt ++ "}"
                                                     , "\\end{figure}"
                                                     ] -}
          return $ Div (ident,[],[("label",capt)]) [imgBlock,Para [Str capt]]
    --bl' = CodeBlock (ident, delete "diagram" classes, namevals) contents

processDiagram block = return block

compileDiagram xDia dWidth = do
  let standaloneTex = False
  let bopts = mkBuildOpts PGF (zero :: V2 Double)
                (PGFOptions def (mkWidth dWidth) False standaloneTex)
                  & snippets .~ []
                  & imports .~ [ "Diagrams.Backend.PGF" ]
                  & diaExpr .~ xDia
                  & decideRegen .~ alwaysRegenerate
  buildDiagram bopts

  {-
  texnya <- readProcess "diagram-pandoc.sh" ["-w", sWidth ] xDia
  --texnya <- texDiaString def example
  return $ Right texnya
-}

{-
texDiaString :: (TypeableFloat n, Monoid' m)
              => Options PGF V2 n -> QDiagram PGF V2 n m -> IO String
texDiaString opts d =
  return $ LB.unpack $ toLazyByteString $ renderDia PGF opts d

example :: Diagram PGF
example = square 30 # lwG 0.05
-}
