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

import Diagrams.Builder.Modules
import Language.Haskell.Exts.Simple -- Module
import Data.Hashable -- hashWithSalt
import System.Directory -- doesFileExist

--addPackagePGF pd = addPackagePGF' processDiagram pd

--addPackagePGF :: Pandoc -> IO Pandoc
addPackagePGF (Pandoc mt@(Meta mtn) blks) = do
  blks' <- blks''
  return $ Pandoc mt' blks'
  where
    mt' = addMetaField "header-includes" (fromList [RawInline (Format "tex") $ unlines ["\\usepackage{tikz}","\\usepackage{tabulary}"]]) mt
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
                             Skipped hash -> "\\input{" ++ getPGFfilename "Figures/" hash ++ "}"
                             OK _ texnya -> LB.unpack $ toLazyByteString texnya
                                            {- unlines  [ "\\begin{figure}"
                                                     , "\\centering"
                                                     , LB.unpack $ toLazyByteString texnya
                                                     , "\\caption{" ++ capt ++ "}"
                                                     , "\\end{figure}"
                                                     ] -}
                             ParseErr err -> "\\begin{verbatim}\n" ++ "ParseErr : " ++ err ++ "\\end{verbatim}\n"
                             InterpErr err -> "\begin{verbatim}\n" ++ "InterpErr : " ++ ppInterpError err ++ contents ++ "\\end{verbatim}\n"
          return $ Div (ident,[],[("label",capt)]) [imgBlock,Para [Str capt]]
    --bl' = CodeBlock (ident, delete "diagram" classes, namevals) contents

processDiagram block = return block

getPGFfilename d h = d ++ hashToHexStr h ++ ".pgf"

compileDiagram xDia dWidth = do
  let standaloneTex = False
  let bopts = mkBuildOpts PGF (zero :: V2 Double)
                (PGFOptions def (mkWidth dWidth) False standaloneTex)
                  & snippets .~ []
                  & imports .~ [ "Diagrams.Backend.PGF" , "Diagrams.TwoD.Arrow" ]
                  & diaExpr .~ xDia
                  & decideRegen .~ alwaysRegenerate
  buildDiagram' bopts

buildDiagram' bopts = do
  case createModule Nothing bopts of
    Left err -> return $ ParseErr $ "buildDiagram' err: " ++ err
    Right m@(Module _ _ srcImps _) -> do
      let diaHash
            = 0 `hashWithSalt` prettyPrint m
                `hashWithSalt` (bopts ^. diaExpr)
                `hashWithSalt` (bopts ^. backendOpts)
                `hashWithSalt` "_build"
      let f = getPGFfilename "Figures/" diaHash
      isCompiled <- doesFileExist f
      case isCompiled of
        True -> return $ Skipped diaHash
        False -> do
          d <- buildDiagram bopts
          case d of
            OK _ texnya -> do
                            LB.writeFile f $ toLazyByteString texnya
                            return $ Skipped diaHash
            _           -> return d
