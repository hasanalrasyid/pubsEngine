{-# LANGUAGE QuasiQuotes #-}
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
import Data.String.QQ

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
    label = case lookup "label" namevals of
              Just w -> w
              Nothing -> "labelNull"
    img = do
          d <- compileDiagram contents width
          let imgBlock =
                           case d of
                             ParseErr err -> "ParseErr : " ++ err
                             InterpErr err -> "InterpErr : " ++ ppInterpError err ++ contents
                             Skipped hash -> "\\input{" ++ getPGFfilename "Figures/" hash ++ "}"
                             OK _ texnya -> LB.unpack $ toLazyByteString texnya
          let imgBlockFin = RawBlock (Format "latex") $ unlines  [ "\\begin{figure}"
                                                     , "\\centering"
                                                     , imgBlock
                                                     , "\\caption{" ++ capt ++ "}"
                                                     , "\\label{" ++ label ++ "}"
                                                     , "\\end{figure}"
                                                     ]
          return $ Div  (ident,[],[("label",label)]) [imgBlockFin]
    --bl' = CodeBlock (ident, delete "diagram" classes, namevals) contents

processDiagram block = return block

getPGFfilename d h = d ++ hashToHexStr h ++ ".pgf"

barSnippet :: String
barSnippet = [s|
{-
barStack :: [String] -> [(String,[Double])] -> String -> IO (QDiagram PGF V2 Double Any)
barStack titles values y_title = do
       let
         target = C.plot_bars_titles .~ titles
                $ C.plot_bars_style .~ C.BarsStacked
                $ C.plot_bars_values .~ values
                $ def
         cb1 = do
               C.layout_all_font_styles . C.font_size .= 24
               C.layout_y_axis . C.laxis_title .= y_title
               C.layout_x_axis . C.laxis_generate .= C.autoIndexAxis (map fst values)
               C.plot $ C.plotBars target -- $ C.bars titles (C.addIndexes (map snd values))
         cb = C.render (C.toRenderable (C.execEC cb1))(C._fo_size def)
       fontSelector <- C._fo_fonts def
       let env = C.createEnv C.vectorAlignmentFns 800 600 fontSelector
       let (res,_) = C.runBackend env cb
       return $ res :: IO (QDiagram PGF V2 Double Any)
-}
genBarStack titles vals = C.liftEC $ do
    styles <- sequence [fmap mkStyle C.takeColor | _ <- titles]
    C.plot_bars_titles      .= titles
    C.plot_bars_values      .= vals
    C.plot_bars_style       .= C.BarsClustered
    C.plot_bars_spacing     .= C.BarsFixGap 30 5
    C.plot_bars_item_styles .= styles
  where
    mkStyle c = (C.solidFillStyle c, Just (C.solidLine 1.0 $ opaque black))
--barChart :: [String] -> [(String,[Double])] -> String -> IO (QDiagram PGF V2 Double Any)
barChart :: [String] -> [(String,[Double])] -> String -> IO (QDiagram PGF V2 Double Any)
barChart t v y = barChartPrime t v y C.bars
barStack :: [String] -> [(String,[Double])] -> String -> IO (QDiagram PGF V2 Double Any)
barStack t v y = barChartPrime t v y genBarStack

barChartPrime titles values y_title genBars = do
       let
         cb1 = do
               C.layout_all_font_styles . C.font_size .= 24
               C.layout_y_axis . C.laxis_title .= y_title
               C.layout_x_axis . C.laxis_generate .= C.autoIndexAxis (map fst values)
               C.plot $ fmap C.plotBars $ genBars titles (C.addIndexes (map snd values))
         cb = C.render (C.toRenderable (C.execEC cb1))(C._fo_size def)
       fontSelector <- C._fo_fonts def
       let env = C.createEnv C.vectorAlignmentFns 800 600 fontSelector
       let (res,_) = C.runBackend env cb
       return $ res :: IO (QDiagram PGF V2 Double Any)
|]

compileDiagram xDia dWidth = do
  let standaloneTex = False
  let bopts = mkBuildOpts PGF (zero :: V2 Double)
                (PGFOptions def (mkWidth dWidth) False standaloneTex)
                  & snippets .~ [ barSnippet ]
                  & pragmas .~ [ "FlexibleContexts" ]
                  & imports .~ [ "Diagrams.Backend.PGF" , "Diagrams.TwoD.Arrow", "Data.List" ]
                  & qimports .~ [ ("Graphics.Rendering.Chart.State", "C")
                                , ("Graphics.Rendering.Chart.Easy", "C")
                                , ("Graphics.Rendering.Chart.Backend.Diagrams", "C")
                                ]
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
