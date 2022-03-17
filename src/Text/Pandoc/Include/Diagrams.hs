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
import Diagrams.Backend.PGF.Render as PGF
import Diagrams.Size
import Data.ByteString.Builder as BSB

import System.Process (readProcess)

import Diagrams.Builder

import Diagrams.Builder.Modules
import Language.Haskell.Exts.Simple -- Module
import Data.Hashable -- hashWithSalt
import System.Directory -- doesFileExist
import qualified Data.Text as T
import qualified Data.Text.Read as T

--addPackagePGF pd = addPackagePGF' processDiagram pd

--addPackagePGF :: Pandoc -> IO Pandoc
addPackagePGF (Pandoc mt@(Meta mtn) blks) = do
  blks' <- blks''
  return $ Pandoc mt' blks'
  where
    mt' = addMetaField "header-includes" (fromList [RawInline (Format "tex") $ T.unlines ["","\\usepackage{tikz}","\\usepackage{tabulary}"]]) mt
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
              Just w -> case T.double w of -- :: Double
                          Left err -> error err
                          Right (a,_) -> a
              Nothing -> 10
    img = do
      d <- compileDiagram (T.unpack contents) width
      let imgBlock = RawBlock (Format "latex") $
                       case d of
                        Skipped hash -> T.pack $ "\\input{" <> getPGFfilename "_build/auto/" hash <> "}"
                        OK _ texnya ->T.pack "try this" --  T.pack $ LB.unpack $ BSB.toLazyByteString texnya
                                        {- unlines  [ "\\begin{figure}"
                                                 , "\\centering"
                                                 , LB.unpack $ toLazyByteString texnya
                                                 , "\\caption{" ++ capt ++ "}"
                                                 , "\\end{figure}"
                                                 ] -}
                        ParseErr err  -> T.pack $ "\\begin{verbatim}\n" ++ "ParseErr : " ++ err ++ "\\end{verbatim}\n"
                        InterpErr err -> T.pack $ "\\begin{verbatim}\n" <> "InterpErr : " <> ppInterpError err <> T.unpack contents <> "\\end{verbatim}\n"
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
      let (diaHash :: Int) = hashWithSalt 0 $ unlines [ prettyPrint m
                                                      , (bopts ^. diaExpr)
                                                      , (bopts ^. backendOpts.surface.command)
                                                      , (unwords $ bopts ^. backendOpts.surface.arguments)
                                                      , "_build"
                                                      ]
      let f = getPGFfilename "_build/auto/" diaHash
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
