module Text.Pandoc.Include.Diagrams (addPackagePGF)
  where
import System.Environment (lookupEnv)
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
import Data.Maybe

--addPackagePGF :: Pandoc -> IO Pandoc
addPackagePGF (Pandoc mt@(Meta mtn) blks) = do
  blks' <- walkM processDiagram blks
  return $ Pandoc mt blks'
--where
--  --mt' = addMetaField "header-includes" (fromList [RawInline (Format "tex") $ T.unlines ["","\\usepackage{tikz}","\\usepackage{tabulary}"]]) mt

processDiagram :: Block -> IO Block
processDiagram cb@(CodeBlock (ident,classes,namevals) contents)
  | elem "diagram" classes = do
    pubsEngineRoot <- lookupEnv "PUBSENGINE_ROOT"
    i <- case pubsEngineRoot of
          Nothing -> pure $ CodeBlock (ident,[],[]) $ T.unlines [ "Figure " <> ident, "ERROR: Full installation of pubsEngine package from compilation is needed for this feature (Diagrams)", contents ]
          _ -> img
    return i
  where
    capt = case lookup "caption" namevals of
             Just f -> f
             Nothing -> "{\\color{red}====No Caption Provided, add caption on diagram statement in md file, i.e. \\{.diagram size=0.6 caption=\"any caption\"\\}}"
    size = fromMaybe "1.0" $ lookup "size" namevals
    img = do
      d <- compileDiagram (T.unpack contents) 800.0
      let imgBlock = RawBlock (Format "latex") $
                       case d of
                        Skipped hash -> T.pack $ unlines
                          [ "\\begin{figure}"
                          , "\\centering"
                          , "\\resizebox{" <> T.unpack size <> "\\linewidth}{!}{\\input{" <> getPGFfilename "auto/" hash <> "}}"
                          , T.unpack $ "\\caption{" <> capt <> "}"
                          , T.unpack $ "\\label{"<> ident <> "}"
                          , "\\end{figure}"
                          ]
                        OK _ texnya -> T.pack $ LB.unpack $ BSB.toLazyByteString texnya
                                        {- unlines  [ "\\begin{figure}"
                                                 , "\\centering"
                                                 , LB.unpack $ toLazyByteString texnya
                                                 , "\\caption{" ++ capt ++ "}"
                                                 , "\\end{figure}"
                                                 ] -}
                        ParseErr err  -> T.pack $ "\\begin{verbatim}\n" ++ "ParseErr : " ++ err ++ "\\end{verbatim}\n"
                        InterpErr err -> T.pack $ "\\begin{verbatim}\n" <> "InterpErr : " <> ppInterpError err <> T.unpack contents <> "\\end{verbatim}\n"
      return $ Div nullAttr [imgBlock]

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
