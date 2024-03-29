{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Text.Pandoc.Include.Thesis
  ( linkTex
  , processPreDoc
  , processPostDoc
  )
  where
import Text.Pandoc.JSON
import Text.Pandoc.Shared (addMetaField)
import Text.Pandoc.Builder (fromList)
import System.Process (callCommand)
import Data.Maybe
import System.FilePath.Posix (takeFileName)
import Text.Pandoc
import Text.Pandoc.Walk
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import Text.Pandoc.Include.Common
import Text.Pandoc.Include.Utils
import qualified Text.Pandoc.Class as PIO
import qualified Data.Map.Strict as M
import Data.Map (Map)
import Control.Monad.IO.Class (liftIO)

mdFile2LaTeX :: FilePath -> IO ()
mdFile2LaTeX f = do
  let fn = takeFileName f
  putStrLn $ "mdFile2LaTeX: " <> fn
  T.readFile (f ++ ".md") >>= md2LaTeX >>= T.writeFile ("_build/" ++ fn ++ ".tex")

md2LaTeX :: T.Text -> IO T.Text
md2LaTeX t = runIOorExplode $
  let param = def { readerExtensions = foldr enableExtension pandocExtensions pandocExtSetting }
   in readMarkdown param t >>= writeLaTeX (def { writerReferenceLinks = True
                                          , writerTopLevelDivision = TopLevelChapter
                                          })

linkTex :: Pandoc -> IO Pandoc
linkTex p@(Pandoc mt blks) = do
  let md@(mdappendix:mdquote:mdacknowledgements:_) = map ((flip lookupMeta) mt) ["mdappendix","mdquote","mdacknowledgements"]
  let mdall = concat $ map fromMetaInlines_Str $ catMaybes md
  mapM_ mdFile2LaTeX mdall
  let mt' = genMeta mt [ ("auto-appendix",[mdappendix])
                       , ("auto-acknowledgements",[mdacknowledgements])
                       , ("auto-quote",[mdquote])
                       ]
  return $ Pandoc mt' blks
    where
      genMeta r0 ((s,ts):sts) =
        let rM = addMetaField s (fromList $ map genRawBlock ts) r0
         in genMeta rM sts
      genMeta res [] = res
      genRawBlock (Just a) = RawBlock (Format "latex") $ T.unlines  [ t | s <- fromMetaInlines_Str a
                                                                      , let u = takeFileName s
                                                                      , let t = T.pack $ "\\input{" ++ u ++ "}"
                                                                      ]
      genRawBlock Nothing = Null
      genPerintah x s = "../" ++ s ++ x
      fromMetaInlines_Str (MetaInlines a) = map fromStr a
      fromMetaInlines_Str (MetaBlocks ((Plain a):_)) = filter (not . null) $ map fromStr a
      fromMetaInlines_Str _ = ["fromMetaInlines_Str failed"]
      fromStr (Str a) = T.unpack a
      fromStr SoftBreak = ""
      fromStr _ = "fromStr failed"


-- Start:
-- convert meta appendix to div inside the pandoc [block]
processPreDoc p@(Pandoc m l) = do
  let facilities = case lookupMeta "facilities" m of
                  Just (MetaInlines a) -> [ Div ("",["facilities"],[]) [Plain a]]
                  _ -> []
  let software = case lookupMeta "software" m of
                  Just (MetaInlines a) -> [ Div ("",["software"],[]) [Plain a]]
                  _ -> []
  let acknowledgements = case lookupMeta "acknowledgements" m of
                  Just (MetaInlines a) -> [Div ("",["acknowledgements"],[]) [Plain a]]
                  _ -> []
  let appendix = case lookupMeta "appendix" m of
                  Just (MetaList a) -> [ Div ("",["appendix"],[])
                          [ RawBlock (Format "latex") "%%%%%%%%%APPENDIX%%%%%%%%%"
                          , CodeBlock ("",["include"],[]) $ T.unlines $ flip map (concat $ flip map a $ \(MetaInlines s) -> s) $ \(Str f) -> f
                          ]
                        ]
                  _ -> []
  return (Pandoc m $ l <> acknowledgements <> facilities <> software <> appendix)

processPostDoc p@(Pandoc m b) =
  let facilities = query getFacilities  b
      software   = query getSoftware    b
      appendix   = query getAppendix    b
      dedicatory = query getDedicatory  b
      acknowledgements = query getAck   b
      newMeta = updateMeta' "appendix" (MetaBlocks appendix)
          $ updateMeta' "acknowledgements" (MetaBlocks acknowledgements)
          $ updateMeta' "dedicatory" (MetaBlocks dedicatory)
          $ updateMeta' "facilities" (MetaBlocks facilities)
          $ updateMeta' "software" (MetaBlocks software) m

      (Pandoc _ newBlocks) = foldl (flip walk) p [rmFacilities , rmSoftware , rmAppendix , rmAck, rmDedicatory]
   in (Pandoc newMeta newBlocks)
    where
      updateMeta' s b m = updateMeta m s b
      rmFacilities (Div (_,["facilities"],_) _) = Null
      rmFacilities a = a
      rmSoftware   (Div (_,["software"],_) _) = Null
      rmSoftware   a = a
      rmAppendix   (Div (_,["appendix"],_) _) = Null
      rmAppendix   a = a
      rmDedicatory (Div (_,["acknowledgements"],_) _) = Null
      rmDedicatory a = a
      rmAck (Div (_,["acknowledgements"],_) _) = Null
      rmAck a = a
      getDedicatory a@(Div (_,["dedicatory"],_) _) = [a]
      getDedicatory _ = []
      getFacilities a@(Div (_,["facilities"],_) _) = [a]
      getFacilities _ = []
      getSoftware   a@(Div (_,["software"],_) _) = [a]
      getSoftware   _ = []
      getAppendix   a@(Div (_,["appendix"],_) _) = [a]
      getAppendix   _ = []
      getAck a@(Div (_,["acknowledgements"],_) _) = [a]
      getAck _ = []
