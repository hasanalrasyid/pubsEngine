{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Text.Pandoc.Include.Thesis (linkTex)
  where
import Text.Pandoc.JSON
import Text.Pandoc.Shared (addMetaField)
import Text.Pandoc.Builder (fromList)
import System.Process (callCommand)
import Data.Maybe
import System.FilePath.Posix (takeFileName)
import Text.Pandoc
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Text.Pandoc.Include.Common



mdFile2LaTeX :: FilePath -> IO ()
mdFile2LaTeX f = do
  let fn = takeFileName f
  T.readFile (f ++ ".md") >>= md2LaTeX >>= T.writeFile ("_build/" ++ fn ++ ".tex")

md2LaTeX :: T.Text -> IO T.Text
md2LaTeX t = let param = def { readerExtensions = foldr enableExtension pandocExtensions pandocExtSetting }
              in runIOorExplode $  readMarkdown param t >>= writeLaTeX (def { writerReferenceLinks = True
                                                                            , writerTopLevelDivision = TopLevelChapter
                                                                            })

linkTex :: Pandoc -> IO Pandoc
linkTex p@(Pandoc mt blks) = do
  let md@(bib:mdappendix:mdquote:mdacknowledgements:_) = map ((flip lookupMeta) mt) ["bibliography","mdappendix","mdquote","mdacknowledgements"]
  let mdall = concat $ map fromMetaInlines_Str $ catMaybes md
  mapM_ mdFile2LaTeX mdall
  let mt' = genMeta mt [ ("auto-appendix",[mdappendix])
                       , ("auto-acknowledgements",[mdacknowledgements])
                       , ("auto-quote",[mdquote])
                       ]
  let command = map genLn $ concat $ map (map (genPerintah ".bib")) $ map fromMetaInlines_Str $ catMaybes [bib]
  if (null bib) then return ()
                 else callCommand $ unlines [ "mkdir -p _build", unlines command]

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
      genLn s = "ln -s -f " ++ s ++ " _build/"
      genPerintah x s = "../" ++ s ++ x
      fromMetaInlines_Str (MetaInlines a) = map fromStr a
      fromMetaInlines_Str (MetaBlocks ((Plain a):_)) = filter (not . null) $ map fromStr a
      fromMetaInlines_Str _ = ["fromMetaInlines_Str failed"]
      fromStr (Str a) = T.unpack a
      fromStr SoftBreak = ""
      fromStr _ = "fromStr failed"
