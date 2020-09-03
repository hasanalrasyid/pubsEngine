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
  let md@(bib:mdappendix:mdquote:mdacknowledgements:mdabbrev:mddedication:_) =
        map ((flip lookupMeta) mt) ["bibliography","mdappendix","mdquote","mdacknowledgements","mdabbrev","mddedication"]
  let mdall = concat $ map fromMetaInlines_Str $ catMaybes md
  mapM_ mdFile2LaTeX mdall

  mt' <- autoMeta "auto-acknowledgements" mdacknowledgements mt >>=
         autoMeta "auto-quote" mdquote >>=
         autoMeta "auto-abbreviations" mdabbrev >>=
         autoMeta "auto-dedication" mddedication

--  putStrLn $ (++) "====" $ show $ catMaybes texs
  let command = map genLn $ concat $ map (map (genPerintah ".bib")) $ map fromMetaInlines_Str $ catMaybes [bib]
  if (null bib) then return ()
                 else callCommand $ unlines [ "mkdir -p _build", unlines command]

  return $ Pandoc mt' blks
    where
      autoMeta autoX mdX target = do
        ackMD <- T.readFile $ fromMaybeMetaValue "blank.md" mdX
        ackEP <- runIO $ readMarkdown (def{readerExtensions = foldr enableExtension pandocExtensions pandocExtSetting}) ackMD
        ackBlocks <- case ackEP of
                      Left _ -> error "error ackEP"
                      Right (Pandoc _ a) -> pure a
        pure $ addMetaField autoX (MetaBlocks ackBlocks) target

      fromMaybeMetaValue s Nothing = s
      fromMaybeMetaValue _ (Just (MetaInlines (Str s:_))) = s ++ ".md"
      fromMaybeMetaValue s _ = s
      genMeta r0 ((s,ts):sts) =
        let rM = addMetaField s (fromList $ map genRawBlock ts) r0
         in genMeta rM sts
      genMeta res [] = res
      genRawBlock (Just a) = RawBlock (Format "latex") $ unlines  [ t | s <- fromMetaInlines_Str a
                                                                      , let u = takeFileName s
                                                                      , let t = "\\input{" ++ u ++ "}"
                                                                      ]
      genRawBlock Nothing = Null
      genLn s = "ln -s -f " ++ s ++ " _build/"
      genPerintah x s = "../" ++ s ++ x
      fromMetaInlines_Str (MetaInlines a) = map fromStr a
      fromMetaInlines_Str (MetaBlocks ((Plain a):_)) = filter (not . null) $ map fromStr a
      fromMetaInlines_Str _ = ["fromMetaInlines_Str failed"]
      fromStr (Str a) = a
      fromStr SoftBreak = ""
      fromStr _ = "fromStr failed"
