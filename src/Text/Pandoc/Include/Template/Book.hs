#!/usr/bin/env stack
--stack --resolver lts-11.9  --install-ghc runghc --package diagrams-pandoc --stack-yaml /home/aku/kanazawa/report/pubsEngine/diagrams-pandoc/stack.yaml

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Text.Pandoc.Include.Template.Book
  ( doBook
  ) where

import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import Data.Maybe

import Text.Pandoc
import Text.Pandoc.Walk
import Text.Pandoc.Process
import qualified Data.ByteString.Lazy as BL
import qualified Text.Pandoc.UTF8 as UTF8
import qualified Data.List as L
import qualified Data.Map as M

doBook :: String -> Pandoc -> Pandoc
doBook template (Pandoc m a) =
  doPreface "book" m $ walk (doSideNote template)
                $ walk (doKaoBox template)
                $ walk (doMath template)
                $ walk (doSideNoteBlock template)
                $ walk (doHeader template) a

doMath :: String -> Block -> Block
doMath "book" (Div (a,("math":[c]), opts) s) =
  let title = if c == "corollary" then "[" <> (fromMaybe "" $ lookup "title" opts) <> "]"
                                  else ""
   in Div (a,[],opts) $ [ RawBlock (Format "latex") $ "\\begin{" <> c <> "}" <> title ]
                      <> s <>
                      [ RawBlock (Format "latex") $ "\\end{" <> c <> "}" ]
doMath "book" (Div (a,("math":_),o) s) = doMath "book" $ Div (a,["math","definition"],o) s
doMath _ h = h

doPreface "book" (Meta oldMeta) p =
  let prefaceMeta = query getPreface p
      newMeta = Meta $ M.union prefaceMeta oldMeta
      newBlocks = walk removePreface p
   in Pandoc newMeta newBlocks
doPreface _ m p = Pandoc m p

getPreface (Div (a,["preface"],opts) s) =
  let title = case lookup "title" opts of
                Nothing -> "Preface"
                Just s -> s
   in M.fromList [ ("preface-title", MetaString title)
                 , ("preface", MetaBlocks s)
                 ]
getPreface _ = M.empty

removePreface (Div (_,["preface"],_) _) = Null
removePreface a = a

doKaoBox :: String -> Block -> Block
doKaoBox "book" (Div (a,["kaobox"], opts) s) =
  let frametitle = lookup "title" opts
   in Div (a,[],opts) $ [ Plain [ RawInline (Format "latex") "\\begin{kaobox}[frametitle="
                                , Str $ fromMaybe "" frametitle
                                , RawInline (Format "latex") "]"
                                ]
                        ] <> s <>
                        [ RawBlock (Format "latex") "\\end{kaobox}"]
doKaoBox _ h = h

doHeader :: String -> Block -> Block
doHeader "book" (Header 1 (_,["partition"],_) s) =
  Div nullAttr $ [ RawBlock (Format "latex") "\\pagelayout{wide}"
                 , Plain $ [ RawInline (Format "latex") "\\addpart{" ]
                           <> s <>
                           [ RawInline (Format "latex") "}"]
                 , RawBlock (Format "latex") "\\pagelayout{margin}"
                 ]
doHeader "book" h@(Header 1 (_,[],opts) _) =
  let chapterImage = case lookup "image" opts of
                Nothing -> ""
                Just s -> let imageHeight = case lookup "height" opts of
                                              Nothing -> "7.5cm"
                                              Just h -> h
                           in "\\setchapterimage["<> imageHeight <>"]{"<>s<>"}"
      margintoc = "\\setchapterpreamble[u]{\\margintoc}"
      chapterStyle = let cStyle = case lookup "style" opts of
                                    Nothing -> "kao"
                                    Just s -> s
                      in "\\setchapterstyle{" <> cStyle <> "}"
      preamble = RawBlock (Format "latex") $ T.unlines [ chapterStyle
                                                       , chapterImage
                                                       , margintoc ]
   in Div nullAttr [preamble, h]
doHeader _ h = h

doSideNoteBlock :: String -> Block -> Block
doSideNoteBlock template b@(Div (a,[c],opts) s)
  | c == "sidenote" || c == "marginnote" =
      let simpleNote = Note s
          offset = case lookup "offset" opts of
                    Just o -> "*" <> o
                    _ -> ""
          cSide a = "\\sidenote[][" <> a <> "]"
          cMargin a = "\\marginnote[" <> a <> "]"
          command = case c of
                      "sidenote" -> cSide
                      "marginnote" -> cMargin
                      _ -> (\_ -> "")
          genNote "book" =
              LineBlock $ [ [ RawInline (Format "latex") $ command offset <> "{" ]
                          <> (L.intercalate [Space] $ map blocksToinlines s)
                          <> [RawInline (Format "latex") "}"]
                          ]
          genNote _ = Plain [ simpleNote ]
       in genNote template
  | otherwise = b
doSideNoteBlock _ b = b

blocksToinlines (Para s) = s
blocksToinlines _ = []

doSideNote :: String -> Inline -> Inline
doSideNote template b@(Cite [c] _)
  | citationId c == "sidenote" || citationId c == "marginnote" =
      let simpleNote = Note [ Para $ citationSuffix c ]
          offset = case citationPrefix c of
                    [Str o] -> "*" <> o
                    _ -> ""
          cSide a = "\\sidenote[][" <> a <> "]"
          cMargin a = "\\marginnote[" <> a <> "]"
          command = case citationId c of
                      "sidenote" -> cSide
                      "marginnote" -> cMargin
                      _ -> (\_ -> "")
          genNote "book" =
              Span nullAttr $ [RawInline (Format "latex") $ command offset <> "{"]
                <> citationSuffix c
                <> [RawInline (Format "latex") "}"]
          genNote _ = simpleNote
       in genNote template
  | otherwise = b
doSideNote _ b = b

processPegon :: String -> Block -> IO Block
processPegon engine cb@(CodeBlock (_, ["nusantara"], _) t) = do
  TIO.writeFile "_build/temp/nusantara.text" t
  (_,r) <- pipeProcess Nothing "txtconv" (words "-i _build/temp/nusantara.text -o /dev/stdout -t _build/nusantara-trans-novoc.tec") ""
  putStrLn "============processPegon"
  return $ case engine of
            "revealjs" -> Div nullAttr [ Para [ Str $ UTF8.toText $ BL.toStrict r ]]
            _ -> Div nullAttr [ RawBlock (Format "latex") $ T.unlines [ "\\begin{txarab}"
                                                                      , UTF8.toText $ BL.toStrict r
                                                                      , "\\end{txarab}"
                                                                      ]
                              ]
processPegon _ b = pure b

processPegonInline :: String -> Inline -> IO Inline
processPegonInline engine l@(Code _ t)
  | T.isPrefixOf ".nu " t = do
      TIO.writeFile "_build/temp/nusantara.text" $ fromMaybe "inna lillahi" $ T.stripPrefix ".nu " t
      (_,r) <- pipeProcess Nothing "txtconv" (words "-i _build/temp/nusantara.text -o /dev/stdout -t _build/nusantara-trans-novoc.tec") ""
      putStrLn "============processPegonInline"
      TIO.putStrLn t
      return $ case engine of
                "revealjs" -> Str $ UTF8.toText $ BL.toStrict r
                _ -> RawInline (Format "latex") $ "\\txarb{" <> (UTF8.toText $ BL.toStrict r) <> "}"
    -- [Cite [Citation {citationId = "nu:BASMALA", citationPrefix = [], citationSuffix = [Space,Str "laa",Space,Str "ilaaha",Space,Str "illa-llah"], citationMode = NormalCitation, citationNoteNum = 1, citationHash = 0}] [Str "[@nu:BASMALA",Space,Str "laa",Space,Str "ilaaha",Space,Str "illa-llah]"]]
  | otherwise = return l
processPegonInline _ l = return l
