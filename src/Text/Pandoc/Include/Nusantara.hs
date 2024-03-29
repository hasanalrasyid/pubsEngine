#!/usr/bin/env stack
--stack --resolver lts-11.9  --install-ghc runghc --package diagrams-pandoc --stack-yaml /home/aku/kanazawa/report/pubsEngine/diagrams-pandoc/stack.yaml

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Text.Pandoc.Include.Nusantara where

import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import Data.Maybe

import Text.Pandoc
import Text.Pandoc.Process
import qualified Data.ByteString.Lazy as BL
import qualified Text.Pandoc.UTF8 as UTF8

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
