{-
pandoc-crossref is a pandoc filter for numbering figures,
equations, tables and cross-references to them.
Copyright (C) 2015  Nikolay Yakimov <root@livid.pp.ru>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License along
with this program; if not, write to the Free Software Foundation, Inc.,
51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
-}

module Text.Pandoc.CrossRef.Util.CodeBlockCaptions
    (
    mkCodeBlockCaptions
    ) where

import Text.Pandoc.Definition
import Data.List (isPrefixOf, stripPrefix)
import Data.Maybe (fromMaybe)
import Text.Pandoc.CrossRef.References.Types
import Text.Pandoc.CrossRef.Util.Options
import Text.Pandoc.CrossRef.Util.Util

mkCodeBlockCaptions :: Options -> [Block] -> WS [Block]
mkCodeBlockCaptions opts x@(cb@(CodeBlock _ _):p@(Para _):xs)
  = return $ fromMaybe x $ orderAgnostic opts $ p:cb:xs
mkCodeBlockCaptions opts x@(p@(Para _):cb@(CodeBlock _ _):xs)
  = return $ fromMaybe x $ orderAgnostic opts $ p:cb:xs
mkCodeBlockCaptions _ x = return x

orderAgnostic :: Options -> [Block] -> Maybe [Block]
orderAgnostic opts (Para ils:CodeBlock (label,classes,attrs) code:xs)
  | codeBlockCaptions opts
  , Just caption <- getCodeBlockCaption ils
  , not $ null label
  , "lst" `isPrefixOf` label
  = return $ Div (label,"listing":classes, [])
      [Para caption, CodeBlock ([],classes,attrs) code] : xs
orderAgnostic opts (Para ils:CodeBlock (_,classes,attrs) code:xs)
  | codeBlockCaptions opts
  , Just (caption, labinl) <- splitLast <$> getCodeBlockCaption ils
  , Just label <- getRefLabel "lst" labinl
  = return $ Div (label,"listing":classes, [])
      [Para $ init caption, CodeBlock ([],classes,attrs) code] : xs
  where
    splitLast xs' = splitAt (length xs' - 1) xs'
orderAgnostic _ _ = Nothing

getCodeBlockCaption :: [Inline] -> Maybe [Inline]
getCodeBlockCaption ils
  | Just caption <- [Str "Listing:",Space] `stripPrefix` ils
  = Just caption
  | Just caption <- [Str ":",Space] `stripPrefix` ils
  = Just caption
  | otherwise
  = Nothing
