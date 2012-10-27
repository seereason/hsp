{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  HSP.XML.PCDATA
-- Copyright   :  (c) Niklas Broberg 2008-2012
-- License     :  BSD-style (see the file LICENSE.txt)
--
-- Maintainer  :  Niklas Broberg, nibro@cs.chalmers.se
-- Stability   :  experimental
-- Portability :  Haskell 98
--
-- Escaping between CDATA <=> PCDATA
-----------------------------------------------------------------------------
module HSP.XML.PCDATA (
	  escape
--	, unescape
        , escaper
--        , unescaper
        , xmlEscapeChars
	) where

import Data.Monoid              ((<>), mempty)
import Data.Text.Lazy           (Text, uncons)
import qualified Data.Text.Lazy as Text
import Data.Text.Lazy.Builder   (Builder, fromLazyText, singleton)

-- | Take a normal string and transform it to PCDATA by escaping special characters.
-- calls 'escaper' with 'xmlEscapeChars'
-- See also: 'escaper'
escape :: Text -> Builder
escape = escaper xmlEscapeChars

-- | Take a normal string and transform it to PCDATA by escaping special characters.
-- See also: 'escape', 'xmlEscapeChars'
escaper :: [(Char, Builder)] -- ^ table of escape characters
        -> Text -- ^ String to escape
        -> Builder -- ^ Escaped String
escaper escapeTable = go
    where
      escapeChars = map fst escapeTable
      go txt | Text.null txt = mempty
      go txt =
          case Text.break (`elem` escapeChars) txt of
            (hd,tl) ->
                case uncons tl of
                  Nothing -> fromLazyText hd
                  (Just (c, tl')) -> fromLazyText hd <> pChar escapeTable c <> go tl'

pChar :: [(Char, Builder)] -- ^ table of escape characters
      -> Char              -- ^ character to escape
      -> Builder           -- ^ escaped character
pChar escapeChars c =
    case lookup c escapeChars of
      Nothing -> singleton c
      Just s  -> singleton '&' <> s <> singleton ';'

-- This list should be extended.
xmlEscapeChars :: [(Char, Builder)]
xmlEscapeChars = [
	('&',	"amp"	),
	('\"',	"quot"	),
	('\'',	"apos"	),
	('<',	"lt"	),
	('>',	"gt"	)
	]
{-
-- | Take a PCDATA string and translate all escaped characters in it to the normal
-- characters they represent.
-- Does no error checking of input string, will fail if input is not valid PCDATA.
-- calls 'unescaper' with 'xmlEscapeChars'
-- See also: 'unescaper'
unescape :: String -> String
unescape = unescaper xmlEscapeChars

-- | Take a PCDATA string and translate all escaped characters in it to the normal
-- characters they represent.
-- Does no error checking of input string, will fail if input is not valid PCDATA.
-- See also: 'unescape', 'xmlEscapeChars'
unescaper :: [(Char, Builder)] -- ^ table of escape characters
          -> Text -- ^ String to unescape
          -> Builder -- ^ unescaped String
unescaper escapeChars = reverse . unE ""
  where unE acc "" = acc
  	unE acc (c:cs) =
  	  case c of
  	    '&' -> let (esc, ';':rest) = break (==';') cs
  	               Just ec = revLookup esc escapeChars
  	            in unE (ec:acc) rest
  	    _ -> unE (c:acc) cs

  	revLookup e = lookup e . map (\(a,b) -> (b,a))
-}