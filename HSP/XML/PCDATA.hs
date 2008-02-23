-----------------------------------------------------------------------------
-- |
-- Module      :  HSP.XML.PCDATA
-- Copyright   :  (c) Niklas Broberg 2008
-- License     :  BSD-style (see the file LICENSE.txt)
-- 
-- Maintainer  :  Niklas Broberg, nibro@cs.chalmers.se
-- Stability   :  experimental
-- Portability :  Haskell 98
--
-- Escaping between CDATA <=> PCDATA
-----------------------------------------------------------------------------
module HSP.XML.PCDATA (
	  escape	-- :: String -> String
	, unescape	-- :: String -> String
	) where


-- | Take a normal string and transform it to PCDATA by escaping special characters.
escape :: String -> String
escape [] = ""
escape (c:cs) = pChar c ++ escape cs

pChar :: Char -> String
pChar c = case lookup c escapeChars of
	   Nothing -> [c]
	   Just s  -> '&' : s ++ ";"

-- This list should be extended.
escapeChars :: [(Char, String)]
escapeChars = [
	('&',	"amp"	),
	('\"',	"quot"	),
	('\'',	"apos"	),
	('<',	"lt"	),
	('>',	"gt"	)
	]

-- | Take a PCDATA string and translate all escaped characters in it to the normal
-- characters they represent.
-- Does no error checking of input string, will fail if input is not valid PCDATA.
unescape :: String -> String
unescape = reverse . unE ""
  where unE acc "" = acc
  	unE acc (c:cs) = 
  	  case c of
  	    '&' -> let (esc, ';':rest) = break (==';') cs
  	               Just ec = revLookup esc escapeChars
  	            in unE (ec:acc) cs
  	    _ -> unE (c:acc) cs
  	
  	revLookup e = lookup e . map (\(a,b) -> (b,a))