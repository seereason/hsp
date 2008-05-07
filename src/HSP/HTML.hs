-----------------------------------------------------------------------------
-- |
-- Module      :  HSP.HTML
-- Copyright   :  (c) Niklas Broberg, Jeremy Shaw 2008
-- License     :  BSD-style (see the file LICENSE.txt)
-- 
-- Maintainer  :  Niklas Broberg, nibro@cs.chalmers.se
-- Stability   :  experimental
-- Portability :  Haskell 98
--
-- Attempt to render XML as well-formed HTML 4.01:
--  * no short tags are used, e.g., <script></script> instead of <script />
--  * the end tag is forbidden for some elements, for these we:
--    * render only the open tag, e.g., <br>
--    * throw an error if the tag contains children
--  * optional end tags are always rendered
--
-- Currently no validation is performed.
-----------------------------------------------------------------------------
module HSP.HTML (
                 -- * Functions
                 renderAsHTML
                ) where

import Data.List
import HSP.XML

-- | Pretty-prints HTML values.
-- FIXME: also verify that the domain is correct
-- FIXME: what to do if a namespace is encountered
-- FIXME: what to do with xmlns in html tag
-- TODO: add strict mode which runs a validator (that probably belongs in the function which calls renderAsHTML)
-- FIXME: elements which forbid and end tag, should perhaps not contain children ?
--
-- NOTE: this can throw errors, but you have to be in the IO monad to
-- catch them. Also, you have to use evaluate if you want to check for
-- errors. This means you can not start sending the page until the
-- whole page has been rendered. And you have to store the whole page
-- in RAM at once. Similar problems occur if we return Either
-- instead. We mostly care about catching errors and showing them in
-- the browser during testing, so perhaps this can be configurable.
--
-- Another solution would be a compile time error if an empty-only
-- tag contained children.
renderAsHTML :: XML -> String
renderAsHTML xml = renderAsHTML' 0 xml ""

data TagType = Open | Close

renderAsHTML' :: Int -> XML -> ShowS
renderAsHTML' _ (CDATA cd) = showString cd
renderAsHTML' n (Element name@(Nothing,nm) attrs children) 
    | nm == "area"	= renderTagEmpty children
    | nm == "base"	= renderTagEmpty children
    | nm == "br"        = renderTagEmpty children
    | nm == "col"       = renderTagEmpty children
    | nm == "hr"        = renderTagEmpty children
    | nm == "img"       = renderTagEmpty children
    | nm == "input"     = renderTagEmpty children
    | nm == "link"      = renderTagEmpty children
    | nm == "meta"      = renderTagEmpty children
    | nm == "param"     = renderTagEmpty children
    where
      renderTagEmpty [] = renderTag Open n name attrs
      renderTagEmpty cs = error $ (filter (/= '\n') (renderTag Open 0 name attrs " should be empty, but contains children:")) ++ "\n" ++ 
                            (foldr (renderAsHTML' 0) "" cs)
renderAsHTML' n (Element name attrs children) =
        let open  = renderTag Open n name attrs 
            cs    = renderChildren n children 
            close = renderTag Close n name []
         in open . cs . close
  where renderChildren :: Int -> Children -> ShowS
        renderChildren n' cs = foldl (.) id $ map (renderAsHTML' (n'+2)) cs


                
renderTag :: TagType -> Int -> Name -> Attributes -> ShowS 
renderTag typ n name attrs = 
        let (start,end) = case typ of
                           Open   -> (showChar '<', showChar '>')
                           Close  -> (showString "</", showChar '>')
            nam = showName name
            as  = renderAttrs attrs
         in start . nam . as . end

  where renderAttrs :: Attributes -> ShowS
        renderAttrs [] = nl
        renderAttrs attrs' = showChar ' ' . ats . nl
          where ats = foldl (.) id $ intersperse (showChar ' ') $ fmap renderAttr attrs'


        renderAttr :: Attribute -> ShowS
        renderAttr (MkAttr (nam, (Value val))) = showName nam . showChar '=' . renderAttrVal val

        renderAttrVal :: String -> ShowS
        renderAttrVal s = showChar '\"' . showString s . showChar '\"'

        showName (Nothing, s) = showString s
        showName (Just d, s)  = showString d . showChar ':' . showString s

        nl = showChar '\n' . showString (replicate n ' ')



