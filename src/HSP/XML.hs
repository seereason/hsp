-----------------------------------------------------------------------------
-- |
-- Module      :  HSP.XML
-- Copyright   :  (c) Niklas Broberg 2008-2012
-- License     :  BSD-style (see the file LICENSE.txt)
--
-- Maintainer  :  Niklas Broberg, niklas.broberg@gmail.com
-- Stability   :  experimental
-- Portability :  Haskell 98
--
-- Datatypes and type classes comprising the basic model behind
-- the scenes of Haskell Server Pages tags.
-----------------------------------------------------------------------------
module HSP.XML (
        -- * The 'XML' datatype
        XML(..),
        XMLMetaData(..),
        Domain,
        Name,
        Attributes,
        Children,
        pcdata,
        cdata,
        -- * The Attribute type
        Attribute(..),
        AttrValue(..),
        attrVal, pAttrVal,
        -- * Functions
        renderXML,
        isElement, isCDATA,
        fromStringLit
        ) where

import Data.List                        (intersperse)
import Data.Monoid                      ((<>), mconcat)
import Data.String                      (fromString)
import Data.Text.Lazy.Builder           (Builder, fromLazyText, singleton, toLazyText)
import Data.Text.Lazy                   (Text)
import qualified Data.Text.Lazy         as Text
import HSP.XML.PCDATA                   (escape)

---------------------------------------------------------------
-- Domain/Name

type Domain = Maybe Text
type Name   = (Domain, Text)

---------------------------------------------------------------
-- Attributes
newtype Attribute = MkAttr (Name, AttrValue)
  deriving Show

-- | Represents an attribue value.
data AttrValue = Value Bool Text

fromStringLit :: String -> Text
fromStringLit = Text.pack

-- | Create an attribue value from a string.
attrVal, pAttrVal :: Text -> AttrValue
attrVal  = Value False
pAttrVal = Value True

instance Show AttrValue where
 show (Value _ txt) = Text.unpack txt

type Attributes = [Attribute]

---------------------------------------------------------------
-- XML
-- | The XML datatype representation. Is either an Element or CDATA.
data XML
    = Element Name Attributes Children
    | CDATA Bool Text
      deriving Show

type Children = [XML]

---------------------------------------------------------------
-- XMLMetaData

-- |The XMLMetaData datatype
--
-- Specify the DOCTYPE, content-type, and preferred render for XML data.
--
-- See also: 'HSP.Monad.setMetaData' and 'HSP.Monad.withMetaData'
data XMLMetaData = XMLMetaData
  {  doctype           :: (Bool, Text) -- ^ (show doctype when rendering, DOCTYPE string)
  ,  contentType       :: Text
  ,  preferredRenderer :: XML -> Builder
  }

{- instance Show XML where
 show = renderXML -}

-- | Test whether an XML value is an Element or CDATA
isElement, isCDATA :: XML -> Bool
isElement (Element {}) = True
isElement _ = False
isCDATA = not . isElement

-- | Embeds a string as a CDATA XML value.
cdata , pcdata :: Text -> XML
cdata  = CDATA False
pcdata = CDATA True

------------------------------------------------------------------
-- Rendering

data TagType = Open | Close | Single

renderTag :: TagType -> Int -> Name -> Attributes -> Builder
renderTag typ n name attrs =
        let (start,end) = case typ of
                           Open   -> (singleton  '<',  singleton  '>')
                           Close  -> (fromString "</", singleton  '>')
                           Single -> (singleton  '<',  fromString "/>")
            nam = showName name
            as  = renderAttrs attrs
         in mconcat [start, nam, as, end]

  where renderAttrs :: Attributes -> Builder
        renderAttrs [] = nl
        renderAttrs attrs' = singleton ' ' <> mconcat  ats <>  nl
          where ats = intersperse (singleton ' ') $ fmap renderAttr attrs'

        renderAttr :: Attribute -> Builder
        renderAttr (MkAttr (nam, (Value needsEscape val))) =
            showName nam <> singleton '=' <> renderAttrVal  (if needsEscape then escape val else fromLazyText val)

        renderAttrVal :: Builder -> Builder
        renderAttrVal txt = singleton '\"' <> txt <> singleton '\"'

        showName (Nothing, s) = fromLazyText s
        showName (Just d, s)  = fromLazyText d <> singleton ':' <> fromLazyText s

        nl = singleton '\n' <> fromString (replicate n ' ')

renderXML' :: Int -> XML -> Builder
renderXML' _ (CDATA needsEscape cd) = if needsEscape then escape cd else fromLazyText cd
renderXML' n (Element name attrs []) = renderTag Single n name attrs
renderXML' n (Element name attrs children) =
        let open  = renderTag Open n name attrs
            cs    = renderChildren n children
            close = renderTag Close n name []
         in open <> cs <> close

  where renderChildren :: Int -> Children -> Builder
        renderChildren n' cs = mconcat $ map (renderXML' (n'+2)) cs

-- TODO: indents are incorrectly calculated

-- | Pretty-prints XML values.
renderXML :: XML -> Text
renderXML xml = toLazyText $ renderXML' 0 xml
