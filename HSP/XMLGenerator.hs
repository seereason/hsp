module HSP.XMLGenerator (
        -- * Type classes
        IsXML(..), 
        IsXMLs(..), 
        IsAttrValue(..),
        IsAttribute(..),
        
        extract,
        
        module HSX.XMLGenerator, HSX.genElement, HSX.genEElement

    ) where

import HSP.Monad
import HSP.XML hiding (Name)

import HSX.XMLGenerator hiding (XMLGenerator(..))
import qualified HSX.XMLGenerator as HSX (XMLGenerator(..))


---------------------------------------------
-- Instantiating XMLGenerator for the HSP monad.


-- | We can use literal XML syntax to generate values of type XML in the HSP monad.
instance HSX.XMLGenerator HSP' where
 type HSX.XML HSP' = XML
 type HSX.Attribute HSP' = Attribute 
 type HSX.Child HSP' = XML
 genElement = element
 genEElement = eElement

instance (IsXMLs a) => EmbedAsChild a (HSP [XML]) where
 asChild = toXMLs


-------------------------------------------------------------------
-- Type classes

-------------
-- IsXML

-- | Instantiate this class to enable values of the given type
-- to appear as XML elements.
class IsXML a where
 toXML :: a -> HSP XML

-- | XML can naturally be represented as XML.
instance IsXML XML where
 toXML = return

-- | Strings should be represented as PCDATA.
instance IsXML String where
 toXML = return . pcdata

-- | Anything that can be shown can always fall back on
-- that as a default behavior.
instance (Show a) => IsXML a where
 toXML = toXML . show 

-- | An IO computation returning something that can be represented
-- as XML can be lifted into an analogous HSP computation.
instance (IsXML a) => IsXML (IO a) where
 toXML = toXML . doIO

-- | An HSP computation returning something that can be represented
-- as XML simply needs to turn the produced value into XML.
instance (IsXML a) => IsXML (HSP a) where
 toXML hspa = hspa >>= toXML


-------------
-- IsXMLs

-- | Instantiate this class to enable values of the given type
-- to be represented as a list of XML elements. Note that for
-- any given type, only one of the two classes IsXML and IsXMLs
-- should ever be instantiated. Values of types that instantiate 
-- IsXML can automatically be represented as a (singleton) list 
-- of XML children.
class IsXMLs a where
 toXMLs :: a -> HSP [XML]

-- | Anything that can be represented as a single XML element
-- can of course be represented as a (singleton) list of XML
-- elements.
--instance (IsXML a) => IsXMLs a where
-- toXMLs a = do xml <- toXML a
-- 	       return [xml]

-- | XML can naturally be represented as XML.
instance IsXMLs XML where
 toXMLs = return . return


-- | We must specify an extra rule for Strings even though the previous
-- rule would apply, because the rule for [a] would also apply and
-- would be more specific.
instance IsXMLs String where
 toXMLs s = do xml <- toXML s
 	       return [xml]

-- | If something can be represented as a list of XML, then a list of 
-- that something can also be represented as a list of XML.
instance (IsXMLs a) => IsXMLs [a] where
 toXMLs as = do xmlss <- mapM toXMLs as
 		return $ concat xmlss

-- | An IO computation returning something that can be represented
-- as a list of XML can be lifted into an analogous HSP computation.
instance (IsXMLs a) => IsXMLs (IO a) where
 toXMLs ioa = doIO ioa >>= toXMLs

-- | Any child elements that arise through the use of literal syntax should be of the same
-- type as the parent element, unless explicitly given a different type. We use TypeCast
-- to accomplish this. This also captures the case when the embedded value is an HSP computation.
instance (IsXMLs x, TypeCast (m x) (HSP' x)) => IsXMLs (XMLGenT m x) where
 toXMLs (XMLGenT x) = (XMLGenT $ typeCast x) >>= toXMLs

-- | Of the base types, () stands out as a type that can only be
-- represented as a (empty) list of XML.
instance IsXMLs () where
 toXMLs _ = return []

-- | Maybe types are handy for cases where you might or might not want
-- to generate XML, such as null values from databases
instance (IsXMLs a) => IsXMLs (Maybe a) where
 toXMLs Nothing = return []
 toXMLs (Just a) = toXMLs a

---------------
-- IsAttrValue

-- | Instantiate this class to enable values of the given type
-- to appear as XML attribute values.
class IsAttrValue a where
 toAttrValue :: a -> HSP AttrValue
-- fromAttrValue :: AttrValue -> a

-- | An AttrValue is trivial.
instance IsAttrValue AttrValue where
 toAttrValue = return

-- | Strings can be directly represented as values.
instance IsAttrValue String where
 toAttrValue = return . pAttrVal

-- | Anything that can be shown can always fall back on
-- that as a default behavior.
instance Show a => IsAttrValue a where
 toAttrValue = toAttrValue . show

-- | An IO computation returning something that can be represented
-- as an attribute value can be lifted into an analogous HSP computation.
instance (IsAttrValue a) => IsAttrValue (IO a) where
 toAttrValue = toAttrValue . doIO 

-- | An HSP computation returning something that can be represented
-- as a list of XML simply needs to turn the produced value into 
-- a list of XML.
instance (IsAttrValue a) => IsAttrValue (HSP a) where
 toAttrValue hspa = hspa >>= toAttrValue

-- | The common way to present list data in attributes is as
-- a comma separated, unbracketed sequence
--instance (IsAttrValue a) => IsAttrValue [a] where
-- toAttrValue as = do [/ (Value vs)* /] <- mapM toAttrValue as
-- 		     return . Value $ concat $ intersperse "," vs


-----------------------------------------------------------------------
-- Manipulating attributes

-- | Just like with XML children, we want to conveniently allow values of various
-- types to appear as attributes. 
class IsAttribute a where
 toAttribute :: a -> HSP Attribute

-- | Attributes can represent attributes. 
instance IsAttribute Attribute where
 toAttribute = return

-- | Values of the Attr type, constructed with :=, can represent attributes.
instance (IsName n, IsAttrValue a) => IsAttribute (Attr n a) where
 toAttribute (n := a) = do av <- toAttrValue a
 			   return (toName n, av)

-- | Attributes can be the result of an HSP computation.
instance (IsAttribute a) => IsAttribute (HSP a) where
 toAttribute hspa = hspa >>= toAttribute

-- | ... or of an IO computation.
instance (IsAttribute a) => IsAttribute (IO a) where
 toAttribute ioa = doIO ioa >>= toAttribute

-- | Anything that can represent an attribute can also be embedded as attributes
-- using the literal XML syntax.
instance (IsAttribute a) => EmbedAsAttr a (HSP Attribute) where
 asAttr = toAttribute

-- | Set an attribute to something in an XML element.
--set :: (IsXML a, IsAttribute at) => a -> at -> HSP XML
--set x a = setAll x [a]

-----------------------------------------
-- SetAttr and AppendChild

-- | Set attributes.
instance SetAttr HSP' XML where
 setAll xml hats = do
        attrs <- fmap concat $ sequence hats
	case xml of
	 CDATA _         -> return xml
	 Element n as cs -> return $ Element n (foldr insert as attrs) cs


instance TypeCast (m x) (HSP' XML)
                => SetAttr HSP' (XMLGenT m x) where
 setAll (XMLGenT hxml) ats = (XMLGenT $ typeCast hxml) >>= \xml -> setAll xml ats

-- | Append children.
instance AppendChild HSP' XML where
 appAll xml children = do
        css <- mapM toXMLs children
        case xml of
         CDATA _         -> return xml
         Element n as cs -> return $ Element n as (cs ++ concat css)

instance TypeCast (m x) (HSP' XML) 
                => AppendChild HSP' (XMLGenT m x) where
 appAll (XMLGenT hxml) chs = (XMLGenT $ typeCast hxml) >>= (flip appAll) chs
 
---------------
-- GetAttrValue

-- | Instantiate this class to enable values of the given type
-- to be retrieved through attribute patterns.
class GetAttrValue a where
 fromAttrValue :: AttrValue -> a

-- | An AttrValue is trivial.
instance GetAttrValue AttrValue where
 fromAttrValue = id

-- | Strings can be directly taken from values.
instance GetAttrValue String where
 fromAttrValue (Value s) = s
 
-- | Anything that can be read can always fall back on
-- that as a default behavior.
instance (Read a) => GetAttrValue a where
 fromAttrValue = read . fromAttrValue

-- | An IO computation returning something that can be represented
-- as an attribute value can be lifted into an analogous HSP computation.
--instance (GetAttrValue a, Monad m) => GetAttrValue (m a) where
-- fromAttrValue = return . fromAttrValue

-- | The common way to present list data in attributes is as
-- a comma separated, unbracketed sequence
instance (GetAttrValue a) => GetAttrValue [a] where
 fromAttrValue v@(Value str) = case str of
 	[/ v1+, (/ ',', vs@:_+ /)+ /] -> 
 		map (fromAttrValue . Value) (v1:vs)
 	_ -> [fromAttrValue v]

-- All that for these two functions
extract :: (GetAttrValue a) => Name -> Attributes -> (Maybe a, Attributes)
extract _ [] = (Nothing, [])
extract name (p@(n, v):as) 
	| name == n = (Just $ fromAttrValue v, as)
	| otherwise = let (val, attrs) = extract name as
		       in (val, p:attrs)


--------------------------------------------------------------------
-- The base XML generation, corresponding to the use of the literal
-- XML syntax.

--  | Generate an XML element from its components.
element :: (IsName n, IsXMLs xmls, IsAttribute at) => n -> [at] -> xmls -> HSP XML
element n attrs xmls = do
	cxml <- toXMLs xmls
	attribs <- mapM toAttribute attrs
	return $ Element (toName n) 
		   (foldr insert eAttrs attribs)
		   (flattenCDATA cxml)
	
  where flattenCDATA :: [XML] -> [XML]
  	flattenCDATA cxml = 
  		case flP cxml [] of
  		 [] -> []
  		 [CDATA ""] -> []
  		 xs -> xs  			
  	flP :: [XML] -> [XML] -> [XML]
  	flP [] bs = reverse bs
  	flP [x] bs = reverse (x:bs)
  	flP (x:y:xs) bs = case (x,y) of
  			   (CDATA s1, CDATA s2) -> flP (CDATA (s1++s2) : xs) bs
  			   _ -> flP (y:xs) (x:bs)

eAttrs :: Attributes
eAttrs = []
insert :: Attribute -> Attributes -> Attributes
insert = (:)

-- | Generate an empty XML element.
eElement :: (IsName n, IsAttribute at) => n -> [at] -> HSP XML
eElement n attrs = element n attrs ([] :: [XML])

