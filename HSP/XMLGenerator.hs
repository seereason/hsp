module HSP.XMLGenerator (
        -- * Type classes
--        IsXML(..), 
        IsXMLs(..), 
--        IsAttrValue(..),
--        IsAttribute(..),
        
--        extract,
        
        module HSX.XMLGenerator, HSX.genElement, HSX.genEElement

    ) where

import HSP.Monad
import HSP.XML hiding (Name)

import HSX.XMLGenerator hiding (XMLGen(..))
import qualified HSX.XMLGenerator as HSX (XMLGen(..))

import Control.Monad (liftM)
import Control.Monad.Trans (lift)

import Data.List (intersperse)
---------------------------------------------
-- Instantiating XMLGenerator for the HSP monad.

-- | We can use literal XML syntax to generate values of type XML in the HSP monad.
instance Monad m => HSX.XMLGen (HSPT' m) where
 type HSX.XML (HSPT' m) = XML
 newtype HSX.Attribute (HSPT' m) = HSPAttr Attribute 
 newtype HSX.Child     (HSPT' m) = HSPChild XML
 xmlToChild = HSPChild
-- genElement = element
-- genEElement = eElement

instance (Monad m, IsXMLs m c) => EmbedAsChild (HSPT' m) c where
 asChild = liftM (map HSX.xmlToChild) . toXMLs

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
class Monad m => IsXMLs m a where
 toXMLs :: a -> HSPT m [XML]

-- | Anything that can be represented as a single XML element
-- can of course be represented as a (singleton) list of XML
-- elements.
--instance (IsXML a) => IsXMLs a where
-- toXMLs a = do xml <- toXML a
--             return [xml]

-- | XML can naturally be represented as XML.
instance Monad m => IsXMLs m XML where
 toXMLs = return . return


-- | We must specify an extra rule for Strings even though the previous
-- rule would apply, because the rule for [a] would also apply and
-- would be more specific.
instance Monad m => IsXMLs m String where
 toXMLs s = return [pcdata s]

-- | If something can be represented as a list of XML, then a list of 
-- that something can also be represented as a list of XML.
instance (IsXMLs m a, Monad m) => IsXMLs m [a] where
 toXMLs as = do xmlss <- mapM toXMLs as
                return $ concat xmlss

-- | An IO computation returning something that can be represented
-- as a list of XML can be lifted into an analogous HSP computation.
instance (IsXMLs m a, Monad m) => IsXMLs m (m a) where
 toXMLs ma = lift (lift ma) >>= toXMLs

-- | Any child elements that arise through the use of literal syntax should be of the same
-- type as the parent element, unless explicitly given a different type. We use TypeCast
-- to accomplish this. This also captures the case when the embedded value is an HSP computation.
instance (IsXMLs m1 x, TypeCast (m x) (HSPT' m1 x)) => IsXMLs m1 (XMLGenT m x) where
 toXMLs (XMLGenT x) = (XMLGenT $ typeCast x) >>= toXMLs

-- | Of the base types, () stands out as a type that can only be
-- represented as a (empty) list of XML.
instance Monad m => IsXMLs m () where
 toXMLs _ = return []

-- | Maybe types are handy for cases where you might or might not want
-- to generate XML, such as null values from databases
instance (IsXMLs m a, Monad m) => IsXMLs m (Maybe a) where
 toXMLs Nothing = return []
 toXMLs (Just a) = toXMLs a

instance Monad m => IsXMLs m (HSX.Child (HSPT' m)) where
 toXMLs (HSPChild x) = toXMLs x

---------------
-- IsAttrValue

-- | Instantiate this class to enable values of the given type
-- to appear as XML attribute values.
class Monad m => IsAttrValue m a where
 toAttrValue :: a -> HSPT m AttrValue
-- fromAttrValue :: AttrValue -> a

-- | An AttrValue is trivial.
instance Monad m => IsAttrValue m AttrValue where
 toAttrValue = return

-- | Strings can be directly represented as values.
instance Monad m => IsAttrValue m String where
 toAttrValue = return . pAttrVal

-- | Anything that can be shown can always fall back on
-- that as a default behavior.
--instance (Monad m, Show a) => IsAttrValue m a where
-- toAttrValue = toAttrValue . show

-- | An IO computation returning something that can be represented
-- as an attribute value can be lifted into an analogous HSP computation.
instance IsAttrValue m a => IsAttrValue m (m a) where
 toAttrValue ma = lift (lift ma) >>= toAttrValue

-- | An HSP computation returning something that can be represented
-- as a list of XML simply needs to turn the produced value into 
-- a list of XML.
instance IsAttrValue m a => IsAttrValue m (HSPT m a) where
 toAttrValue hspa = hspa >>= toAttrValue

{-- | The common way to present list data in attributes is as
-- a comma separated, unbracketed sequence
instance (Monad m, IsAttrValue m a) => IsAttrValue m [a] where
 toAttrValue as = do [/ (Value vs)* /] <- mapM toAttrValue as
                     return . Value $ concat $ intersperse "," vs
-}
-----------------------------------------------------------------------
-- Manipulating attributes

-- | Just like with XML children, we want to conveniently allow values of various
-- types to appear as attributes. 
class Monad m => IsAttribute m a where
 toAttribute :: a -> HSPT m Attribute

-- | Attributes can represent attributes. 
instance Monad m => IsAttribute m Attribute where
 toAttribute = return

-- | Values of the Attr type, constructed with :=, can represent attributes.
instance (IsName n, IsAttrValue m a) 
        => IsAttribute m (Attr n a) where
 toAttribute (n := a) = do av <- toAttrValue a
                           return $ MkAttr (toName n, av)

-- | Attributes can be the result of an HSP computation.
instance IsAttribute m a => IsAttribute m (HSPT m a) where
 toAttribute hspa = hspa >>= toAttribute

-- | ... or of an IO computation.
instance IsAttribute m a => IsAttribute m (m a) where
 toAttribute ma = lift (lift ma) >>= toAttribute

instance Monad m => IsAttribute m (HSX.Attribute (HSPT' m)) where
 toAttribute (HSPAttr a) = toAttribute a

{-
-- | Anything that can represent an attribute can also be embedded as attributes
-- using the literal XML syntax.
instance (IsAttribute a) => EmbedAsAttr HSP' a where
 asAttr = fmap HSPAttr . toAttribute

-- | Set an attribute to something in an XML element.
--set :: (IsXML a, IsAttribute at) => a -> at -> HSP XML
--set x a = setAll x [a]
-}
-----------------------------------------
-- SetAttr and AppendChild
{-
-- | Set attributes.
instance SetAttr HSP' XML where
 setAll xml hats = do
        attrs <- hats
        case xml of
         CDATA _         -> return xml
         Element n as cs -> return $ Element n (foldr insert as (map stripAttr attrs)) cs


instance TypeCast (m x) (HSP' XML)
                => SetAttr HSP' (XMLGenT m x) where
 setAll (XMLGenT hxml) ats = (XMLGenT $ typeCast hxml) >>= \xml -> setAll xml ats

-- | Append children.
instance AppendChild HSP' XML where
 appAll xml children = do
        chs <- children
        case xml of
         CDATA _         -> return xml
         Element n as cs -> return $ Element n as (cs ++ (map stripChild chs))

instance TypeCast (m x) (HSP' XML) 
                => AppendChild HSP' (XMLGenT m x) where
 appAll (XMLGenT hxml) chs = (XMLGenT $ typeCast hxml) >>= (flip appAll) chs
-}
---------------
-- GetAttrValue
{-
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
-}

--------------------------------------------------------------------
-- The base XML generation, corresponding to the use of the literal
-- XML syntax.
{-
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

stripAttr  (HSPAttr a) = a
stripChild (HSPChild c) = c

eAttrs :: Attributes
eAttrs = []
insert :: Attribute -> Attributes -> Attributes
insert = (:)

-- | Generate an empty XML element.
eElement :: (IsName n, IsAttribute at) => n -> [at] -> HSP XML
eElement n attrs = element n attrs ([] :: [XML])

-}