{-# OPTIONS_GHC -fallow-overlapping-instances #-}
{-# OPTIONS_GHC -fallow-undecidable-instances #-}
{-# OPTIONS_GHC -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  HSP.Data
-- Copyright   :  (c) Niklas Broberg 2008
-- License     :  BSD-style (see the file LICENSE.txt)
-- 
-- Maintainer  :  Niklas Broberg, nibro@cs.chalmers.se
-- Stability   :  experimental
-- Portability :  requires undecidable and overlapping instances, and forall in datatypes
--
-- Datatypes and type classes comprising the basic model behind
-- the scenes of Haskell Server Pages tags.
-----------------------------------------------------------------------------

module HSP.Monad (
	-- * The 'HSP' Monad
	HSP,
	HSPEnv(..),
	runHSP,
        unsafeRunHSP,  -- dangerous!!
	-- * Type classes
	IsXML(..), 
	IsXMLs(..), 
	IsAttrValue(..),
	-- * Functions
	getEnv, set, setAll, catch, extract,
	-- * Modules
	module HSX.XMLGenerator
	) where

-- Monad imports
import Control.Monad.Reader (ReaderT(..), ask, lift)
import Control.Monad.Trans (MonadIO(..))
import HSX.XMLGenerator
import HSP.XML -- the XML type generated with this XMLGenerator monad
import Prelude hiding (catch)

-- Exceptions
import Control.Exception (catchDyn)
import HSP.Exception

import HSP.Env.Request

--------------------------------------------------------------
-- The HSP Monad

-- | The HSP monad is a reader wrapper around
-- the IO monad, but extended with an XMLGenerator wrapper.
type HSP' = ReaderT HSPEnv IO
type HSP  = XMLGenerator HSP'

-- | The runtime environment for HSP pages.
data HSPEnv = HSPEnv {
	  getReq  :: Request -- In CGI mode we only support Request
--	, getResp :: Rp.Response
	}

-- do NOT export this in the final version
dummyEnv :: HSPEnv
dummyEnv = undefined

-- | Runs a HSP computation in a particular environment. Since HSP wraps the IO monad,
-- the result of running it will be an IO computation.
runHSP :: HSP a -> HSPEnv -> IO a
runHSP = runReaderT . unXMLGenerator

-- | Runs a HSP computation without an environment. Will work if the page in question does
-- not touch the environment. Not sure about the usefulness at this stage though...
unsafeRunHSP :: HSP a -> IO a
unsafeRunHSP hspf = runHSP hspf dummyEnv

-- | Execute an IO computation within the HSP monad.
doIO :: IO a -> HSP a
doIO = liftIO

-- | Supplíes the HSP environment.
getEnv :: HSP HSPEnv
getEnv = lift ask

---------------------------------------------
-- Instantiating GenXML for the HSP monad.

-- | We can use literal XML syntax to generate values of type XML in the HSP monad.
instance GenerateXML HSP' XML (HSP Attribute) (HSP [XML]) where
 genElement = element
 genEElement = eElement

-- | Any child elements that arise through the use of literal syntax should be of the same
-- type as the parent element, unless explicitly given a different type. We use TypeCast
-- to accomplish this.
instance TypeCast (m x) (HSP' XML) => EmbedAsChild (XMLGenerator m x) (HSP [XML]) where
 asChild (XMLGenerator x) = XMLGenerator $ fmap return $ typeCast x

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
instance (IsXML a) => IsXMLs a where
 toXMLs a = do xml <- toXML a
 	       return [xml]

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
 toXMLs = toXMLs . doIO

-- | An HSP computation returning something that can be represented
-- as a list of XML simply needs to turn the produced value into 
-- a list of XML.
instance (IsXMLs a) => IsXMLs (HSP a) where
 toXMLs hspa = hspa >>= toXMLs

-- | Of the base types, () stands out as a type that can only be
-- represented as a (empty) list of XML.
instance IsXMLs () where
 toXMLs _ = return []

-- | Maybe types are handy for cases where you might or might not want
-- to generate XML, such as null values from databases
instance (IsXMLs a) => IsXMLs (Maybe a) where
 toXMLs Nothing = return []
 toXMLs (Just a) = toXMLs a

-- | Anything that can be represented as a list of XML elements can
-- also be used as children of an XML element.
instance (IsXMLs a) => EmbedAsChild a (HSP [XML]) where
 asChild = toXMLs

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

-- | Names can be simple or qualified with a domain. We want to conveniently
-- use both simple strings or pairs wherever a Name is expected.
class IsName n where
 toName :: n -> Name

-- | Names can represent names, of course.
instance IsName Name where
 toName = id

-- | Strings can represent names, meaning a simple name with no domain.
instance IsName String where
 toName s = (Nothing, s)

-- | Pairs of strings can represent names, meaning a name qualified with a domain.
instance IsName (String, String) where
 toName (ns, s) = (Just ns, s)


-- | Just like with XML children, we want to conveniently allow values of various
-- types to appear as attributes. 
class IsAttribute a where
 toAttribute :: a -> HSP Attribute

-- | Attributes can represent attributes. 
instance IsAttribute Attribute where
 toAttribute = return

-- | Values of the Attr type, constructed with :=, can represent attributes.
instance IsAttribute Attr where
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
set :: (IsXML a, IsAttribute at) => a -> at -> HSP XML
set x a = setAll x [a]

-- | Set many attributes at once.
setAll :: (IsXML a, IsAttribute at) => a -> [at] -> HSP XML
setAll isxml ats = do
	xml   <- toXML isxml
	attrs <- mapM toAttribute ats
	case xml of
	 CDATA _     -> return xml
	 Element n as cs -> return $ Element n (foldr insert as attrs) cs


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



-----------------------------------------------------------------------
-- Exception handling

-- | Catch a user-caused exception.
catch :: HSP a -> (Exception -> HSP a) -> HSP a
catch (XMLGenerator (ReaderT f)) handler = XMLGenerator $ ReaderT $ \e ->
	f e `catchDyn` (\ex -> (let (XMLGenerator (ReaderT g)) = handler ex
				 in g e))
