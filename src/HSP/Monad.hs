{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, OverlappingInstances, MultiParamTypeClasses, TypeFamilies #-}
module HSP.Monad where

import Control.Applicative  (Applicative, Alternative, (<$>))
import Control.Monad.Cont   (MonadCont)
import Control.Monad.Error  (MonadError)
import Control.Monad.Fix    (MonadFix)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Writer (MonadWriter)
import Control.Monad.State  (MonadState)
import Control.Monad.Trans  (MonadIO, MonadTrans(lift))
import Data.String          (fromString)
import Data.Text.Lazy       (Text)
import qualified Data.Text.Lazy as Text
import HSP.XMLGenerator     (AppendChild(..), Attr(..), EmbedAsAttr(..), EmbedAsChild(..), IsName(toName), SetAttr(..), XMLGen(..), XMLGenerator)
import HSP.XML              (Attribute(..), XML(..), pAttrVal, pcdata)

newtype HSPT xml m a = HSPT { unHSPT :: m a }
    deriving (Functor, Applicative, Alternative, Monad, MonadIO, MonadReader r, MonadWriter w, MonadState s, MonadCont, MonadError e, MonadFix)

instance MonadTrans (HSPT xml) where
    lift = HSPT

instance (Functor m, Monad m) => (XMLGen (HSPT XML m)) where
    type    XMLType       (HSPT XML m) = XML
    newtype ChildType     (HSPT XML m) = HSPChild { unHSPChild :: XML }
    newtype AttributeType (HSPT XML m) = HSPAttr  { unHSPAttr  :: Attribute }
    genElement n attrs childr          =
        do as <- (map unHSPAttr  . concat) <$> sequence attrs
           cs <- (map unHSPChild . concat) <$> sequence childr
           return (Element n as cs)
    xmlToChild                         = HSPChild
    pcdataToChild str                  = HSPChild (pcdata str)

instance (Functor m, Monad m) => SetAttr (HSPT XML m) XML where
    setAll xml hats =
        do attrs <- hats
           case xml of
             CDATA _ _       -> return xml
             Element n as cs -> return $ Element n (foldr (:) as (map unHSPAttr attrs)) cs

instance (Functor m, Monad m) => AppendChild (HSPT XML m) XML where
 appAll xml children =
        do chs <- children
           case xml of
             CDATA _ _       -> return xml
             Element n as cs -> return $ Element n as (cs ++ (map unHSPChild chs))

instance (Functor m, Monad m) => EmbedAsChild (HSPT XML m) XML where
    asChild = return . (:[]) . HSPChild

instance (Functor m, Monad m) => EmbedAsChild (HSPT XML m) [XML] where
    asChild = return . map HSPChild

instance (Functor m, Monad m) => EmbedAsChild (HSPT XML m) String where
    asChild = return . (:[]) . HSPChild . pcdata . Text.pack

instance (Functor m, Monad m) => EmbedAsChild (HSPT XML m) Text where
    asChild = return . (:[]) . HSPChild . pcdata

instance (Functor m, Monad m) => EmbedAsChild (HSPT XML m) Char where
    asChild = return . (:[]) . pcdataToChild . Text.singleton

instance (Functor m, Monad m) => EmbedAsChild (HSPT XML m) () where
    asChild = return . const []

instance (Monad m, Functor m) => EmbedAsAttr (HSPT XML m) (Attr Text Char) where
    asAttr (n := c)  = asAttr (n := Text.singleton c)

instance (Monad m, Functor m) => EmbedAsAttr (HSPT XML m) Attribute where
    asAttr = return . (:[]) . HSPAttr

instance (Functor m, Monad m) => EmbedAsAttr (HSPT XML m) (Attr Text Bool) where
    asAttr (n := True)  = asAttr $ MkAttr (toName n, pAttrVal $ fromString "true")
    asAttr (n := False) = asAttr $ MkAttr (toName n, pAttrVal $ fromString "false")

instance (Functor m, Monad m) => EmbedAsAttr (HSPT XML m) (Attr Text Int) where
    asAttr (n := i)  = asAttr $ MkAttr (toName n, pAttrVal $ fromString (show i))

instance (Functor m, Monad m) => EmbedAsAttr (HSPT XML m) (Attr Text Text) where
    asAttr (n := txt)  = asAttr $ MkAttr (toName n, pAttrVal txt)

instance (Functor m, Monad m) => XMLGenerator (HSPT XML m)
