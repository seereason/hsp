{-# OPTIONS_GHC -fglasgow-exts #-}
{-# OPTIONS_GHC -fallow-undecidable-instances #-}
{-# OPTIONS_GHC -fallow-overlapping-instances #-}
module HSP.Layout (
        Layout(..), IsLayout(..),
        above, beside, (^^), (<>)
    )where

import Prelude hiding ((^^))

import HSP.Monad
import HSP.XML

data Layout
  = Above Layout Layout
  | Beside Layout Layout
  | forall x . IsXMLs x => Item x

class IsLayout a where
  toLayout :: a -> Layout

instance IsLayout Layout where
  toLayout = id

instance (IsXMLs a) => IsLayout a where
  toLayout = Item

(^^),(<>),above,beside :: (IsLayout a, IsLayout b) =>a -> b -> Layout
a ^^ b = Above  (toLayout a) (toLayout b)
a <> b = Beside (toLayout a) (toLayout b)
above = (^^)
beside = (<>)

instance IsXMLs Layout where
 toXMLs a@(Above _ _) = fmap return $  
   <table border="0"><% mapM mkRow $ foldAbove a %></table>
 toXMLs b@(Beside _ _) = fmap return $ 
   <table border="0"><tr><% map mkCell $ foldBeside b %></tr></table>
 toXMLs (Item xml) = toXMLs xml

foldAbove :: Layout -> [Layout]
foldAbove (Above a b) = foldAbove a ++ foldAbove b
foldAbove l = [l]

foldBeside :: Layout -> [Layout]
foldBeside (Beside a b) = foldBeside a ++ foldBeside b
foldBeside l = [l]

mkRow xml = <tr><% mkCell xml %></tr>
mkCell xml = <td><% xml %></td>
