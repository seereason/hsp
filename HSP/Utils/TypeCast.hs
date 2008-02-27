{-# OPTIONS_GHC -fglasgow-exts #-}
{-# OPTIONS_GHC -fallow-overlapping-instances #-}
{-# OPTIONS_GHC -fallow-undecidable-instances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  HSP.Utils.TypeCast
-- Copyright   :  (c) Niklas Broberg 2008
-- License     :  BSD-style (see the file LICENSE.txt)
-- 
-- Maintainer  :  Niklas Broberg, nibro@cs.chalmers.se
-- Stability   :  experimental
-- Portability :  Ugh. MPTCs with fundeps, overlapping and undecideable instances,
--		  not to mention being dependent on the way GHC does type inference...
--
-- Stolen from HList. TypeCast forces an ambiguous value to be cast to
-- a particular type during instance selection.
-----------------------------------------------------------------------------
module HSP.Utils.TypeCast where

-- literally lifted from the HList library
class TypeCast   a b   | a -> b, b -> a      where typeCast   :: a -> b
class TypeCast'  t a b | t a -> b, t b -> a  where typeCast'  :: t->a->b
class TypeCast'' t a b | t a -> b, t b -> a  where typeCast'' :: t->a->b
instance TypeCast'  () a b => TypeCast a b   where typeCast x = typeCast' () x
instance TypeCast'' t a b => TypeCast' t a b where typeCast' = typeCast''
instance TypeCast'' () a a where typeCast'' _ x  = x
