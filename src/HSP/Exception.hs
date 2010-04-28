{-# OPTIONS -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  HSP.Exception
-- Copyright   :  (c) Niklas Broberg 2008
-- License     :  BSD-style (see the file LICENSE.txt)
-- 
-- Maintainer  :  Niklas Broberg, nibro@cs.chalmers.se
-- Stability   :  experimental
-- Portability :  needs dynamic exceptions and deriving Typeable
--
-- Defines a datatype for runtime exceptions that may arise during
-- the evaluation of a HSP page.
-----------------------------------------------------------------------------
module HSP.Exception (
	Exception(..),
	throwHSP 
	) where

import Data.Typeable
#ifdef BASE4
import Control.OldException (throwDyn)
#else
import Control.Exception (throwDyn)
#endif
data Exception
	=  ParameterLookupFailed String	-- ^ User tried to do an irrefutable parameter lookup
					-- that failed.
	-- | ... I'm sure there should be more exceptions, we'll add them when we get to them.
 deriving (Eq, Show, Typeable)

-- Internal funcion that throws a dynamic exception particular to HSP.
throwHSP :: Exception -> a
throwHSP = throwDyn

