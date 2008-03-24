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
	HSP, HSP',
	runHSP, evalHSP,
        unsafeRunHSP,  -- dangerous!!
	-- * Functions
	getParam, getIncNumber, doIO, catch
	) where

-- Monad imports
import Control.Monad.Reader (ReaderT(..), ask, lift)
import Control.Monad.Trans (MonadIO(..))

import Prelude hiding (catch)

-- Exceptions
import Control.Exception (catchDyn)
import HSP.Exception

import HSP.Env

import HSX.XMLGenerator (XMLGenT(..), unXMLGenT)

--------------------------------------------------------------
-- The HSP Monad

-- | The HSP monad is a reader wrapper around
-- the IO monad, but extended with an XMLGenerator wrapper.
type HSP' = ReaderT HSPEnv IO
type HSP  = XMLGenT HSP'

-- do NOT export this in the final version
dummyEnv :: HSPEnv
dummyEnv = undefined

-- | Runs a HSP computation in a particular environment. Since HSP wraps the IO monad,
-- the result of running it will be an IO computation.
runHSP :: HSP a -> HSPEnv -> IO a
runHSP = runReaderT . unXMLGenT

evalHSP :: HSP a -> IO a
evalHSP hsp = mkSimpleEnv >>= runHSP hsp 

-- | Runs a HSP computation without an environment. Will work if the page in question does
-- not touch the environment. Not sure about the usefulness at this stage though...
unsafeRunHSP :: HSP a -> IO a
unsafeRunHSP hspf = runHSP hspf dummyEnv

-- | Execute an IO computation within the HSP monad.
doIO :: IO a -> HSP a
doIO = liftIO

----------------------------------------------------------------
-- Environment stuff

-- | Supplíes the HSP environment.
getEnv :: HSP HSPEnv
getEnv = lift ask

getRequest :: HSP Request
getRequest = fmap getReq getEnv

getParam :: String -> HSP (Maybe String)
getParam s = getRequest >>= \req -> return $ getParameter req s

getIncNumber :: HSP Int
getIncNumber = getEnv >>= doIO . incNumber . getNG



-----------------------------------------------------------------------
-- Exception handling

-- | Catch a user-caused exception.
catch :: HSP a -> (Exception -> HSP a) -> HSP a
catch (XMLGenT (ReaderT f)) handler = XMLGenT $ ReaderT $ \e ->
	f e `catchDyn` (\ex -> (let (XMLGenT (ReaderT g)) = handler ex
				 in g e))
