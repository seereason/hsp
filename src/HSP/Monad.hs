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
        HSP, HSPT, HSPT',
        runHSP, evalHSP, runHSPT, evalHSPT, getEnv,
        unsafeRunHSP,  -- dangerous!!
        -- * Functions
        getParam, getIncNumber, doIO, catch,
        setMetaData, withMetaData
        ) where

-- Monad imports
-- import Control.Monad.Reader (ReaderT(..), ask, lift)
import Control.Monad.RWS (RWST(..), ask, lift, put)
import Control.Monad.State
import Control.Monad.Trans (MonadIO(..))

import Prelude hiding (catch)

-- Exceptions
import Control.Exception (catchDyn)
import HSP.Exception

import HSP.Env

import HSP.XML
import HSX.XMLGenerator (XMLGenT(..), unXMLGenT)

--------------------------------------------------------------
-- The HSP Monad

-- | The HSP monad is a reader wrapper around
-- the IO monad, but extended with an XMLGenerator wrapper.
--type HSP' = ReaderT HSPEnv IO
--type HSP  = XMLGenT HSP'

type HSP =  HSPT IO

type HSPT' m = RWST HSPEnv () (Maybe XMLMetaData) m
type HSPT  m = XMLGenT (HSPT' m)

-- do NOT export this in the final version
dummyEnv :: HSPEnv
dummyEnv = undefined

-- | Runs a HSP computation in a particular environment. Since HSP wraps the IO monad,
-- the result of running it will be an IO computation.
runHSP :: HSP a -> HSPEnv -> Maybe XMLMetaData -> IO (Maybe XMLMetaData, a)
runHSP hsp hspEnv xmd = runRWST (unXMLGenT hsp) hspEnv xmd >>= \(a,md,()) -> return (md, a)

runHSPT :: (Monad m) => HSPT m a -> HSPEnv -> Maybe XMLMetaData -> m (Maybe XMLMetaData, a)
runHSPT hsp hspEnv xmd = runRWST (unXMLGenT hsp) hspEnv xmd >>= \(a,md,()) -> return (md, a)

evalHSPT :: MonadIO m => HSPT m a -> Maybe XMLMetaData -> m (Maybe XMLMetaData, a)
evalHSPT hsp xmd = liftIO mkSimpleEnv >>= \env -> runHSPT hsp env xmd

evalHSP :: HSP a -> Maybe XMLMetaData -> IO (Maybe XMLMetaData, a)
evalHSP hsp xmd = mkSimpleEnv >>= \env -> runHSP hsp env xmd

-- | Runs a HSP computation without an environment. Will work if the page in question does
-- not touch the environment. Not sure about the usefulness at this stage though...
unsafeRunHSP :: HSP a -> IO (Maybe XMLMetaData, a)
unsafeRunHSP hspf = runHSP hspf dummyEnv Nothing

-- | Execute an IO computation within the HSP monad.
doIO :: IO a -> HSP a
doIO = liftIO

setMetaData :: (Monad m) => (Maybe XMLMetaData) -> HSPT m ()
setMetaData xmd = lift (put xmd)

withMetaData :: (Monad m) => Maybe XMLMetaData -> HSPT m a -> HSPT m a
withMetaData xmd h = do
  x <- h
  setMetaData xmd
  return x

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
catch (XMLGenT (RWST f)) handler = XMLGenT $ RWST $ \e s ->
        f e s `catchDyn` (\ex -> (let (XMLGenT (RWST g)) = handler ex
                                 in g e s))
