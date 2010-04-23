{-# LANGUAGE OverlappingInstances, UndecidableInstances #-}
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
runHSP :: Maybe XMLMetaData -> HSP a -> HSPEnv -> IO (Maybe XMLMetaData, a)
runHSP xmd hsp hspEnv = runRWST (unXMLGenT hsp) hspEnv xmd >>= \(a,md,()) -> return (md, a)

runHSPT :: (Monad m) => Maybe XMLMetaData -> HSPT m a -> HSPEnv -> m (Maybe XMLMetaData, a)
runHSPT xmd hsp hspEnv = runRWST (unXMLGenT hsp) hspEnv xmd >>= \(a,md,()) -> return (md, a)

evalHSPT :: MonadIO m => Maybe XMLMetaData -> HSPT m a -> m (Maybe XMLMetaData, a)
evalHSPT xmd hsp = liftIO mkSimpleEnv >>= \env -> runHSPT xmd hsp env 

evalHSP :: Maybe XMLMetaData -> HSP a -> IO (Maybe XMLMetaData, a)
evalHSP xmd hsp = mkSimpleEnv >>= \env -> runHSP xmd hsp env

-- | Runs a HSP computation without an environment. Will work if the page in question does
-- not touch the environment. Not sure about the usefulness at this stage though...
unsafeRunHSP :: HSP a -> IO (Maybe XMLMetaData, a)
unsafeRunHSP hspf = runHSP Nothing hspf dummyEnv

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

-- | Supplies the HSP environment.
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
