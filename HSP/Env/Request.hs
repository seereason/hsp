-----------------------------------------------------------------------------
-- |
-- Module      :  HSP.Env.Request
-- Copyright   :  (c) Niklas Broberg 2006,
-- License     :  BSD-style (see the file LICENSE.txt)
-- 
-- Maintainer  :  Niklas Broberg, nibro@cs.chalmers.se
-- Stability   :  experimental
-- Portability :  Haskell 98
--
-- An interface to a request object as held in the HSP environment.
-----------------------------------------------------------------------------

module HSP.Env.Request (
	Request(..)
	) where

import HSP.Exception

-- | A record representing an interface to an HTTP request. This allows us to use
-- many different underlying types for these requests, all we need is to supply
-- conversions to this interface. This is useful for when we run as CGI, or when
-- we run inside a server app.
data Request = Request {
          getParameterL         :: String -> [String]
        , getHeaders		:: [(String, String)]
	}

-- | Get a parameter from the request query string (GET) or body (POST).
-- | Returns @Nothing@ if the parameter is not set.
getParameter :: Request -> String -> Maybe String
getParameter r s = case getParameterL r s of
		    (s:_) -> Just s
		    _     -> Nothing

-- | Unsafe version of getParameter, essentially fromJust.(getParameter r) but throws
-- a ParameterLookupFailed exception if the parameter is not set.
getParameter_ :: Request -> String -> String
getParameter_ r s = maybe (throwHSP $ ParameterLookupFailed s) id $ getParameter r s

-- | Return a value instead of a string.
readParameter :: Read a => Request -> String -> Maybe a
readParameter r s = fmap read $ getParameter r s

-- | Return a list of values read from their string representations.
readParameterL :: Read a => Request -> String -> [a]
readParameterL r s = map read $ getParameterL r s

-- | Unsafe version of readParameter
readParameter_ :: Read a => Request -> String -> a
readParameter_ r s = read $ getParameter_ r s