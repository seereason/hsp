module HSP.Env (
        module HSP.Env.Request,
        module HSP.Env.NumberGen,
        
        HSPEnv(..),
        
        mkSimpleEnv
    ) where
    
import HSP.Env.Request
import HSP.Env.NumberGen

import Data.IORef

-- | The runtime environment for HSP pages.
data HSPEnv = HSPEnv {
          getReq  :: Request -- In CGI mode we only support Request
--      , getResp :: Rp.Response
        , getNG   :: NumberGen
        }



mkSimpleEnv :: IO HSPEnv
mkSimpleEnv = do
        x <- newIORef 0
        let req = Request (const []) []
            num = NumberGen (atomicModifyIORef x (\a -> (a+1,a)))
        return $ HSPEnv req num
