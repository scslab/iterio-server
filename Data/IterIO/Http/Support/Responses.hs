{-# LANGUAGE CPP #-}
#if defined(__GLASGOW_HASKELL__) && (__GLASGOW_HASKELL__ >= 702)
{-# LANGUAGE Safe #-}
#endif
-- |Utility functions for responding to HTTP requests from within an 'Action'.
module Data.IterIO.Http.Support.Responses (
    render
  , redirectTo
  , respond404
  , respondStat
) where

import Control.Monad.Trans.State
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import Data.IterIO
import Data.IterIO.Http.Support.Action (Action, ActionState(..))
import Data.IterIO.Http

-- | Responds to the client with an empty @404@ (Not Found) response.
respond404 :: Monad m => Action t m ()
respond404 = modify $
  \s -> s { actionResp = resp404 $ actionReq s }

-- | Replaces the HTTP status in the current 'HttpResp' with the given
-- 'HttpStatus'.
respondStat :: Monad m => HttpStatus -> Action t m ()
respondStat status = modify $
  \s -> s { actionResp = (actionResp s) { respStatus = status} }

-- | Responds to the client with a @303@ (Temporary Redirect) response to the given
-- path.
redirectTo :: Monad m
           => String -- ^ The path to redirect to
           -> Action t m ()
redirectTo path = modify $
  \s -> s { actionResp = resp303 path }

-- | Responds to the client with a @200@ (Success) response with the given body
-- and mime-type.
render :: Monad m
       => String -- ^ The mime-type of the response (commonly \"text\/html\")
       -> L.ByteString -- ^ The response body
       -> Action t m ()
render ctype text = modify $ \s -> s { actionResp = mkResp $ actionResp s }
  where len = (S.pack "Content-Length", S.pack . show . L.length $ text)
        ctypeHeader = (S.pack "Content-Type", S.pack ctype)
        mkResp resp = resp { respHeaders = ctypeHeader : (len : respHeaders resp)
                           , respBody = inumPure text }

