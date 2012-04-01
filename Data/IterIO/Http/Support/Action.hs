{-# LANGUAGE CPP #-}
#if defined(__GLASGOW_HASKELL__) && (__GLASGOW_HASKELL__ >= 702)
{-# LANGUAGE Safe #-}
#endif
-- |Defines the 'Action' monad which abstracts some of the details of handling
-- HTTP requests with IterIO.
module Data.IterIO.Http.Support.Action (
    Action
  , ActionState(..)
  , Param(..)
  , params, param, paramVal, paramValM
  , setParams
  , getBody
  , getHttpReq
  , setSession, destroySession
  , requestHeader
) where

import Control.Monad
import Control.Monad.Trans.State
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import Data.IterIO.Http
import Data.Maybe

-- | A request parameter from a form field in the HTTP body
data Param = Param {
    paramKey :: S.ByteString
  , paramValue :: L.ByteString
  , paramHeaders :: [(S.ByteString, S.ByteString)] -- ^ Header of a @multipart/form-data@ post
} deriving (Show)

data ActionState t b m = ActionState {
    actionReq  :: HttpReq t
  , actionResp :: HttpResp m
  , actionParams   :: [Param]
  , actionBody :: b
}

-- | A 'StateT' monad in which requests can be handled. It keeps track of the
-- 'HttpReq', the form parameters from the request body and an 'HttpResp' used
-- to reply to the client.
type Action t b m a = StateT (ActionState t b m) m a

-- |Sets a the value for \"_sess\" in the cookie to the given string.
setSession :: Monad m => String -> Action t b m ()
setSession cookie = modify $ \s ->
  let cookieHeader = (S.pack "Set-Cookie", S.pack $ "_sess=" ++ cookie ++ "; path=/;")
  in s { actionResp = respAddHeader cookieHeader (actionResp s)}

-- |Removes the \"_sess\" key-value pair from the cookie.
destroySession :: Monad m => Action t b m ()
destroySession = modify $ \s ->
  let cookieHeader = (S.pack "Set-Cookie", S.pack "_sess=; path=/; expires=Thu, Jan 01 1970 00:00:00 UTC;")
  in s { actionResp = respAddHeader cookieHeader (actionResp s)}

-- |Returns the value of an Http Header from the request if it exists otherwise
-- 'Nothing'
requestHeader :: Monad m => S.ByteString -> Action t b m (Maybe S.ByteString)
requestHeader name = do
  httpReq <- getHttpReq
  return $ lookup name (reqHeaders httpReq)

-- |Returns the 'HttpReq' for the current request.
getHttpReq :: Monad m => Action t b m (HttpReq t)
getHttpReq = gets actionReq

-- |Returns the body of the current request.
getBody :: Monad m => Action t b m b
getBody = gets actionBody

-- | Set the list of 'Param's.
setParams :: Monad m => [Param] -> Action t b m [Param]
setParams prms = do
  modify $ \s -> s { actionParams = prms }
  return prms

-- | Returns a list of all 'Param's.
params :: Monad m => Action t b m [Param]
params = do
  gets actionParams

-- | Returns the 'Param' corresponding to the specified key or 'Nothing'
-- if one is not present in the request.
param :: Monad m => S.ByteString -> Action t b m (Maybe Param)
param key = do
  prms <- params
  return $ foldl go Nothing prms
  where go Nothing p = if paramKey p == key then Just p else Nothing
        go a _ = a

-- | Force get parameter value
paramVal :: Monad m => S.ByteString -> Action t b m (L.ByteString)
paramVal n = (paramValue . fromJust) `liftM` param n

-- | Get (maybe) paramater value and transform it with @f@
paramValM :: Monad m
          => (L.ByteString -> a)
          -> S.ByteString
          -> Action t b m (Maybe a)
paramValM f n = fmap (f . paramValue) `liftM` param n


