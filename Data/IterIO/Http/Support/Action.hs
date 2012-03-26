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
  , routeAction, routeActionPattern
  , params, param, paramVal, paramValM
  , getHttpReq
  , setSession, destroySession
  , requestHeader
) where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import Data.List
import Data.List.Split
import Data.IterIO
import Data.IterIO.Http
import Data.IterIO.HttpRoute
import Data.Maybe

-- | A request parameter from a form field in the HTTP body
data Param = Param {
    paramKey :: S.ByteString
  , paramValue :: L.ByteString
  , paramHeaders :: [(S.ByteString, S.ByteString)] -- ^ Header of a @multipart/form-data@ post
}

data ActionState t m = ActionState {
    actionReq  :: HttpReq t
  , actionResp :: HttpResp m
  , actionParams   :: ([Param], Maybe [Param])
}

-- | A 'StateT' monad in which requests can be handled. It keeps track of the
-- 'HttpReq', the form parameters from the request body and an 'HttpResp' used
-- to reply to the client.
type Action t m a = StateT (ActionState t m) (Iter L.ByteString m) a

-- | Routes an 'Action'
routeAction :: Monad m => Action t m () -> HttpRoute m t
routeAction action = routeFn $ runAction action

-- | Routes an 'Action' to the given URL pattern. Patterns can include
-- directories as well as variable patterns (prefixed with @:@) to be passed
-- into the 'Action' as extra 'Param's. Some examples of URL patters:
--
--  * \/posts\/:id
--
--  * \/posts\/:id\/new
--
--  * \/:date\/posts\/:category\/new
--
routeActionPattern :: Monad m => String -> Action t m () -> HttpRoute m t
routeActionPattern pattern action = foldl' addVar (routeFn $ runActionWithRouteNames patternList action) patternList
  where patternList = reverse $ filter ((/= 0) . length) $ splitOn "/" pattern
        addVar rt (':':_) = routeVar rt
        addVar rt name = routeName name rt

-- |Sets a the value for \"_sess\" in the cookie to the given string.
setSession :: Monad m => String -> Action t m ()
setSession cookie = modify $ \s ->
  let cookieHeader = (S.pack "Set-Cookie", S.pack $ "_sess=" ++ cookie ++ "; path=/;")
  in s { actionResp = respAddHeader cookieHeader (actionResp s)}

-- |Removes the \"_sess\" key-value pair from the cookie.
destroySession :: Monad m => Action t m ()
destroySession = modify $ \s ->
  let cookieHeader = (S.pack "Set-Cookie", S.pack "_sess=; path=/; expires=Thu, Jan 01 1970 00:00:00 UTC;")
  in s { actionResp = respAddHeader cookieHeader (actionResp s)}

-- |Returns the value of an Http Header from the request if it exists otherwise
-- 'Nothing'
requestHeader :: Monad m => S.ByteString -> Action t m (Maybe S.ByteString)
requestHeader name = do
  httpReq <- getHttpReq
  return $ lookup name (reqHeaders httpReq)

-- |Returns the 'HttpReq' for the current request.
getHttpReq :: Monad m => Action t m (HttpReq t)
getHttpReq = gets actionReq

urlencoded :: S.ByteString
urlencoded = S.pack "application/x-www-form-urlencoded"

multipart :: S.ByteString
multipart = S.pack "multipart/form-data"

-- | Returns a list of all 'Param's.
params :: Monad m => Action t m ([Param])
params = do
  s <- get
  let req = actionReq s
  let (prms, mprms) = actionParams s
  case mprms of
    Just formPrms -> return $ prms ++ formPrms
    Nothing -> do
      formPrms <- lift $ case reqContentType req of
        Just (mt, _) | mt /= urlencoded  && mt /= multipart ->
          return []
        _ -> paramList req
      put $ s { actionParams = (prms, Just formPrms) }
      return $ prms ++ formPrms

-- | Returns the 'Param' corresponding to the specified key or 'Nothing'
-- if one is not present in the request.
param :: Monad m => S.ByteString -> Action t m (Maybe Param)
param key = do
  prms <- params
  return $ foldl go Nothing prms
  where go Nothing p = if paramKey p == key then Just p else Nothing
        go a _ = a

-- | Force get parameter value
paramVal :: Monad m => S.ByteString -> Action t m (L.ByteString)
paramVal n = (paramValue . fromJust) `liftM` param n

-- | Get (maybe) paramater value and transform it with @f@
paramValM :: Monad m
          => (L.ByteString -> a)
          -> S.ByteString
          -> Action t m (Maybe a)
paramValM f n = fmap (f . paramValue) `liftM` param n

runAction :: Monad m
              => Action s m ()
              -> HttpReq s
              -> Iter L.ByteString m (HttpResp m)
runAction = runActionWithRouteNames []

runActionWithRouteNames :: Monad m
              => [String]
              -> Action s m ()
              -> HttpReq s
              -> Iter L.ByteString m (HttpResp m)
runActionWithRouteNames routeNames action req = do
  prms <- paramList req
  let pathLstParams = pathLstToParams req routeNames
  let s = ActionState req (mkHttpHead stat200) (pathLstParams ++ prms, Nothing)
  (_, result) <- (runStateT action) s
  return $ actionResp result


pathLstToParams :: HttpReq s -> [String] -> [Param]
pathLstToParams req routeNames = result
  where (result, _) = foldl go ([], reqPathParams req) routeNames
        go (prms, plst) (':':var) = ((transform var $ head plst):prms, tail plst)
        go s _ = s
        transform k v = Param (S.pack k) (L.fromChunks [v]) []

paramList :: Monad m => HttpReq s -> Iter L.ByteString m [Param]
paramList req = foldForm req handle []
  where handle accm field = do
          val <- pureI
          return $ (Param (ffName field) val (ffHeaders field)):accm

