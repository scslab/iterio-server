module Data.IterIO.Http.Support.Action (
    Action
  , Param(..)
  , routeAction
  , routeActionPattern
  , params
  , param
  , getHttpReq
  , setSession
  , destroySession
  , requestHeader
) where

import Control.Monad.Trans
import Control.Monad.Trans.State
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import Data.List
import Data.List.Split
import Data.IterIO
import Data.IterIO.Http
import Data.IterIO.HttpRoute

data Param = Param {
    paramKey :: S.ByteString
  , paramValue :: L.ByteString
  , paramHeaders :: [(S.ByteString, S.ByteString)]
}

type Action t m a = StateT (HttpReq t, HttpResp m, [Param]) m a

routeAction :: Monad m => Action t m () -> HttpRoute m t
routeAction action = routeFn $ runAction action

routeActionPattern :: Monad m => String -> Action t m () -> HttpRoute m t
routeActionPattern pattern action = foldl' addVar (routeFn $ runActionWithRouteNames patternList action) patternList
  where patternList = reverse $ filter ((/= 0) . length) $ splitOn "/" pattern
        addVar rt (':':_) = routeVar rt
        addVar rt name = routeName name rt

setSession :: Monad m => String -> Action t m ()
setSession cookie = StateT $ \(req, resp, prm) ->
  let cookieHeader = S.pack $ "Set-Cookie: _sess=" ++ cookie ++ "; path=/;"
  in return $ ((), (req, resp { respHeaders = cookieHeader:(respHeaders resp)}, prm))

destroySession :: Monad m => Action t m ()
destroySession = StateT $ \(req, resp, prm) ->
  let cookieHeader = S.pack $ "Set-Cookie: _sess=; path=/; expires=Thu, Jan 01 1970 00:00:00 UTC;"
  in return $ ((), (req, resp { respHeaders = cookieHeader:(respHeaders resp)}, prm))

requestHeader :: Monad m => S.ByteString -> Action t m (Maybe S.ByteString)
requestHeader name = do
  httpReq <- getHttpReq
  return $ lookup name (reqHeaders httpReq)

getHttpReq :: Monad m => Action t m (HttpReq t)
getHttpReq = StateT $ \(req, resp, prm) -> return $ (req, (req, resp, prm))

params :: Monad m => Action t m ([Param])
params = StateT $ \s@(_, _, prm) -> return (prm, s)

param :: Monad m => S.ByteString -> Action t m (Maybe Param)
param key = do
  prms <- params
  return $ foldl go Nothing prms
  where go Nothing p = if paramKey p == key then Just p else Nothing
        go a _ = a


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
  (_, (_, response, _)) <- lift $ (runStateT action) (req, mkHttpHead stat200, pathLstParams ++ prms)
  return $ response


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

