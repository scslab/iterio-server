{-# LANGUAGE CPP #-}
#if defined(__GLASGOW_HASKELL__) && (__GLASGOW_HASKELL__ >= 702)
{-# LANGUAGE Safe #-}
#endif
{-# LANGUAGE OverloadedStrings #-}
-- |Utility functions for routing.
module Data.IterIO.Http.Support.Routing (
    ActionRoute(..)
  , runActionRoute, runIterActionRoute
  , routeAction, routeActionPattern
  , routeFn, routeConst, routeName, routeVar
  , routeTop, routeMethod
    ) where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State
import Data.Char
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString.Char8 as S
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Monoid
import Data.IterIO
import Data.IterIO.Http
import Data.IterIO.Http.Support.Action

-- |Converts a 'ByteString' to upper-case
upcase :: S.ByteString -> S.ByteString
upcase = S.map toUpper

runIterActionRoute :: Monad m
                   => ActionRoute L.ByteString m s
                   -> HttpReq s
                   -> Iter L.ByteString m (HttpResp m)
runIterActionRoute act req = do
  body <- inumHttpBody req .| pureI
  lift $ runActionRoute act req body

-- |Like 'runHttpRoute' but replaces @GET@ and @POST@ request headers with the
-- value of the @X-HTTP-Method-Override@ HTTP header if it is present. This
-- allows applicaitons to respond to @DELETE@ and @PUT@ methods even though
-- many browsers do not support those methods.
runActionRoute :: Monad m
              => ActionRoute b m s
              -> HttpReq s
              -> b
              -> m (HttpResp m)
runActionRoute (ActionRoute route) req body =
  fromMaybe (return $ resp404 req) $ route transformedReq body
  where method = upcase $ reqMethod req
        overrideHeader = lookup "X-HTTP-Method-Override" (reqHeaders req)
        transformedReq
          | method /= "GET" && method /= "POST" = req
          | isJust overrideHeader =
              req{reqMethod = (upcase . fromJust $ overrideHeader)}
          | otherwise = req

newtype ActionRoute b m s = ActionRoute (HttpReq s -> b -> Maybe (m (HttpResp m)))

instance Monoid (ActionRoute b m s) where
  mempty = ActionRoute $ const.const $ Nothing
  mappend (ActionRoute a) (ActionRoute b) =
    ActionRoute $ \req bd -> a req bd `mplus` b req bd

routeFn :: Monad m => (HttpReq s -> b -> m (HttpResp m)) -> ActionRoute b m s
routeFn fn = ActionRoute $ justify fn
  where justify fn0 a b = Just $ fn0 a b

routeConst resp = ActionRoute $ const.const $ Just $ return resp

popPath :: Bool -> HttpReq s -> HttpReq s
popPath isParm req =
    case reqPathLst req of
      h:t -> req { reqPathLst = t
                 , reqPathCtx = reqPathCtx req ++ [h]
                 , reqPathParams = if isParm then h : reqPathParams req
                                             else reqPathParams req
                 }
      _   -> error "empty path"

-- | Routes a specific directory name, like 'routeMap' for a singleton
-- map.
routeName :: String -> ActionRoute b m s -> ActionRoute b m s
routeName name (ActionRoute route) = ActionRoute check
    where sname = S.pack name
          headok (h:_) | h == sname = True
          headok _                  = False
          check req | headok (reqPathLst req) = route $ popPath False req
          check _                             = const Nothing

-- | Matches any directory name, but additionally pushes it onto the
-- front of the 'reqPathParams' list in the 'HttpReq' structure.  This
-- allows the name to serve as a variable argument to the eventual
-- handling function.
routeVar :: ActionRoute b m s -> ActionRoute b m s
routeVar (ActionRoute route) = ActionRoute check
    where check req = case reqPathLst req of
                        _:_ -> route $ popPath True req
                        _   -> const Nothing

routeTop :: ActionRoute b m s -> ActionRoute b m s
routeTop (ActionRoute route) = ActionRoute $ \req ->
                               if null $ reqPathLst req then route req
                               else const Nothing

routeMethod :: String -> ActionRoute b m s -> ActionRoute b m s
routeMethod method (ActionRoute route) = ActionRoute check
  where smethod = S.pack method
        check req | reqMethod req /= smethod = const Nothing
                  | otherwise = route req

-- | Routes an 'Action'
routeAction :: Monad m => Action t b m a -> ActionRoute b m t
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
routeActionPattern :: Monad m => String -> Action t b m a -> ActionRoute b m t
routeActionPattern pattern action = foldl' addVar (routeFn $ runActionWithRouteNames patternList action) patternList
  where patternList = reverse $ filter ((/= 0) . length) $ splitOn "/" pattern
        addVar rt (':':_) = routeVar rt
        addVar rt name = routeName name rt

runActionWithRouteNames :: Monad m
          => [String]
          -> Action s b m a
          -> HttpReq s
          -> b
          -> m (HttpResp m)
runActionWithRouteNames routeNames action req body = do
  let pathLstParams = pathLstToParams req routeNames
      s = ActionState req (mkHttpHead stat200) pathLstParams body
  (_, result) <- runStateT action s
  return $ actionResp result

runAction :: Monad m
          => Action s b m a
          -> HttpReq s
          -> b
          -> m (HttpResp m)
runAction = runActionWithRouteNames []

pathLstToParams :: HttpReq s -> [String] -> [Param]
pathLstToParams req routeNames = result
  where (result, _) = foldl go ([], reqPathParams req) routeNames
        go (prms, plst) (':':var) = ((transform var $ head plst):prms, tail plst)
        go s _ = s
        transform k v = Param (S.pack k) (L.fromChunks [v]) []

