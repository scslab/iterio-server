{-# LANGUAGE CPP #-}
#if defined(__GLASGOW_HASKELL__) && (__GLASGOW_HASKELL__ >= 702)
{-# LANGUAGE Safe #-}
#endif
{-# LANGUAGE OverloadedStrings #-}
-- |Utility functions for routing.
module Data.IterIO.Http.Support.Routing (
    ActionRoute(..)
  , runActionRoute, runIterAction, runAction
  , routeAction, routePattern
  , routeName, routeVar
  , routeTop, routeMethod, routeFileSys
    ) where

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
import Data.IterIO.Http.Support.Responses

import System.FilePath.Posix
import System.Posix.Files

-- |Converts a 'ByteString' to upper-case
upcase :: S.ByteString -> S.ByteString
upcase = S.map toUpper

-- | Like 'runAction' but consumes the rest of the request for the
-- body
runIterAction :: Monad m
              => Action s L.ByteString m a
              -> HttpReq s
              -> Iter L.ByteString m (HttpResp m)
runIterAction act req = do
  body <- inumHttpBody req .| pureI
  lift $ runAction act req body

-- | Runs an 'ActionRoute'. If it satisfies the request, the
-- underlying 'Action' is returned, otherwise an 'Action' responding
-- with HTTP 404 (Not Found) is returned instead. Can be used at the
-- top of a request handler, for example:
-- 
-- > httpHandler :: Action b m ()
-- > httpHandler = runActionRoute $ mconcat [
-- >     routeTop $ routeAction homeAction
-- >   , routeMethod "POST" $ routeAction handleForm
-- >   , routeMethod "GET" $ routeAction showForm
-- >   ]
-- 
-- But also can be nested inside of another action to created nested
-- routes, or state dependant routes:
-- 
-- > httpHandler :: Action b m ()
-- > httpHandler = runActionRoute $ mconcat [
-- >     routeTop $ routeAction homeAction
-- >   , routeName "foo" $ routeAction $ runActionRoute $ mconcat [
-- >         routeMethod "GET" $ runAction showForm
-- >       , routeMethod "POST" $ runAction handleForm
-- >       ]
-- >   ]
--
-- or
--
-- > handleForm = do
-- >   day <- lift $ getDayOfWeek
-- >   case mod day 2 of
-- >     0 -> runActionRoute $ routeName "stts" $ routeAction doRes
-- >     1 -> runActionRoute $ routeName "mwf" $ routeAction doRes
--
runActionRoute :: Monad m
              => ActionRoute b m s
              -> Action s b m ()
runActionRoute (ActionRoute route) = do
  req <- gets actionReq
  res <- lift $ route req
  fromMaybe respond404 res

-- | An @ActionRoute@ either matches an 'HttpReq' and returns a
-- corresponding 'Action' that responds to it, or 'Nothing'
-- signifying, no approriate 'Action' was found. @ActionRoute@s can
-- be strung together with, e.g., 'mappend' such that each will be
-- searched in turn until an 'Action' responding to the 'HttpReq' is
-- found.
newtype ActionRoute b m s = ActionRoute (HttpReq s -> m (Maybe (Action s b m ())))
-- TODO(alevy): Implement ActionRoute in terms of Reader

instance Monad m => Monoid (ActionRoute b m s) where
  mempty = ActionRoute $ const $ return Nothing
  mappend (ActionRoute a) (ActionRoute b) =
    ActionRoute $ \req -> do
      f <- a req
      case f of
        Just _ -> return f
        Nothing -> b req

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
routeName :: Monad m => String -> ActionRoute b m s -> ActionRoute b m s
routeName name (ActionRoute route) = ActionRoute check
    where sname = S.pack name
          headok (h:_) | h == sname = True
          headok _                  = False
          check req | headok (reqPathLst req) = route $ popPath False req
          check _                             = return Nothing

-- | Matches any directory name, but additionally pushes it onto the
-- front of the 'reqPathParams' list in the 'HttpReq' structure.  This
-- allows the name to serve as a variable argument to the eventual
-- handling function.
routeVar :: Monad m => ActionRoute b m s -> ActionRoute b m s
routeVar (ActionRoute route) = ActionRoute check
    where check req = case reqPathLst req of
                        _:_ -> route $ popPath True req
                        _   -> return Nothing

-- | Match request with path \"/\".
routeTop :: Monad m => ActionRoute b m s -> ActionRoute b m s
routeTop (ActionRoute route) = ActionRoute $ \req ->
                               if null $ reqPathLst req then route req
                               else return Nothing

-- | Match requests with the given method (\"GET\", \"POST\", etc).
routeMethod :: Monad m => String -> ActionRoute b m s -> ActionRoute b m s
routeMethod method (ActionRoute route) = ActionRoute check
  where smethod = S.pack method
        check req | reqMethod req /= smethod = return Nothing
                  | otherwise = route req

-- | Routes an 'Action'
routeAction :: Monad m => Action t b m () -> ActionRoute b m t
routeAction action = ActionRoute $ const . return . Just $ action

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
routePattern :: Monad m => String -> ActionRoute b m t -> ActionRoute b m t
routePattern pattern action = foldl' addVar (routeActionWithRouteNames patternList action) patternList
  where patternList = reverse $ filter (not . null) $ splitOn "/" pattern
        addVar rt (':':_) = routeVar rt
        addVar rt name = routeName name rt

routeActionWithRouteNames :: Monad m
          => [String]
          -> ActionRoute b m s
          -> ActionRoute b m s
routeActionWithRouteNames routeNames (ActionRoute route) = ActionRoute $ \req -> do
  mact <- route req
  case mact of
    Just act -> return . Just $ do
      prms <- params
      _ <- setParams (prms ++ pathLstToParams req routeNames)
      act
    Nothing -> return Nothing

-- | Runs an 'Action' given an 'HttpReq' and body. Returns the 'HttpResp'
-- generated by the 'Action'.
runAction :: Monad m
          => Action s b m a
          -> HttpReq s
          -> b
          -> m (HttpResp m)
runAction action req body = do
  let s = ActionState transformedReq (mkHttpHead stat200) [] body
  (_, result) <- runStateT action s
  return $ actionResp result
  where method = upcase $ reqMethod req
        overrideHeader = lookup "x-http-method-override" (reqHeaders req)
        transformedReq
          | method /= "GET" && method /= "POST" = req
          | isJust overrideHeader =
              req{reqMethod = (upcase . fromJust $ overrideHeader)}
          | otherwise = req

pathLstToParams :: HttpReq s -> [String] -> [Param]
pathLstToParams req routeNames = result
  where (result, _) = foldl go ([], reqPathParams req) routeNames
        go (prms, plst@(_:_)) (':':var) = ((transform var $ head plst):prms, tail plst)
        go s _ = s
        transform k v = Param (S.pack k) (L.fromChunks [v]) []

routeFileSys :: (String -> S.ByteString)
             -> FilePath
             -> ActionRoute b IO t
routeFileSys mimeMap root = ActionRoute $ \req -> do
  mresp <- check req
  case mresp of
    Just resp -> return $ Just $ modify $ \s -> s { actionResp = resp }
    _ -> return Nothing
  where check req = do 
          let path = foldl (</>) root (map S.unpack $ reqPathLst req)
          exist <- liftIO $ fileExist path
          if exist then do
            st <- liftIO $ getFileStatus path
            case () of
              _ | isRegularFile st -> doFile req path st
                | otherwise -> return Nothing
            else return Nothing
        doFile req path st
            | reqMethod req == "HEAD" =
                return $ Just $ resp { respStatus = stat200 }
            | reqMethod req == "GET" = do
                return $ Just $
                  resp { respBody = enumFile' path }
            | otherwise = return Nothing
          where resp = defaultHttpResp { respChunk = False
                                       , respHeaders = mkHeaders req st }
        mkHeaders req st = 
          [ ("Content-Length", S.pack . show $ fileSize st)
          , ("Content-Type", mimeMap $ fileExt req) ]
        fileExt req =
          drop 1 $ takeExtension $ case reqPathLst req of
                                     [] -> "."
                                     l  -> S.unpack $ last l

