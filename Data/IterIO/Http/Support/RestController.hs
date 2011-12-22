{-# LANGUAGE OverloadedStrings #-}
module Data.IterIO.Http.Support.RestController (
    RestController(..)
  , routeRestController
) where

import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Data.IterIO.HttpRoute
import Data.IterIO.Http.Support.Action
import Data.IterIO.Http.Support.Responses
import Data.Monoid

class RestController a where
  restIndex :: Monad m => a -> Action t m ()
  restIndex _ = respond404
  
  restShow :: Monad m => a -> L.ByteString -> Action t m ()
  restShow _ _ = respond404
  
  restNew :: Monad m => a -> Action t m ()
  restNew _ = respond404
  
  restCreate :: Monad m => a -> Action t m ()
  restCreate _ = respond404
  
  restEdit :: Monad m => a -> L.ByteString -> Action t m ()
  restEdit _ _ = respond404
  
  restUpdate :: Monad m => a -> L.ByteString -> Action t m ()
  restUpdate _ _ = respond404
  
  restDestroy :: Monad m => a -> L.ByteString -> Action t m ()
  restDestroy _ _ = respond404

runWithVar :: Monad m => S.ByteString -> (L.ByteString -> Action t m ()) -> Action t m ()
runWithVar varName controller = do
  (Just var) <- param varName
  controller $ paramValue var

routeRestController :: (Monad m, RestController a) => String -> a -> HttpRoute m t
routeRestController prefix controller = routeName prefix $ mconcat [
    routeTop $ routeMethod "GET" $ routeAction $ restIndex controller
  , routeTop $ routeMethod "POST" $ routeAction $ restCreate controller
  , routeMethod "GET" $ routeActionPattern "/:id/edit" $ runWithVar "id" $ restEdit controller
  , routeMethod "GET" $ routeActionPattern "/new" $ restNew controller
  , routeMethod "GET" $ routeActionPattern "/:id" $ runWithVar "id" $ restShow controller
  , routeMethod "POST" $ routeActionPattern "/:id/delete" $ runWithVar "id" $ restDestroy controller
  , routeMethod "POST" $ routeActionPattern "/:id" $ runWithVar "id" $ restUpdate controller
  ]