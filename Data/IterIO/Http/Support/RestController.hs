{-# LANGUAGE CPP #-}
#if defined(__GLASGOW_HASKELL__) && (__GLASGOW_HASKELL__ >= 702)
{-# LANGUAGE Safe #-}
#endif
{-# LANGUAGE OverloadedStrings, MultiParamTypeClasses #-}
-- | This module defines the 'RestController' class
 
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

-- |The class @RestController@ allows a set of actions to be routed using
-- RESTful HTTP verbs.
class Monad m => RestController m a where
  -- |GET \/
  restIndex :: a -> Action t m ()
  restIndex _ = respond404

  -- |GET \/:id
  --
  -- @id@ is passed in as the second parameter.
  restShow :: a -> L.ByteString -> Action t m ()
  restShow _ _ = respond404

  -- |GET \/new
  restNew :: a -> Action t m ()
  restNew _ = respond404

  -- |POST \/
  restCreate :: a -> Action t m ()
  restCreate _ = respond404

  -- |GET \/:id\/edit
  --
  -- @id@ is passed in as the second parameter.
  restEdit :: a -> L.ByteString -> Action t m ()
  restEdit _ _ = respond404

  -- |PUT \/:id
  --
  -- @id@ is passed in as the second parameter.
  --
  -- Since @PUT@ is not supported by many browsers, this action also responds to
  -- requests containing the HTTP header "X-HTTP-Method-Override: PUT"
  -- regardless of the actual HTTP method (@GET@ or @POST@)
  restUpdate :: a -> L.ByteString -> Action t m ()
  restUpdate _ _ = respond404

  -- |DELETE \/:id
  -- 
  -- @id@ is passed in as the second parameter.
  --
  -- Since @DELETE@ is not supported by many browsers, this action also responds to
  -- requests containing the HTTP header "X-HTTP-Method-Override: DELETE"
  -- regardless of the actual HTTP method (@GET@ or @POST@)
  restDestroy :: a -> L.ByteString -> Action t m ()
  restDestroy _ _ = respond404

-- |Runs an action, passing in named parameter.
runWithVar :: Monad m => S.ByteString -> (L.ByteString -> Action t m ()) -> Action t m ()
runWithVar varName controller = do
  (Just var) <- param varName
  controller $ paramValue var

-- |Routes URLs under the given @String@ to actions in a @RestController@. For
-- example
--
-- @
--    routeRestController "posts" myRestController
-- @
--
-- will map the follwoing URLs:
--
--    * GET \/posts => myRestController#restIndex
--
--    * POST \/posts => myRestController#restCreate
--
--    * GET \/posts\/:id => myRestController#restShow
--
--    * GET \/posts\/:id\/edit => myRestController#restEdit
--
--    * GET \/posts\/:id\/new => myRestController#restNew
--
--    * DELETE \/posts\/:id => myRestController#restDestroy
--
--    * PUT \/posts\/:id => myRestController#restUpdate
--
routeRestController :: RestController m a => String -> a -> HttpRoute m t
routeRestController prefix controller = routeName prefix $ mconcat [
    routeTop $ routeMethod "GET" $ routeAction $ restIndex controller
  , routeTop $ routeMethod "POST" $ routeAction $ restCreate controller
  , routeMethod "GET" $ routeActionPattern "/:id/edit" $ runWithVar "id" $ restEdit controller
  , routeMethod "GET" $ routeActionPattern "/new" $ restNew controller
  , routeMethod "GET" $ routeActionPattern "/:id" $ runWithVar "id" $ restShow controller
  , routeMethod "DELETE" $ routeActionPattern "/:id" $ runWithVar "id" $ restDestroy controller
  , routeMethod "PUT" $ routeActionPattern "/:id" $ runWithVar "id" $ restUpdate controller
  , routeMethod "POST" $ routeActionPattern "/:id" $ runWithVar "id" $ restUpdate controller
  ]

