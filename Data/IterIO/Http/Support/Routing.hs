{-# LANGUAGE OverloadedStrings #-}
-- |Utility functions for routing.
module Data.IterIO.Http.Support.Routing (
    runLHttpRoute
    ) where

import Data.Char
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString.Char8 as S
import Data.Maybe
import Data.IterIO.Http
import Data.IterIO.HttpRoute
import Data.IterIO.Iter

-- |Converts a 'ByteString' to upper-case
upcase :: S.ByteString -> S.ByteString
upcase = S.map toUpper

-- |Like 'runHttpRoute' but replaces @GET@ and @POST@ request headers with the
-- value of the @X-HTTP-Method-Override@ HTTP header if it is present. This
-- allows applicaitons to respond to @DELETE@ and @PUT@ methods even though
-- many browsers do not support those methods.
runLHttpRoute :: Monad m
              => HttpRoute m s
              -> HttpReq s
              -> Iter L.ByteString m (HttpResp m)
runLHttpRoute route req = runHttpRoute route $ transformedReq
  where method = upcase $ reqMethod req
        overrideHeader = lookup "X-HTTP-Method-Override" (reqHeaders req)
        transformedReq
          | method /= "GET" && method /= "POST" = req
          | isJust overrideHeader =
              req{reqMethod = (upcase . fromJust $ overrideHeader)}
          | otherwise = req

