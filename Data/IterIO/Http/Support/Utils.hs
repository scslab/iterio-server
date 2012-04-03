{-# LANGUAGE CPP #-}
#if defined(__GLASGOW_HASKELL__) && (__GLASGOW_HASKELL__ >= 702)
{-# LANGUAGE Safe #-}
#endif

module Data.IterIO.Http.Support.Utils where

import Data.ByteString.Lazy.Char8
import Data.IterIO
import Data.IterIO.Http
import Data.IterIO.Http.Support.Action

-- | For 'Action's where the body type is a 'ByteString', parse the
-- body with 'parseParams\'' and prepend the result to the 'Action''s
-- 'Param's
parseParams :: Monad m => Action t ByteString m [Param]
parseParams = do
  req <- getHttpReq
  body <- getBody
  prms0 <- params
  prms1 <- parseParams' req body
  setParams $ prms1 ++ prms0

-- | Parse url encoded or form encoded paramters from an HTTP
-- body.
parseParams' :: Monad m => HttpReq a -> ByteString -> m [Param]
parseParams' req body = inumPure body |$ foldForm req handle []
  where handle accm field = do
          val <- pureI
          return $ (Param (ffName field) val (ffHeaders field)):accm

