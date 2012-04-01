{-# LANGUAGE Safe #-}

module Data.IterIO.Http.Support.Utils where

import qualified Data.ByteString.Lazy.Char8 as L8
import Data.IterIO
import Data.IterIO.Http
import Data.IterIO.Http.Support.Action

parseParams :: Monad m => Action t L8.ByteString m [Param]
parseParams = do
  req <- getHttpReq
  body <- getBody
  prms0 <- params
  prms1 <- parseParams' req body
  setParams $ prms1 ++ prms0

parseParams' :: Monad m => HttpReq a -> L8.ByteString -> m [Param]
parseParams' req body = inumPure body |$ foldForm req handle []
  where handle accm field = do
          val <- pureI
          return $ (Param (ffName field) val (ffHeaders field)):accm

