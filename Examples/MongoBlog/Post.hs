{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Post where

import qualified Data.ByteString.Lazy.Char8 as L
import Data.IterIO.Http.Support.Action
import Data.Maybe
import System.IO

import Data.Bson
import Data.Typeable
import Database.MongoDB.Structured
import Database.MongoDB.Structured.Deriving.TH
import Database.MongoDB.Structured.Query

import Control.Monad
import Control.Exception

-- | Perform action on DB. This is slow because it always tears down
-- the connection.
withDB act = do
   pipe <- runIOE $ connect (host "localhost")
   qr <- access pipe master "auth" act
   close pipe
   case qr of
    Right r -> return r
    Left e  -> throwIO . userError $ "Failed with: " ++ show e

data Post = Post { postId    :: SObjId
                 , postTitle :: String
                 , postBody  :: String
                 } deriving (Eq, Show, Typeable)
$(deriveStructured ''Post)

emptyPost :: Post
emptyPost = Post noSObjId "" ""

newPost :: [Param] -> IO Post
newPost prms = do
  let post = emptyPost { postTitle =  (L.unpack title)
                       , postBody  = (L.unpack body) }
  oID <- insertPost post
  return $ post { postId = oID }
    where lookup' key [] = Nothing
          lookup' key (p:ps)
            | key == paramKey p = Just p
            | otherwise = lookup' key ps
          title = paramValue $ fromJust $ lookup' "title" prms
          body = paramValue $ fromJust $ lookup' "body" prms

insertPost :: Post -> IO SObjId
insertPost post = withDB $ liftM (fromJust . cast') $ insert post

findPosts :: IO [Post]
findPosts = withDB $  do
  let query = select ( (.*) :: QueryExp Post)
  c <- find query
  liftM catMaybes $ rest c

findPost :: SObjId -> IO Post
findPost pid = withDB $ do
  let query = select (PostId .== pid)
  fetch query
