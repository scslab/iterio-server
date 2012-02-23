{-# LANGUAGE OverloadedStrings #-}
module Post where

import qualified Data.ByteString.Lazy.Char8 as L
import Data.IterIO.Http.Support.Action
import Data.Maybe
import Database.HSQL
import Database.HSQL.SQLite3
import System.IO

db :: IO Connection
db = connect "blog.sqlite3" ReadWriteMode

data Post = Post { postId :: Maybe Integer
                 , postTitle :: String
                 , postBody :: String
                 }
  deriving (Show)

emptyPost :: Post
emptyPost = Post Nothing "" ""

newPost :: [Param] -> IO Post
newPost prms = let post = Post Nothing (L.unpack title) (L.unpack body)
               in insertPost post >> return post
  where lookup' key [] = Nothing
        lookup' key (p:ps)
          | key == paramKey p = Just p
          | otherwise = lookup' key ps
        title = paramValue $ fromJust $ lookup' "title" prms
        body = paramValue $ fromJust $ lookup' "body" prms

insertPost :: Post -> IO ()
insertPost post = do
  conn <- db
  stmt <- query conn $ "insert into posts (title, body) values('" 
                                      ++ postTitle post  ++ "','"
                                      ++ postBody post   ++ "')"
  closeStatement stmt

toPost :: Statement -> IO Post
toPost stmt = do
  pid <- getFieldValue stmt "id"
  title <- getFieldValue stmt "title"
  body <- getFieldValue stmt "body"
  return $ Post (Just pid) title body

findPosts :: IO [Post]
findPosts = do
  conn <- db
  stmt <- query conn "select * from posts"
  collectRows toPost stmt

findPost :: Integer -> IO Post
findPost pid = do
  conn <- db
  stmt <- query conn $ "select * from posts where id = " ++ (show pid)
  fetch stmt
  toPost stmt


