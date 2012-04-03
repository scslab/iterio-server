{-# LANGUAGE OverloadedStrings, MultiParamTypeClasses #-}
import Control.Monad.Trans
import qualified Data.ByteString.Lazy.Char8 as L
import Data.IterIO.Server.TCPServer
import Data.IterIO.Http.Support
import Data.Monoid

import Views
import Post

main :: IO ()
main = runTCPServer $ simpleHttpServer 8080 $ runIterAction $ do
  parseParams
  runActionRoute $ mconcat
    [ routeTop $ routeAction $ restIndex PostsController
    , routeRestController "posts" PostsController
    , routeFileSys mimes "public"
    ]

mimes "html" = "text/html"
mimes "css" = "text/css"
mimes "js" = "text/javascript"

data PostsController = PostsController

instance RestController () L.ByteString IO PostsController where
  restIndex _ = do
    posts <- lift $ findPosts
    render "text/html" $ layout $ postsIndex posts

  restShow _ pid = do
    post <- lift $ findPost $ read . L.unpack $ pid
    render "text/html" $ layout $ postsShow post

  restNew _ = do
    render "text/html" $ layout postsNew

  restCreate _ = do
    ps <- params
    post <- lift $ insertPost $ newPost ps
    redirectTo "/"

