{-# LANGUAGE OverloadedStrings, MultiParamTypeClasses #-}
import Control.Monad.Trans
import qualified Data.ByteString.Lazy.Char8 as L
import Data.IterIO.HttpRoute
import Data.IterIO.Server.TCPServer
import Data.IterIO.Http.Support
import Data.Monoid

import Views

main :: IO ()
main = runTCPServer $ simpleHttpServer 8080 $ runLHttpRoute $ mconcat
  [ routeTop $ routeAction $ restIndex PostsController
  , routeRestController "posts" PostsController
  ]


data PostsController = PostsController

instance RestController IO PostsController where
  restIndex _ = do
    allPosts <- lift $ findPosts
    render "text/html" $ postsIndex allPosts

