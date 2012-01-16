{-# LANGUAGE OverloadedStrings #-}
module Views where

import Text.Blaze.Renderer.Utf8 (renderHtml)
import Text.Blaze.Html5 hiding (param)
import Text.Blaze.Html5.Attributes


data Post = Post { postTitle :: String
                 , postBody :: String
                 }

toPost :: Map String SqlValue -> Post


postsIndex posts = renderHtml $ docTypeHtml $ do
  h1 "A Blog..."
  mapM posts $ do
    post <- toPost posts
    h2 
