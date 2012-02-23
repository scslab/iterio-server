{-# LANGUAGE OverloadedStrings #-}
module Views where

import Prelude hiding (head, id, div)
import Text.Blaze.Renderer.Utf8 (renderHtml)
import Text.Blaze.Html5 hiding (param)
import Text.Blaze.Html5.Attributes hiding (form, label)

import Post

truncateStr :: Int -> String -> String
truncateStr len str =
  take len str ++
    if length str > len then "..." else "" 

layout content = renderHtml $ docTypeHtml $ do
  head $ do
    link ! href "/stylesheets/application.css" ! rel "stylesheet"
    script ! src "/javascript/application.js" $ ""
  body $ do
    h1 $ do
      a ! href "/" $ "A Blog..."
    nav $ do
      a ! href "/posts/new" $ "New Post"
    content

postsIndex posts = do
  mapM_ (\post -> do
    article $ do
      h2 $ do
        let (Just pid) = postId post
        a ! href (toValue $ "/posts/" ++ (show $ pid)) $
          toHtml $ postTitle post
      p $ toHtml $ truncateStr 500 $ postBody post
    ) posts

postsShow post = article $ do
  h2 $ do
    let (Just pid) = postId post
    a ! href (toValue $ "/posts/" ++ (show $ pid)) $
      toHtml $ postTitle post
  p $ toHtml $ postBody post

postsNew = do
  h2 "New Post"
  postsForm emptyPost

postsForm post = form ! action target ! method "POST" $ do
  div ! class_ "field" $ do
    label ! for "post_title" $ "Title:"
    br
    input ! type_ "text"
          ! id "post_title"
          ! name "title"
          ! value (toValue $ postTitle post)
  div ! class_ "field" $ do
    label ! for "post_body" $ "Body:"
    br
    textarea ! id "post_title" ! name "body" $ toHtml $ postBody post
  input ! type_ "submit" ! value "Submit"
  where target = toValue $ "/posts/" ++ (maybe "" show $ postId post)

