{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.Trans
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.IterIO.Server.TCPServer
import Data.Monoid
import Data.Maybe
import System.Random

import Text.Blaze.Renderer.Utf8 (renderHtml)
import Text.Blaze.Html5 hiding (param)
import Text.Blaze.Html5.Attributes hiding (id, min, max)

import Data.IterIO.Http.Support

type L = L8.ByteString

main :: IO ()
main = runTCPServer httpServer

quotes :: [String]
quotes = [  
            "Now with more cowbell."
          , "Hot potato grand champion."
          , "Goes all the way to eleven."
          , "Does the Kessel Run in less than 12 parsecs."
          , "Will work for hugs."
          , "Ate a whole wheel of cheese."
          , "Fueled by Mountain Dew"
          , "Saw a MiG 28 do a 4g negative dive."
          , "Has never started a land war in Asia."
          , "Has, like, a ton of Facebook friends."
          , "Always chooses the write homophone."]

httpServer = simpleHttpServer 8080 $ runIterActionRoute $ mconcat [
    routeTop $ routeAction welcomeAction
  , routeActionPattern "/factoid/:index" quoteAction
  , routeActionPattern "/factoid" indexAction
  ]

welcomeAction :: Action t L IO ()
welcomeAction = do
  render "text/html" $ renderHtml $ docTypeHtml $ do
    body $ do
      h1 "Welcome to my simple site!"
      p $ do
        "Click "; a ! href "/factoid" $ "here"; " to learn somethine about me."

indexAction :: Action t L IO ()
indexAction = do
  idx <- lift $ getStdRandom (randomR (0,length quotes - 1)) 
  let quote = quotes !! idx
  render "text/html" $ renderHtml $ docTypeHtml $ do
    body $ do
      h1 "A Random quote:"
      p $ do
        "Johnny Carson "
        a ! href (toValue $ "/factoid/" ++ (show idx)) $ toHtml quote

quoteAction :: Action t L IO ()
quoteAction = do
  (Just idx') <- param "index" 
  let idx   = maybe 0 id $ maybeRead $ (L8.unpack . paramValue) idx'
      quote = quotes !! (max 0 (min idx (length quotes -1)))
  render "text/html" $ renderHtml $ docTypeHtml $ do
    body $ do
      h1 $ do "Johnny Carson "; toHtml quote
      p $ a ! href "/factoid" $ "Another random quote..."
    where maybeRead = fmap fst . listToMaybe . reads
