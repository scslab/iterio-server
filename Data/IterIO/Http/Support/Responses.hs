module Data.IterIO.Http.Support.Responses (
    render
  , redirectTo
  , respond404
  , respondStat
) where

import Control.Monad.Trans.State
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import Data.IterIO
import Data.IterIO.Http.Support.Action (Action)
import Data.IterIO.Http
import Data.IterIO.HttpRoute

respond404 :: Monad m => Action t m ()
respond404 = StateT $
  \(req, _, params) -> return $ ((), (req, resp404 req, params))

respondStat :: Monad m => HttpStatus -> Action t m ()
respondStat status = StateT $
  \(req, resp, params) -> return $ ((), (req, resp { respStatus = status }, params))

redirectTo :: Monad m => String -> Action t m ()
redirectTo path = StateT $
  \(req, _, params) -> return $ ((), (req, resp303 path, params))

render :: Monad m => String -> L.ByteString -> Action t m ()
render ctype text = StateT $ \(req, resp, prm) -> return $ ((), (req, mkResp resp, prm))
  where len = S.pack $ "Content-Length: " ++ show (L.length text)
        ctypeHeader = S.pack $ "Content-Type: " ++ ctype
        mkResp resp = resp { respHeaders = respHeaders resp ++ [ctypeHeader, len],
                        respBody = inumPure text }
