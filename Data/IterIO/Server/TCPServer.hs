{-# LANGUAGE OverloadedStrings #-}

module Data.IterIO.Server.TCPServer (
  simpleServer,
) where

import Control.Concurrent.MonadIO
import Control.Monad
import qualified Network.Socket as Net
import System.IO
import Data.IterIO
import Data.ListLike.IO

sockListen :: Net.PortNumber -> IO Net.Socket
sockListen pn = do
  sock <- Net.socket Net.AF_INET Net.Stream Net.defaultProtocol
  Net.setSocketOption sock Net.ReuseAddr 1
  Net.bindSocket sock (Net.SockAddrInet pn Net.iNADDR_ANY)
  Net.listen sock Net.maxListenQueue
  return sock

simpleServer :: (ListLikeIO inp e, ListLikeIO out e,
                 ChunkData inp, ChunkData out, HasFork m)
              => Net.PortNumber
              -> Inum inp out m ()
              -> m ()
simpleServer port appHandler = do
  sock <- liftIO $ sockListen port
  forever $ do
    (iter, enum) <- liftIO $ do
      (s, _) <- Net.accept sock
      h <- Net.socketToHandle s ReadWriteMode
      hSetBuffering h NoBuffering
      return (handleI h, enumHandle h)
    _ <- fork $ do
      enum |$ appHandler .| iter
    return ()

