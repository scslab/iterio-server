-- |Generic building blocks for creating TCP Servers based on 'IterIO'
module Data.IterIO.Server.TCPServer (
  TCPServer,
  runTCPServer,
  simpleHttpServer,
  echoServer
) where

import Control.Concurrent.MonadIO
import Control.Monad
import qualified Data.ByteString.Lazy as L
import qualified Network.Socket as Net
import System.IO
import Data.IterIO
import Data.IterIO.Http
import Data.ListLike.IO

sockListenTCP :: Net.PortNumber -> IO Net.Socket
sockListenTCP pn = do
  sock <- Net.socket Net.AF_INET Net.Stream Net.defaultProtocol
  Net.setSocketOption sock Net.ReuseAddr 1
  Net.bindSocket sock (Net.SockAddrInet pn Net.iNADDR_ANY)
  Net.listen sock Net.maxListenQueue
  return sock

-- |'TCPServer' holds all the information necessary to run
-- bind to a sock and respond to TCP requests from the network.
data TCPServer inp m out = TCPServer {
    serverPort :: Net.PortNumber
  , serverHandler :: Inum inp out m ()
}

instance Show (TCPServer inp m out) where
  show s = "TCPServer { serverPort: " ++ (show $ serverPort s) ++ " }"

-- |Runs a 'TCPServer' in a loop.
runTCPServer :: (ListLikeIO inp e, ListLikeIO out e,
                 ChunkData inp, ChunkData out, HasFork m)
              => TCPServer inp m out
              -> m ()
runTCPServer server = do
  sock <- liftIO $ sockListenTCP $ serverPort server
  forever $ do
    (iter, enum) <- liftIO $ do
      (s, _) <- Net.accept sock
      h <- Net.socketToHandle s ReadWriteMode
      hSetBuffering h NoBuffering
      return (handleI h, enumHandle h)
    _ <- fork $ do
      enum |$ serverHandler server .| iter
    return ()

-- |Creates a simple HTTP server from an 'HTTPRequestHandler'.
simpleHttpServer :: (HasFork m)
                =>  Net.PortNumber
                ->  HttpRequestHandler m ()
                -> TCPServer L.ByteString m L.ByteString
simpleHttpServer port reqHandler = TCPServer port httpAppHandler
  where httpAppHandler = mkInumM $ do
          req <- httpReqI
          resp <- liftI $ reqHandler req
          irun $ enumHttpResp resp Nothing
-- |Creates a 'TCPServer' that echoes each line from the client until EOF.
echoServer :: (HasFork m) => Net.PortNumber -> TCPServer String m String
echoServer port = TCPServer port echoAppHandler
  where echoAppHandler = mkInumM $ forever $ do
          input <- safeLineI
          case input of
            Just output -> irun $ enumPure $ output ++ "\r\n"
            Nothing -> irun $ enumPure []

