-- |Generic building blocks for creating TCP Servers based on 'IterIO'
module Data.IterIO.Server.TCPServer (
  TCPServer(..),
  runTCPServer,
  defaultSocketAcceptor,
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

-- |Sets up a TCP socket to listen on the given port.
sockListenTCP :: Net.PortNumber -> IO Net.Socket
sockListenTCP pn = do
  sock <- Net.socket Net.AF_INET Net.Stream Net.defaultProtocol
  Net.setSocketOption sock Net.ReuseAddr 1
  Net.bindSocket sock (Net.SockAddrInet pn Net.iNADDR_ANY)
  Net.listen sock Net.maxListenQueue
  return sock

-- |'TCPServer' holds all the information necessary to run
-- bind to a sock and respond to TCP requests from the network.
data TCPServer inp m = TCPServer {
-- |The TCP port the server will listen for incomming connections on.
    serverPort :: Net.PortNumber
-- |This 'Inum' implements the actual functionality of the server. The input
--  and output of the 'Inum' correspond to the input and output of the socket.
  , serverHandler :: Inum inp inp m ()
-- |A function to accept incomming connections. Most servers should just use
-- 'defaultSocketAcceptor' but this can be used for special cases, e.g.
-- accepting SSL connections with 'Data.IterIO.SSL.iterSSL'.
  , socketAcceptor :: Net.Socket -> m (Iter inp m (), Onum inp m ())
}

instance Show (TCPServer inp m) where
  show s = "TCPServer { serverPort: " ++ (show $ serverPort s) ++ " }"

-- |This socket-acceptor simply accepts a connection from the socket and
--  creates an 'Iter' and 'Onum' using 'handleI' and 'enumHandle',
--  respectively.
defaultSocketAcceptor ::  (ListLikeIO inp e,
                           ChunkData inp, HasFork m)
                      => Net.Socket -> m (Iter inp m (), Onum inp m a)
defaultSocketAcceptor sock = liftIO $ do
  (s, _) <- Net.accept sock
  h <- Net.socketToHandle s ReadWriteMode
  hSetBuffering h NoBuffering
  return (handleI h, enumHandle h)

-- |Runs a 'TCPServer' in a loop.
runTCPServer :: (ListLikeIO inp e,
                 ChunkData inp, HasFork m)
              => TCPServer inp m
              -> m ()
runTCPServer server = do
  sock <- liftIO $ sockListenTCP $ serverPort server
  forever $ do
    (iter, enum) <- (socketAcceptor server) sock
    _ <- fork $ do
      enum |$ serverHandler server .| iter
    return ()

-- |Creates a simple HTTP server from an 'HTTPRequestHandler'.
simpleHttpServer :: (HasFork m)
                =>  Net.PortNumber
                ->  HttpRequestHandler m ()
                -> TCPServer L.ByteString m
simpleHttpServer port reqHandler = TCPServer port httpAppHandler defaultSocketAcceptor
  where httpAppHandler = mkInumM $ do
          req <- httpReqI
          resp <- liftI $ reqHandler req
          irun $ enumHttpResp resp Nothing

-- |Creates a 'TCPServer' that echoes each line from the client until EOF.
echoServer :: (HasFork m) => Net.PortNumber -> TCPServer String m
echoServer port = TCPServer port echoAppHandler defaultSocketAcceptor
  where echoAppHandler = mkInumM $ forever $ do
          input <- safeLineI
          case input of
            Just output -> irun $ enumPure $ output ++ "\r\n"
            Nothing -> irun $ enumPure []

