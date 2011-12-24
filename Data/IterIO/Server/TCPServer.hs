-- |Generic building blocks for creating TCP Servers based on 'IterIO'
module Data.IterIO.Server.TCPServer (
  TCPServer(..),
  runTCPServer,
  defaultServerAcceptor,
  minimalTCPServer,
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
-- |A function to transform an accept incomming connection into an iter and onum.
--  Most servers should just use 'defaultSocketAcceptor' but this can be used for
--  special cases, e.g. accepting SSL connections with 'Data.IterIO.SSL.iterSSL'.
  , serverAcceptor :: Net.Socket -> m (Iter inp m (), Onum inp m ())
-- |Must execute the monadic result. Servers operating in the 'IO' Monad can
--  use 'id'.
  , serverResultHandler :: m () -> IO ()
}

instance Show (TCPServer inp m) where
  show s = "TCPServer { serverPort: " ++ (show $ serverPort s) ++ " }"

-- |For convenience, a TCPServer in the 'IO' Monad with null defaults:
--
--    * Port 0 (next availabel port)
--
--    * Handler set to 'inumNop'
--
--    * Acceptor set to 'defaultServerAcceptor'
--
--    * Request handler set to 'id' (noop)
--
minimalTCPServer :: (ListLikeIO inp e, ChunkData inp) => TCPServer inp IO
minimalTCPServer = TCPServer 0 inumNop defaultServerAcceptor id

-- |This acceptor creates an 'Iter' and 'Onum' using 'handleI' and
--  'enumHandle' respectively.
defaultServerAcceptor ::  (ListLikeIO inp e,
                           ChunkData inp, MonadIO m)
                      => Net.Socket -> m (Iter inp m (), Onum inp m a)
defaultServerAcceptor sock = liftIO $ do
  h <- Net.socketToHandle sock ReadWriteMode
  hSetBuffering h NoBuffering
  return (handleI h, enumHandle h)

-- |Runs a 'TCPServer' in a loop.
runTCPServer :: (ListLikeIO inp e,
                 ChunkData inp, Monad m)
              => TCPServer inp m
              -> IO ()
runTCPServer server = do
  sock <- sockListenTCP $ serverPort server
  let handler = serverResultHandler server
  forever $ do
    (s, _) <- Net.accept sock
    _ <- forkIO $ handler $ do
      (iter, enum) <- (serverAcceptor server) s
      enum |$ serverHandler server .| iter
    return ()

-- |Creates a simple HTTP server from an 'HTTPRequestHandler'.
simpleHttpServer :: Net.PortNumber
                 -> HttpRequestHandler IO ()
                 -> TCPServer L.ByteString IO
simpleHttpServer port reqHandler = minimalTCPServer { serverPort = port, serverHandler = httpAppHandler }
  where httpAppHandler = mkInumM $ do
          req <- httpReqI
          resp <- liftI $ reqHandler req
          irun $ enumHttpResp resp Nothing

-- |Creates a 'TCPServer' that echoes each line from the client until EOF.
echoServer :: Net.PortNumber -> TCPServer String IO
echoServer port = minimalTCPServer { serverPort = port, serverHandler = echoAppHandler }
  where echoAppHandler = mkInumM $ forever $ do
          input <- safeLineI
          case input of
            Just output -> irun $ enumPure $ output ++ "\r\n"
            Nothing -> irun $ enumPure []

