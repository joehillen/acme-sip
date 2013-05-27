module Acme.Serve where

import Acme.Request
import Acme.Response
import Acme.Types
import Acme.Error
import Control.Concurrent (killThread, forkIO)
import Control.Exception     (try)
import Control.Exception.Extensible as E
import Control.Monad         (forever)
import Control.Monad.Trans
import Data.ByteString       (ByteString, empty)
import Data.ByteString.Char8 (pack)
import Network.BSD           (getProtocolNumber)
import Network.Socket        ( Socket, SockAddr(..), SocketOption(..)
                             , SocketType(Stream), SocketType(Datagram)
                             , Family(AF_INET) , accept, bindSocket
                             , iNADDR_ANY, sClose, listen, maxListenQueue
                             , setSocketOption, socket
                             )
import Network.Socket.ByteString    (recv, recvFrom, sendAll, sendAllTo)
import System.IO



-- | start TCP listening on a port
listenTCP :: Int  -- ^ port number
         -> IO Socket
listenTCP portm = do
    proto <- getProtocolNumber "tcp"
    E.bracketOnError
        (socket AF_INET Stream proto)
        sClose
        (\sock -> do
            setSocketOption sock ReuseAddr 1
            setSocketOption sock NoDelay 1
            bindSocket sock (SockAddrInet (fromIntegral portm) iNADDR_ANY)
            listen sock (max 1024 maxListenQueue)
            return sock
        )

-- | start UDP listening on a port
listenUDP :: Int  -- ^ port number
         -> IO Socket
listenUDP portm = do
    proto <- getProtocolNumber "udp"
    E.bracketOnError
        (socket AF_INET Datagram proto)
        sClose
        (\sock -> do
            setSocketOption sock ReuseAddr 1
            bindSocket sock (SockAddrInet (fromIntegral portm) iNADDR_ANY)
            return sock
        )

-- | listen on a port and handle 'Requests'
serve ::  Int                     -- ^ port to listen on
      -> (Request -> IO Response)  -- ^ request handler
      -> IO ()
serve port app = do
    sockTCP <- listenTCP port
    sockUDP <- listenUDP port
    serveSocket sockUDP sockTCP app
    sClose sockTCP
    sClose sockUDP

-- | handle 'Requests' from an already listening 'Socket'
serveSocket :: Socket                  -- ^ 'Socket' in listen mode
            -> Socket
            -> (Request -> IO Response) -- ^ request handler
            -> IO ()
serveSocket sockUDP sockTCP app =
    forever $ do
        let readerUDP = recvFrom sockUDP 4096
            writerUDP = sendAllTo sockUDP
        forkIO $ requestLoopUDP
                    False
                    readerUDP
                    writerUDP
                    app `E.catch` (\ConnectionClosed -> return ())
        (sock, addr) <- accept sockTCP
        let readerTCP = recv sock 4096
            writerTCP = sendAll sock
        forkIO $ do
            requestLoopTCP
                False
                addr
                readerTCP
                writerTCP
                app `E.catch` (\ConnectionClosed -> return ())
            sClose sock

requestLoopTCP :: Bool
               -> SockAddr
               -> IO ByteString
               -> (ByteString -> IO ())
               -> (Request -> IO Response)
               -> IO ()
requestLoopTCP secure addr reader writer app = go empty
    where
        go bs = do
           r <- try $ parseRequest reader bs secure
           case r of
             Left ex -> do
                putStrLn "left"
                exceptionHandler writer ex
                go empty
             Right (request, bs') -> do
                sendResponse writer =<< app request
                go bs'


--requestLoop :: Bool
--            -> IO ByteString
--            -> (ByteString -> IO ())
--            -> (Request -> IO Response)
--            -> IO ()
requestLoopUDP secure reader writer app = go empty
    where
        go bs = do
            (msg, addr) <- reader
            let writer' bs = writer bs addr
            (request, bs') <- parseRequest (return msg) bs secure
            sendResponse writer' =<< app request
            go bs'
