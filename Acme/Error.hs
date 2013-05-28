{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}
module Acme.Error where

import Control.Exception.Extensible    (Exception)
import Data.ByteString                 (ByteString, empty, pack, append)
import Data.Typeable                   (Typeable)
import Acme.Response                   (sendResponse)
import Acme.Types                      ( SIPVersion(..), Response(..)
                                       , Method(..), Request(..), cr, colon
                                       , nl, space, lstrip, Headers
                                       , ppResponse
                                       )
import qualified Data.Map.Lazy    as M


------------------------------------------------------------------------------
-- Exceptions
------------------------------------------------------------------------------

-- | thrown when the remote-side closes the connection
data ConnectionClosed
    = ConnectionClosed
      deriving (Typeable, Show)

instance Exception ConnectionClosed

------------------------------------------------------------------------------
-- Parse Exception
------------------------------------------------------------------------------

data ParseError
    = Unexpected
    | MalformedRequestLine ByteString
    | MalformedHeader      ByteString
    | MissingHeader        ByteString Headers
    | UnknownSIPVersion    ByteString
      deriving (Typeable, Show, Eq)

instance Exception ParseError

errorResponse :: Int -> ByteString -> Headers -> Response
errorResponse code msg headers =
    ByteStringResponse
      { rsCode    = code
      , rsStatus  = msg
      , rsHeaders = M.toList headers
      , rsBody    = empty
      }

exceptionHandler :: (ByteString -> IO ())
                 -> ParseError
                 -> IO ()
exceptionHandler writer error = do
    let (msg, headers) =
         case error of
          MissingHeader name headers ->
            ("Missing " `append` name  `append` " header field", headers)
          otherwise                  -> ("", M.empty)
    let response = errorResponse 404 msg headers
    print $ ppResponse response
    sendResponse writer response

