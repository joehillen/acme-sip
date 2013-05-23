{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module Acme.Response where

import Acme.Types               (Response(..))
import Data.ByteString       (ByteString, concat, append)
import Data.ByteString.Char8 () -- instance IsString ByteString
import Prelude               hiding (concat)

------------------------------------------------------------------------------
-- send a response
------------------------------------------------------------------------------

sendResponse :: (ByteString -> IO ()) -> Response -> IO ()
sendResponse send ByteStringResponse{..} =
    send . concat $ statusLine rsCode : (formatHeaders rsHeaders) ++ [rsBody]
    where
      formatHeaders :: [(ByteString, ByteString)] -> [ByteString]
      formatHeaders         [] = ["\r\n"]
      formatHeaders ((f,v):hs) = [ f, ": " , v , "\r\n"] ++ formatHeaders hs

------------------------------------------------------------------------------
-- Status Lines
------------------------------------------------------------------------------

{-
  Status-Line = SIP-Version SP Status-Code SP Reason-Phrase CRLF
-}

statusLine :: Int -> ByteString
statusLine 200 = okStatus

okStatus :: ByteString
okStatus = "SIP/2.0 200 OK\r\n"


