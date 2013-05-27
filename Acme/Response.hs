{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module Acme.Response where

import Acme.Types                 (Response(..))
import Data.ByteString            (ByteString, concat, append)
import Data.ByteString.Char8      (pack)
import Prelude                    hiding (concat)
import qualified Data.Map.Lazy    as M

------------------------------------------------------------------------------
-- send a response
------------------------------------------------------------------------------

sendResponse :: (ByteString -> IO ()) -> Response -> IO ()
sendResponse send ByteStringResponse{..} =
    send . concat $ 
        statusLine rsCode rsStatus : (formatHeaders rsHeaders)
        ++ [rsBody]
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

statusLine :: Int -> ByteString -> ByteString
statusLine 200  _   = okStatus
statusLine code msg = "SIP/2.0 " `append` (pack . show) code
                                 `append` " "
                                 `append` msg
                                 `append` "\r\n"

okStatus :: ByteString
okStatus = "SIP/2.0 200 OK\r\n"


