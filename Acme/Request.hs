{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}
module Acme.Request where

import Control.Monad.Trans             (lift, liftIO)
import Control.Exception.Extensible
import           Data.ByteString       ( ByteString, elemIndex, empty, split
                                       , uncons
                                       )
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as C
import Data.ByteString.Unsafe          (unsafeDrop, unsafeIndex, unsafeTake)
import Data.Monoid                     (mappend)
import Data.Typeable                   (Typeable)
import Acme.Types                      ( ConnectionClosed(..), SIPVersion(..)
                                       , Method(..), Request(..), cr, colon
                                       , nl, space
                                       )
import Data.List                       (find)

------------------------------------------------------------------------------
-- Parse Exception
------------------------------------------------------------------------------

data ParseError
    = Unexpected
    | MalformedRequestLine ByteString
    | MalformedHeader      ByteString
    | MissingHeader        ByteString
    | UnknownSIPVersion    ByteString
      deriving (Typeable, Show, Eq)

instance Exception ParseError

------------------------------------------------------------------------------
-- Request Parser
------------------------------------------------------------------------------


{-
        Request       = Request-Line              ; Section 5.1
                        *(( general-header        ; Section 4.5
                         | request-header         ; Section 5.3
                         | entity-header ) CRLF)  ; Section 7.1
                        CRLF
                        [ message-body ]          ; Section 4.3
-}
parseRequest :: IO ByteString -> ByteString -> Bool -> IO (Request, ByteString)
parseRequest getChunk bs secure =
    do (line, bs')     <- takeLine getChunk bs
       let (method, requestURI, sipVersion) = parseRequestLine line
       (headers, bs'') <- parseHeaders getChunk bs'
       let toHeader = getHeader "To" headers
       let callID = getHeader "Call-ID" headers
       let cseq = getHeader "CSeq" headers
       let request = Request { rqMethod      = method
                             , rqURIbs       = requestURI
                             , rqSIPVersion  = sipVersion
                             , rqToHeader    = toHeader
                             , rqCallID      = callID
                             , rqCseq        = cseq
                             , rqHeaders     = headers
                             , rqSecure      = secure
                             , rqBody        = empty
                             }
--       liftIO $ print request
       return (request, bs'')

getHeader name headers =
    let header = find (\(k,v) -> k == name) headers
    in case header of
        Just (k, v) -> v
        _           -> throw (MissingHeader name)

{-
   The Request-Line ends with CRLF.  No CR or LF are allowed except in
   the end-of-line CRLF sequence.  No linear whitespace (LWS) is allowed
   in any of the elements.

         Request-Line  =  Method SP Request-URI SP SIP-Version CRLF

-}
parseRequestLine :: ByteString -> (Method, ByteString, SIPVersion)
parseRequestLine bs =
    case split space bs of
      [method, requestURI, sipVersion] ->
          (parseMethod method, requestURI, parseSIPVersion sipVersion)
      _ -> throw (MalformedRequestLine bs)


{-

The Method token indicates the method to be performed on the resource
identified by the Request-URI. The method is case-sensitive.

       Method   = "INVITE"
                | "ACK"
                | "BYE"
                | "CANCEL"
                | "REGISTER"
                | "OPTIONS"
                | "INFO" -- [RFC2976]
                | extension-method
       extension-method = token
-}
parseMethod :: ByteString -> Method
parseMethod bs
    | bs == "INVITE"   = INVITE
    | bs == "ACK"      = ACK
    | bs == "BYE"      = BYE
    | bs == "CANCEL"   = CANCEL
    | bs == "REGISTER" = REGISTER
    | bs == "OPTIONS"  = OPTIONS
    | bs == "INFO"     = INFO
    | otherwise        = EXTENSION bs


parseSIPVersion :: ByteString -> SIPVersion
parseSIPVersion bs
    | bs == "SIP/2.0" = SIP20
    | otherwise       = throw (UnknownSIPVersion bs)

parseHeaders :: IO ByteString
             -> ByteString
             -> IO ([(ByteString, ByteString)], ByteString)
parseHeaders getChunk remainder =
    do (line, bs) <- takeLine getChunk remainder
       if B.null line
          then return ([], bs)
          else do
            (headers, bs') <- parseHeaders getChunk bs
            return (((parseHeader line) : headers),  bs')


{-
       message-header = field-name ":" [ field-value ]
       field-name     = token
       field-value    = *( field-content | LWS )
       field-content  = <the OCTETs making up the field-value
                        and consisting of either *TEXT or combinations
                        of token, separators, and quoted-string>
-}
parseHeader :: ByteString -> (ByteString, ByteString)
parseHeader bs =
    let (fieldName, remaining) = parseToken bs
    in case uncons remaining of
         (Just (c, fieldValue))
             | c == colon -> (fieldName, fieldValue)
         _                -> throw (MalformedHeader bs)

{-
       token          = 1*<any CHAR except CTLs or separators>
       separators     = "(" | ")" | "<" | ">" | "@"
                      | "," | ";" | ":" | "\" | <">
                      | "/" | "[" | "]" | "?" | "="
                      | "{" | "}" | SP | HT
       CTL            = <any US-ASCII control character
                        (octets 0 - 31) and DEL (127)>
-}
-- FIXME: follow the spec
parseToken :: ByteString -> (ByteString, ByteString)
parseToken = B.span (/= colon)

-- | find a line terminated by a '\r\n'
takeLine :: IO ByteString -> ByteString -> IO (ByteString, ByteString)
takeLine getChunk bs =
    -- find the index of the next '\n'
    case elemIndex nl bs of
      Nothing ->
           do x <- getChunk
              if B.null x
                 then throw ConnectionClosed
                 else takeLine getChunk (bs `mappend` x)
      (Just 0) -> do
        print "No newline found"
        throw Unexpected
      (Just i) -> return $
         -- check if the '\n' was preceded by '\r'
         if unsafeIndex bs (i - 1) == cr
            then (unsafeTake (i - 1) bs, unsafeDrop (i + 1) bs)
            else (unsafeTake i bs, unsafeDrop (i + 1) bs)
