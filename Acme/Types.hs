{-# LANGUAGE DeriveDataTypeable, RankNTypes, RecordWildCards #-}
module Acme.Types where

import           Data.ByteString       (ByteString)
import qualified Data.ByteString.Char8 as C
import Data.ByteString.Internal        (c2w)
import Data.Data                       (Data, Typeable)
import Text.PrettyPrint.HughesPJ       (Doc, ($$), (<+>), ($+$), (<>), char, nest, text, vcat)
import Data.Word                       (Word8)
import qualified Data.Map.Lazy         as M
import Data.Char                       (isSpace)

------------------------------------------------------------------------------
-- SIPVersion
------------------------------------------------------------------------------
data SIPVersion = SIP20


ppSIPVersion :: Doc
ppSIPVersion = text "SIP/2.0"

------------------------------------------------------------------------------
-- Method
------------------------------------------------------------------------------

data Method
    = INVITE
    | ACK
    | BYE
    | CANCEL
    | REGISTER
    | OPTIONS
    | INFO -- [RFC2976]
    | EXTENSION ByteString
    deriving (Eq, Ord, Read, Show, Data, Typeable)

ppMethod :: Method -> Doc
ppMethod INVITE   = text "INVITE"
ppMethod ACK      = text "ACK"
ppMethod BYE      = text "BYE"
ppMethod CANCEL   = text "CANCEL"
ppMethod REGISTER = text "REGISTER"
ppMethod OPTIONS  = text "OPTIONS"
ppMethod INFO     = text "INFO"
ppMethod (EXTENSION ext) = text (C.unpack ext)

------------------------------------------------------------------------------
-- Request
------------------------------------------------------------------------------

type Headers = M.Map ByteString ByteString

data Request = Request
    { rqMethod      :: !Method
    , rqURIbs       :: !ByteString
    , rqSIPVersion  :: !SIPVersion
    , rqToHeader    :: !ByteString
    , rqCallID      :: !ByteString
    , rqCseq        :: !ByteString
    , rqHeaders     :: !Headers
    , rqSecure      :: !Bool
    , rqBody        :: !ByteString
    }
    deriving Typeable

instance Show Request where
    show = show . ppRequest

ppRequest :: Request -> Doc
ppRequest Request{..} =
    text "Request {"  $+$
      nest 2 (
        vcat [ field "  rqMethod"      (ppMethod    rqMethod)
             , field ", rqURIbs"       (bytestring  rqURIbs)
             , field ", rqSIPVersion"  ppSIPVersion
             , field ", rqToHeader"    (bytestring  rqToHeader)
             , field ", rqCallID"      (bytestring  rqCallID)
             , field ", rqCseq"        (bytestring  rqCseq)
             , field ", rqHeaders"     (ppHeaders   rqHeaders)
             , field ", rqSecure"      (text $ show rqSecure)
             ])        $+$
    text "}"

------------------------------------------------------------------------------
-- Response
------------------------------------------------------------------------------

data Response
    = PongResponse               -- ^ return PONG in the request body
    | ByteStringResponse
      { rsCode    :: !Int
      , rsStatus  :: !ByteString
      , rsHeaders :: ![(ByteString, ByteString)]
      , rsBody    :: !ByteString
      }

ppResponse :: Response -> Doc
ppResponse PongResponse = text "PongResponse"
ppResponse ByteStringResponse{..} =
    text "Response {"  $+$
      nest 2 (vcat [ field "rsCode"    (text $ show rsCode)
                   , field "rsStatus"  (bytestring rsStatus)
                   , field "rsHeaders" (ppHeaders (M.fromList rsHeaders))
                   , field "rsBody"    (text $ show rsBody)
                   ])  $+$
    text "}"

instance Show Response where
    show = show . ppResponse

------------------------------------------------------------------------------
-- pretty-print helpers
------------------------------------------------------------------------------

-- | render a 'ByteString' to 'Doc'
bytestring :: ByteString -> Doc
bytestring = text . C.unpack

-- | render, field = value
field :: String -- ^ field name
      -> Doc    -- ^ field value
      -> Doc
field name doc = text name $$ nest 15 (char '=' <+> doc)

-- | pretty-print a SIP header
ppHeader :: (ByteString, ByteString) -> Doc
ppHeader (fieldName, fieldValue) =
    bytestring fieldName <> text ": " <> bytestring fieldValue

-- | pretty-print SIP headers
ppHeaders :: Headers
          -> Doc
ppHeaders headers = vcat $ map ppHeader $ M.toList headers

lstrip :: ByteString -> ByteString
lstrip = C.dropWhile isSpace

------------------------------------------------------------------------------
-- 'Word8' constants for popular characters
------------------------------------------------------------------------------

colon, cr, nl, space :: Word8
colon = c2w ':'
cr    = c2w '\r'
nl    = c2w '\n'
space = c2w ' '

