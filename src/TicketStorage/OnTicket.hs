{-# LANGUAGE DefaultSignatures, DeriveGeneric #-}

module TicketStorage.OnTicket (onTicketStore) where

import TicketStorage.Base
import Common
import Model.MinUser
import Crypto.Hash.Algorithms (SHA1)
import Crypto.Hash (Digest)
import Crypto.MAC.HMAC (hmacGetDigest, hmac)
import Data.Serialize as Serial
import GHC.Generics
import Data.ByteString.Base64.URL as B64
import Data.ByteString.Char8 as C8
import Data.Text as T
import Data.Text.Encoding
import Data.ByteArray as BA
import Data.Word

data BMUser = BMUser {
    ouid :: ByteString,
    firstname :: ByteString,
    lastname :: ByteString
} deriving (Generic)

instance Serialize BMUser

bmToM :: BMUser -> MinimalUser
bmToM (BMUser i fn ln) = MinimalUser (decodeUtf8 i) (decodeUtf8 fn) (decodeUtf8 ln)

mToBm :: MinimalUser -> BMUser
mToBm (MinimalUser i fn ln) = BMUser (encodeUtf8 i) (encodeUtf8 fn) (encodeUtf8 ln) 

data Ticket = Ticket {
    user :: BMUser,
    hash :: [Word8]
} deriving (Generic)

instance Serialize Ticket

macSecret :: ByteString 
macSecret = "HARDCODEDsecret"

userMac :: BMUser -> [Word8]
userMac u = BA.unpack digest
        where digest = hmacGetDigest . hmac macSecret . Serial.encode $ u :: Digest SHA1

userToTicket :: MinimalUser -> IO Text
userToTicket u = return . T.pack . C8.unpack . B64.encode . Serial.encode $ t
    where t = Ticket (mToBm u) (userMac . mToBm $ u)

ticketToUser :: Text -> IO (Maybe MinimalUser)
ticketToUser td = 
    case t of
        Right u | userMac (user u) == hash u -> return . Just . bmToM . user $ u
        _ -> return Nothing
    where t = (B64.decode . C8.pack . T.unpack $ td) >>= Serial.decode

onTicketStore :: TicketStorageProvider
onTicketStore = return $ UserTicketStorageContext  userToTicket ticketToUser