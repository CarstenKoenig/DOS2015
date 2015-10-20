{-# LANGUAGE OverloadedStrings #-}

module CountdownGame.Cookies
       ( PlayerCookie (..)
       , getPlayerCookie
       , setPlayerCookie
       ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy.Encoding as LT
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Blaze.ByteString.Builder as B

import Web.Scotty
import Web.Cookie

import qualified Web.Scotty as S

data PlayerCookie = PlayerCookie { nickName :: Text, playerId :: Integer }

getPlayerCookie :: ActionM (Maybe PlayerCookie)
getPlayerCookie = do
  cs <- getCookies
  return $ cs >>= readPlayerValues

readPlayerValues :: CookiesText -> Maybe PlayerCookie
readPlayerValues cs = do    
    name <- lookup "nickName" cs
    id   <- lookup "playerId" cs
    return $ PlayerCookie name (read $ T.unpack id)

setPlayerCookie :: PlayerCookie -> ActionM ()
setPlayerCookie pc = do
  setCookie "playerId" (T.encodeUtf8 . T.pack . show $ playerId pc)
  setCookie "nickName" (T.encodeUtf8 $ nickName pc)
  
makeCookie :: BS.ByteString -> BS.ByteString -> SetCookie
makeCookie n v = def { setCookieName = n, setCookieValue = v, setCookieMaxAge = Just $ 60 * 60 * 24 * 365 }

renderSetCookie' :: SetCookie -> Text
renderSetCookie' = T.decodeUtf8 . BSL.toStrict . B.toLazyByteString . renderSetCookie

setCookie :: BS.ByteString -> BS.ByteString -> ActionM ()
setCookie n v = addHeader "Set-Cookie" (LT.fromStrict . renderSetCookie' $ makeCookie n v)

getCookies :: ActionM (Maybe CookiesText)
getCookies =
    fmap (parseCookiesText . lazyToStrict . LT.encodeUtf8) <$> header "Cookie"
    where
        lazyToStrict = BS.concat . BSL.toChunks
