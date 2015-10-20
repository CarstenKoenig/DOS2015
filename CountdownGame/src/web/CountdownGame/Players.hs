{-# LANGUAGE OverloadedStrings #-}

module CountdownGame.Players
       ( isRegistered
       , registeredPlayer
       , registerPlayer
       )where

import Control.Monad.IO.Class(liftIO)

import Data.Char (toLower)
import Data.List(isPrefixOf)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe(isJust)

import Data.IORef (IORef(..), newIORef, atomicModifyIORef')

import Data.Text (Text, unpack)

import Web.Scotty
import qualified Web.Scotty as S

import Countdown.Game (Player(Player), nickName, playerId)

import CountdownGame.Spiel (State (connectionPool))
import CountdownGame.Database as Db
import qualified CountdownGame.Cookies as Cookies
               
registeredPlayer :: State -> ActionM (Maybe Player)
registeredPlayer state = do
  cookie <- Cookies.getPlayerCookie
  case cookie of
    Nothing -> return Nothing
    Just c  -> liftIO $ Db.checkPlayer (Cookies.playerId c) (Cookies.nickName c) (connectionPool state)

registerPlayer :: Text -> State -> ActionM Player
registerPlayer nick state = do
  cookie <- Cookies.getPlayerCookie
  player <- liftIO $ case cookie of
    Nothing -> Db.addPlayer nick (connectionPool state)
    Just c  -> Db.updatePlayer (Cookies.playerId c) nick (connectionPool state)
  let cookie = Cookies.PlayerCookie (nickName player) (playerId player)
  Cookies.setPlayerCookie cookie
  return player

isRegistered :: State -> ActionM Bool
isRegistered state = do
  player <- registeredPlayer state
  return $ isJust player

fromCookie :: Cookies.PlayerCookie -> Player
fromCookie cookie = Player (Cookies.nickName cookie) (Cookies.playerId cookie)
