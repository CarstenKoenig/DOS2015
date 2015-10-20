{-# LANGUAGE OverloadedStrings #-}

module CountdownGame.Actions
       ( play
       , register
       , postRegister
       , admin
       , highScores
       , getPlayers
       , getSnapshot
       , evalFormula
       , isLocalhost
       )where

import Debug.Trace (trace)

import Control.Monad.IO.Class(liftIO)

import Data.Char (toLower)
import Data.List(isPrefixOf)
import Data.Maybe(isNothing, fromJust, isJust)

import Data.Text.Lazy (Text, unpack)
import qualified Data.Text as T

import Web.Scotty
import qualified Web.Scotty as S

import Text.Blaze.Html (Html)
import Text.Blaze.Html.Renderer.Text (renderHtml)

import Text.Blaze.Html5 (link, (!))
import Text.Blaze.Html5.Attributes

import Network.Socket (SockAddr(..))
import Network.Wai (remoteHost)

import Countdown.Game (PlayerId)
import qualified Countdown.Game as G

import CountdownGame.Spiel (State)
import qualified CountdownGame.Spiel as Spiel
import qualified CountdownGame.Players as Players
import qualified CountdownGame.Database as Rep

import qualified CountdownGame.Views.Play as PlayView
import qualified CountdownGame.Views.Register as RegisterView
import qualified CountdownGame.Views.Admin as AdminView
import qualified CountdownGame.Views.Highscores as ScoresView

-- * controller actions

play :: State -> ActionM ()
play state = do
    player <- Players.registeredPlayer state
    if isNothing player
      then redirect "/register"
      else render $ PlayView.render (fromJust player)
              
register :: ActionM ()
register = render RegisterView.render

postRegister :: State -> ActionM ()
postRegister state = do
  name <- param "nickName"
  Players.registerPlayer name state
  redirect "/play"         

admin :: State -> ActionM ()
admin state = do
  localhost <- isLocalhost
  if not localhost
    then raise "you are not allowed"
    else render (AdminView.render state)

highScores :: State -> ActionM ()
highScores state = do
  scores <- liftIO $ Rep.getHighscores (Spiel.connectionPool state)
  render (ScoresView.render scores)
         

-- * Web-API Part

getPlayers :: State -> ActionM ()
getPlayers state = do
  players <- liftIO $ Rep.getPlayers (Spiel.connectionPool state)
  localhost <- isLocalhost
  if not localhost
    then raise "you are not allowed to do that"
    else json players

getSnapshot :: State -> ActionM ()
getSnapshot state = do
  isHost <- isLocalhost
  regPlayer <- isJust <$> Players.registeredPlayer state
  if not regPlayer
    then raise "you are not allowed to do that"
    else do
      snap <- liftIO $ Spiel.takeSnapshot state
      json snap

evalFormula :: State -> ActionM ()
evalFormula state = do
  formula <- param "formula"
  pl <- Players.registeredPlayer state
  case pl of
    Just pl' -> do
      att <- liftIO $ Spiel.versuchHinzufuegen (Spiel.aktuellePhase state) Rep.setPlayerScore pl' formula
      json att
    Nothing -> raise "kein Spieler registriert"

-- * Helpers

-- ** rendering
  
render :: Html -> ActionM ()
render html = do
  blaze $ do
    link ! rel "stylesheet" ! href "styles.css" ! type_ "text/css"
    html
  
blaze :: Html -> ActionM ()
blaze = S.html . renderHtml

-- ** access checks

isLocalhost :: ActionM Bool
isLocalhost = do
  remote <- remoteHost <$> request
  return $ hostnameIsLocal remote
  where hostnameIsLocal sockAdr =
          "127.0.0.1" `isPrefixOf` show sockAdr ||
          "localhost" `isPrefixOf` (map toLower . show) sockAdr
