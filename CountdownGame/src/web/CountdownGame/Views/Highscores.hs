{-# LANGUAGE OverloadedStrings #-}
module CountdownGame.Views.Highscores where

import Data.Text (pack)

import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Control.Monad (forM, forM_)

import CountdownGame.Spiel (State)
import Countdown.Game as G

render :: [(G.Player, Int)] -> Html
render scores = html $ do
  script ! A.src "jquery.js" $ text ""
  body $ do

    H.header $ do
      H.h1 "CountDown"

    H.section $ do
      H.div ! A.id "right" $ do
        H.table $ do
          H.thead $ do
            H.th "Spieler"
            H.th "Punkte"
          H.tbody $ forM_ scores (\ (pl, sc) ->
            H.tr $ do
              H.td (text $ G.nickName pl)
              H.td (text . pack . show $ sc))

    H.footer "Developer Open Space 2015"
