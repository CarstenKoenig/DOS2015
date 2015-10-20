{-# LANGUAGE OverloadedStrings #-}
module CountdownGame.Views.Admin where

import Data.Text (pack)

import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Control.Monad (forM_)

import CountdownGame.Spiel (State)

render :: State -> Html
render _ = html $ do
  script ! A.src "jquery.js" $ text ""
  script ! A.src "jquery.timer.js" $ text ""
  script ! A.src "knockout.js" $ text ""
  script ! A.src "admin.js" $ text ""
  body $ do

    H.header $ do
      H.h1 "CountDown"

    H.section $ do
      H.div ! A.id "left" $
        H.h1 ! A.id "timer" ! dataBind "text: secondsLeft" $ "--"

      H.div ! A.id "right" ! dataBind "visible: isRunning" $ do
        p $ do
          H.span "Ziel: "
          H.span ! dataBind "text: goal" $ ""
        p $ do
          H.span "Zahlen: "
          H.span ! dataBind "text: numbers" $ ""

      H.div ! A.id "right" ! dataBind "visible: isWaiting" $ do
        H.table $ do
          H.thead $ do
            H.th "Spieler"
            H.th "Punkte"
            H.th "Ergebnis"
            H.th "Differenz"
            H.th "Formel"
          H.tbody ! dataBind "foreach: scores" $
            H.tr $ do
              H.td ! dataBind "text: name" $ ""
              H.td ! dataBind "text: score" $ ""
              H.td ! dataBind "text: value" $ ""
              H.td ! dataBind "text: difference" $ ""
              H.td ! dataBind "text: formula" $ ""

    H.footer "Developer Open Space 2015"
      
dataBind :: AttributeValue -> Attribute
dataBind = dataAttribute "bind"
