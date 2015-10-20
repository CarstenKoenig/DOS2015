{-# LANGUAGE OverloadedStrings #-}
module CountdownGame.Views.Play where

import Data.Text (append)

import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Countdown.Game (Player, nickName)

render :: Player -> Html
render player = html $ do
  script ! A.src "jquery.js" $ text ""
  script ! A.src "jquery.timer.js" $ text ""
  script ! A.src "knockout.js" $ text ""
  script ! A.src "player.js" $ text ""
  body $ do
    H.div ! A.class_ "overlay" ! dataBind "visible: gotBusted" $ do
      H.div ! A.class_ "centerMessage" $ do
        H.div ! A.class_ "Error" ! dataBind "html: error" $ ""

    H.header $ do
      H.h1 "CountDown"

    H.section $ do
      H.div ! A.id "left" $
        H.h1 ! A.id "timer" ! dataBind "text: secondsLeft" $ "--"

      H.div ! A.id "right" ! dataBind "visible: isRunning" $ do
        H.div ! A.id "zahlen" $ do
          H.h3 "Aufgabe"
          H.ul ! dataBind "foreach: numbers" $ do
            H.li $ H.button ! dataBind "text: $data" $ ""
          H.h2 ! dataBind "text: goal" $ ""
        H.div ! A.id "versuch" $ do
          H.h3 "Dein Versuch"
          H.form $ do
            H.input ! A.type_ "text" ! A.autofocus "" ! dataBind "value: formula"
            H.input ! A.type_ "submit" ! dataBind "click: eval" ! A.value "OK"
        H.div ! A.id "ergebnis" $ do
          p $ do
            H.span "letztes Ergebnis: "
            H.span ! dataBind "text: result" $ ""
        H.div ! A.class_ "Error" ! dataBind "html: error" $ ""
            

    H.div ! A.id "right" ! dataBind "visible: isWaiting" $ do
      H.table $ do
        H.thead $ do
          H.th "Spieler"
          H.th "Punkte"
        H.tbody ! dataBind "foreach: scores" $
          H.tr $ do
            H.td ! dataBind "text: name" $ ""
            H.td ! dataBind "text: score" $ ""

    H.footer "Developer Open Space 2015"
      
dataBind :: AttributeValue -> Attribute
dataBind = dataAttribute "bind"

