{-# LANGUAGE OverloadedStrings #-}
module CountdownGame.Views.Register where

import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

render :: Html
render = html $ body $ do

    H.header $ do
      H.h1 "CountDown"

    H.section $ do
      H.div ! A.id "left" $
        H.h1 ! A.id "timer" $ "--"

      H.div ! A.id "right" $ do
        H.h2 "Welchen Namen möchtest Du benutzen?"
        H.div H.! A.id "versuch" $ do
          H.form H.! method "post" H.! action "/register" $ do
            H.input H.! type_ "text" H.! name "nickName"
            H.input H.! type_ "submit" H.! value "Anmelden"

    H.footer "Developer Open Space 2015"

