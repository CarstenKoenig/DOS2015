{-# LANGUAGE OverloadedStrings #-}
module CountdownGame.Views.Index where

import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

render :: Html
render = do
  html $ do
    body $ do
      h1 "COUNTdown"
      ul $ do
        li "just do it"
