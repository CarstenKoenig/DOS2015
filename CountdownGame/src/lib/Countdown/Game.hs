{-# LANGUAGE OverloadedStrings #-}

module Countdown.Game
       ( PlayerId
       , Player (..)
       , PlayersMap (..)
       , Attempt (..)
       , AttemptsMap (..)
       , Challange (..)
       , attempt
       , attemptFromFormula
       , generateChallange
       )where

import Countdown.Game.Players
import Countdown.Game.Attempts
import Countdown.Game.Challanges
import Countdown.Game.Random
