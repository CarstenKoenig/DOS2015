{-# LANGUAGE OverloadedStrings #-}
{-#LANGUAGE DeriveGeneric #-}

module CountdownGame.Spiel
       ( State (..)
       , initState
       , takeSnapshot
       , versuchHinzufuegen
       )where

import GHC.Generics (Generic)

import Data.Aeson (ToJSON)
import qualified Data.Map.Strict as M
import Data.Text (Text, pack)
import Data.Time.Clock (UTCTime, NominalDiffTime, getCurrentTime, diffUTCTime)
import Database.Persist.Sql (ConnectionPool)

import Countdown.Game (Attempt, Challange, Player, PlayerId)
import qualified Countdown.Game as G


import CountdownGame.References
import CountdownGame.Spiel.Phasen (Phasen (..), SpielParameter (..), Ergebnisse, startGameLoop, versuchHinzufuegen)
import CountdownGame.Database (setPlayerScore, createPool)

data State =
  State
  { aktuellePhase  :: Reference Phasen
  , connectionPool :: ConnectionPool
  }

initState :: Int -> IO State
initState nrPoolCons = do
  phases <- startGameLoop (SpielParameter 30 60)
  pool <- createPool nrPoolCons
  return $ State phases pool

data Snapshot =
  Snapshot
  { goal         :: Maybe Int
  , availableNrs :: [Int]
  , isWaiting    :: Bool
  , isRunning    :: Bool
  , secondsLeft  :: Int
  , scoreBoard   :: Ergebnisse
  } deriving (Generic, Show)

instance ToJSON Snapshot

takeSnapshot :: State -> IO Snapshot
takeSnapshot state = do
  now <- getCurrentTime
  readRef (readPhase now) $ aktuellePhase state
  where
    readPhase _ Start = Snapshot Nothing [] False False 0 []
    readPhase n (WartePhase t ergs _) =
      Snapshot Nothing [] True False (bisT n t) ergs
    readPhase n (RundePhase t ch _ _ _) =
      Snapshot (Just $ G.targetNumber ch) (G.availableNumbers ch) False True (bisT n t) []
    bisT n t = max 0 . truncate $ t `diffUTCTime` n
