{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module CountdownGame.State.Snapshots
       ( Snapshot
       , takeSnapshot
       )where

import GHC.Generics (Generic)

import Data.Aeson (ToJSON)
import Data.Function (on)
import Data.Text (Text)
import Data.Time.Clock (UTCTime, getCurrentTime, diffUTCTime)
import Data.Maybe (isJust, fromMaybe, listToMaybe)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.List (sortBy)

import Countdown.Game (Attempt, AttemptsMap, Challange, Player, PlayersMap, PlayerId)
import qualified Countdown.Game as G

import CountdownGame.References
import CountdownGame.Database (getPlayersMap)
import CountdownGame.State.Definitions (State (..), Ergebnisse)
import qualified CountdownGame.State.Definitions as Def

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

takeSnapshot :: Bool -> State -> IO Snapshot
takeSnapshot isAdmin state = do
  zielZ <- Def.zielZahl state
  verfNrs <- Def.verfuegbareZahlen state
  wartet <- Def.istWartend state
  laueft <- Def.istInRunde state
  seks <- Def.nochZuWartendeSekunden state
  ergs <- Def.ergebnisListe state
  return $ Snapshot zielZ verfNrs wartet laueft seks ergs
