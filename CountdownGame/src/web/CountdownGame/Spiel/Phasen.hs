{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}


module CountdownGame.Spiel.Phasen
       ( SpielParameter (..)
       , Phasen (..)
       , Versuche
       , Ergebnisse, Ergebnis (..)
       , startGameLoop
       , versuchHinzufuegen
       )where

import GHC.Generics (Generic)

import Control.Concurrent.Async (Async, async, wait, poll, asyncThreadId, waitBoth)
import Control.Concurrent (forkIO, threadDelay, ThreadId)

import Data.Aeson (ToJSON)
import Data.Function (on)
import Data.Int (Int64)
import Data.IORef (IORef(..), newIORef, readIORef, atomicModifyIORef')
import Data.List (sortBy)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import Data.Time (UTCTime, NominalDiffTime, getCurrentTime, addUTCTime)

import CountdownGame.References
import CountdownGame.Database (insertChallange)

import Countdown.Game (Attempt, AttemptsMap, Challange)
import qualified Countdown.Game as G

-- $doc
-- Es gibt zwei Zustände:
--
--   - warten auf den Begin der nächsten Runde
--   - die Runde läuft
--
-- Beide Zustände laufen nur eine bestimmte Zeitland und
-- wechseln dann in den jeweils anderen Zustand
--
-- Während des Wartens wird zusätzlich die Challange für die nächste
-- Runde berechnet und die Ergebnise der letzen Runde (falls vorhanden)
-- sind verfügbar.
-- Am Ende dieser Wartezeit ist ist die nächste Challange verfügbar
--
-- Während einer Runde ist die aktuelle Challange verfügbar und die
-- Spieler können ihre Attempts schicken (die gespeichert werden)
-- Am Ende dieser Phase sind die Ergebnise der Runde verfügbar

data SpielParameter =
  SpielParameter
  { warteZeit  :: NominalDiffTime
  , rundenZeit :: NominalDiffTime
  }

data Phasen
  = Start
  | WartePhase
    { startNaechsteRunde :: UTCTime
    , letzteErgebnisse   :: Ergebnisse
    , naechsteChallange  :: Async Challange }
  | RundePhase
    { endeRunde       :: UTCTime
    , aufgabe         :: Challange
    , spielerVersuche :: Versuche
    , databaseKey     :: Int64
    , ergebnisse      :: Async Ergebnisse }

startGameLoop :: SpielParameter -> IO (Reference Phasen)
startGameLoop params = do
  ref <- createRef Start
  _   <- forkIO $ gameLoop params [] ref
  return ref

type Versuche   = Reference AttemptsMap

versuchHinzufuegen :: Reference Phasen -> (Int64 -> G.PlayerId -> Int -> IO ()) -> G.Player -> Text -> IO (Maybe Attempt)
versuchHinzufuegen ref saveScore p f = do
  phase <- readRef id ref
  case phase of
    (RundePhase _ chal vers key _) -> do
      v <- modifyRef (G.attempt chal f p) vers
      saveScore key (G.playerId p) (G.score v)
      return $ Just v
    _ -> return Nothing


type Ergebnisse = [Ergebnis]
data Ergebnis =
  Ergebnis
  { name       :: Text
  , score      :: Int
  , value      :: Maybe Int
  , difference :: Maybe Int
  , formula    :: Text
  } deriving (Generic, Show)

instance ToJSON Ergebnis

berechneErgebnisse :: AttemptsMap -> Ergebnisse
berechneErgebnisse attMap =
  sortBy (compare `on` (negate . score)) scores
  where
    scores = map calcScore . M.toList $ attMap
    calcScore (_, att) =
      Ergebnis (G.nickName $ G.fromPlayer att) (G.score att) (G.value att) (G.difference att) (G.formula att)


gameLoop :: SpielParameter -> Ergebnisse ->  Reference Phasen -> IO ()
gameLoop params ltErg phase = do
  warten <- wartePhase params ltErg
  modifyRef (const (warten, ())) phase
  aufg <- wait . naechsteChallange $ warten
  runde <- rundenPhase params aufg
  modifyRef (const (runde, ())) phase
  erg <- wait . ergebnisse $ runde
  gameLoop params erg phase
  
wartePhase :: SpielParameter -> Ergebnisse -> IO Phasen
wartePhase params ltErg = do
  wartenBis <- (warteZeit params `addUTCTime`) <$> getCurrentTime
  start     <- async $ warteStart wartenBis
  return $ WartePhase wartenBis ltErg start

warteStart :: UTCTime -> IO Challange
warteStart startZeit  = do
  neue  <- async G.generateChallange
  warte <- async $ warteBis startZeit
  (chal, _) <- waitBoth neue warte
  return chal

rundenPhase :: SpielParameter -> Challange -> IO Phasen
rundenPhase params chal = do
  chId     <- insertChallange chal
  rundeBis <- (rundenZeit params `addUTCTime`) <$> getCurrentTime
  vers     <- createRef M.empty
  erg      <- async $ warteErgebnise rundeBis chal vers
  return $ RundePhase rundeBis chal vers chId erg

warteErgebnise :: UTCTime -> Challange -> Versuche -> IO Ergebnisse
warteErgebnise endeZeit aufg refVers = do
  warteBis endeZeit
  readRef berechneErgebnisse refVers

warteBis :: UTCTime -> IO ()
warteBis zeit = do
  now <- getCurrentTime
  if now < zeit then do
    threadDelay 250000
    warteBis zeit
  else return ()
