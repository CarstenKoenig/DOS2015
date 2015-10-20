{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module CountdownGame.State.Definitions
       ( SpielParameter (..)
       , Phasen (..)
       , istWartend
       , istInRunde
       , nochZuWartendeSekunden
       , ergebnisListe
       , zielZahl
       , verfuegbareZahlen
       , Versuche
       , Ergebnis (..), Ergebnisse, berechneErgebnisse
       , State (..)
       )where

import GHC.Generics (Generic)

import Control.Concurrent.Async (Async)

import Data.Aeson (ToJSON)
import Data.Function (on)
import Data.Int (Int64)
import Data.List (sortBy)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import Data.Time.Clock (UTCTime, NominalDiffTime, getCurrentTime, diffUTCTime)
import Database.Persist.Sql (ConnectionPool)

import Countdown.Game (Attempt, AttemptsMap, Challange, Player, PlayersMap)
import qualified Countdown.Game as G

import CountdownGame.References

data State =
  State
  { aktuellePhase  :: Reference Phasen
  , connectionPool :: ConnectionPool
  }

istWartend :: State -> IO Bool
istWartend = readRef warted . aktuellePhase
  where warted (WartePhase _ _ _) = True
        warted _                  = False

istInRunde :: State -> IO Bool
istInRunde = readRef inRunde . aktuellePhase
  where inRunde (RundePhase _ _ _ _ _) = True
        inRunde _                      = False

nochZuWartendeSekunden :: State -> IO Int
nochZuWartendeSekunden state = do
  now <- getCurrentTime
  readRef (seks now) $ aktuellePhase state
  where seks n (WartePhase t _ _)     = bisT n t
        seks n (RundePhase t _ _ _ _) = bisT n t
        seks _ _                      = 0
        bisT n t = truncate $ min 0 $ t `diffUTCTime` n

ergebnisListe :: State -> IO Ergebnisse
ergebnisListe = readRef ergs . aktuellePhase
  where ergs (WartePhase _ e _) = e
        ergs _                  = []

zielZahl :: State -> IO (Maybe Int)
zielZahl = readRef zz . aktuellePhase
  where zz (RundePhase _ c _ _ _) = Just $ G.targetNumber c
        zz _                      = Nothing

verfuegbareZahlen :: State -> IO [Int]
verfuegbareZahlen = readRef aNrs . aktuellePhase
  where aNrs (RundePhase _ c _ _ _) = G.availableNumbers c
        aNrs _                      = []


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

type Versuche   = Reference AttemptsMap

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
