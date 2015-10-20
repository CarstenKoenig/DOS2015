{-# LANGUAGE OverloadedStrings #-}

module CountdownGame.State
       ( State (..)
       , Round (..)
       , initState
       , setAttempt
       , takeSnapshot
       , initialCompletions
       , completions
       )where

import Data.Text (Text, pack)
import qualified Data.Map.Strict as M

import Countdown.Game (Attempt, attempt, Challange, Player, PlayerId, playerId, score, availableNumbers)
import qualified Countdown.Completion as CC

import CountdownGame.References
import CountdownGame.State.Definitions (State (..), Round (..))
import CountdownGame.State.Snapshots (takeSnapshot)
import CountdownGame.Database (setPlayerScore, createPool)

initialCompletions :: State -> IO [(Text, (CC.Input, CC.Expression))]
initialCompletions state = do
  rd <- readRef id $ currentRound state
  case rd of
    Nothing  -> return []
    Just rd' -> do
      let nrs  = availableNumbers $ challange rd'
      return $ map (\ (i,e) -> (pack (show i), (i,e))) $CC.start nrs

completions :: State -> (CC.Input, CC.Expression) -> IO [(Text, (CC.Input, CC.Expression))]
completions state inp = do
  rd <- readRef id $ currentRound state
  case rd of
    Nothing  -> return []
    Just rd' -> do
      let nrs  = availableNumbers $ challange rd'
      return $ map (\ (i,e) -> (pack (show i), (i,e))) $ CC.next nrs inp

setAttempt :: State -> Player -> Text -> IO (Maybe Attempt)
setAttempt state p txt = do
  rd <- readRef id $ currentRound state
  case rd of
    Just rd' -> do
      let ch  = challange rd'
          cId = databaseKey rd'
      at <- modifyRef (attempt ch txt p) $ playerAttempts state
      setPlayerScore cId (playerId p) (score at)
      return $ Just at
    Nothing  -> return Nothing

initState :: Int -> IO State
initState nrPoolCons = do
  emptyR <- emptyRoundState
  emptyP <- emptyChallange
  noGuesses <- createRef M.empty
  pool <- createPool nrPoolCons
  return $ State emptyR emptyP noGuesses pool

emptyRoundState :: IO (Reference (Maybe Round))
emptyRoundState = createRef Nothing

emptyChallange :: IO (Reference (Maybe Challange))
emptyChallange = createRef Nothing
