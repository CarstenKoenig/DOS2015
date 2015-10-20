{-# LANGUAGE OverloadedStrings    #-}

module CountdownGame.Database
       ( createPool
       , initializeDatabase
       , addPlayer
       , updatePlayer
       , getPlayer
       , checkPlayer
       , getPlayers
       , getPlayersMap
       , insertChallange
       , setPlayerScore
       , getHighscores
       ) where

import Control.Monad (forM)
import Control.Monad.Logger (runNoLoggingT)
import Control.Monad.Trans.Resource (runResourceT)

import Data.Function (on)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.List (groupBy, sortBy, foldl')
import qualified Data.Map.Strict as M
import Database.Persist
import Database.Persist.Sql (ConnectionPool, SqlPersistT, insert, fromSqlKey, toSqlKey, runSqlPool, rawSql)
import Database.Persist.Sqlite (runSqlite, createSqlitePool, runMigration)
import Database.Persist.TH

import CountdownGame.Database.Models
import qualified Countdown.Game as G
import qualified Countdown.Game.Players as P

createPool :: Int -> IO ConnectionPool
createPool n = runResourceT . runNoLoggingT  $ createSqlitePool connectionString n

runInPool :: ConnectionPool ->  SqlPersistT IO a -> IO a
runInPool = flip runSqlPool

connectionString :: Text
connectionString = "countdown.db"

initializeDatabase :: ConnectionPool -> IO ()
initializeDatabase pool = runInPool pool $ do runMigration migrateAll

addPlayer :: Text -> ConnectionPool -> IO P.Player
addPlayer nick pool = runInPool pool $ addPlayer' nick

getPlayer :: P.PlayerId -> ConnectionPool -> IO (Maybe P.Player)
getPlayer id pool = runInPool pool $ getPlayer' id

getPlayer' id = do
  pl <- get (toSqlKey $ fromIntegral id)
  return $ case pl of
    Nothing -> Nothing
    Just p  -> Just $ P.Player (playerNickname p) id

checkPlayer :: P.PlayerId -> Text -> ConnectionPool -> IO (Maybe P.Player)
checkPlayer id nick pool = runInPool pool $ do
  pl <- get (toSqlKey $ fromIntegral id)
  return $ case pl of
    Nothing -> Nothing
    Just p  ->
      let nick' = playerNickname p
      in if nick' == nick
         then Just $ P.Player (playerNickname p) id
         else Nothing

updatePlayer :: P.PlayerId -> Text -> ConnectionPool -> IO P.Player
updatePlayer id nick pool = runInPool pool $ do
  let pId = toSqlKey $ fromIntegral id
  pl <- get pId
  case pl of
    Nothing -> addPlayer' nick
    Just p  -> do
      replace pId $ Player nick
      return $ P.Player nick id

getPlayersMap :: ConnectionPool -> IO P.PlayersMap
getPlayersMap pool = do
  ps <- map (\ p -> (P.playerId p, p)) <$> getPlayers pool
  return $ M.fromList ps

getPlayers :: ConnectionPool -> IO [P.Player]
getPlayers pool = runInPool pool $ do
  pls <- selectList [] []
  return $ map fromEntity pls
  where
    fromEntity entity =
       P.Player (playerNickname $ entityVal entity) (fromIntegral . fromSqlKey $ entityKey entity)

addPlayer' nick = do
  key <- insert $ Player nick
  let id = fromIntegral $ fromSqlKey key
  return $ P.Player nick id

insertChallange :: G.Challange -> IO Int64
insertChallange ch = runSqlite connectionString $ do
  key <- insert $ Challange (G.targetNumber ch) (G.availableNumbers ch)
  return $ fromSqlKey key

setPlayerScore :: Int64 -> P.PlayerId -> Int -> IO ()
setPlayerScore chId pId score = runSqlite connectionString $ do
  let id = toSqlKey chId
      pid = toSqlKey $ fromIntegral pId
  sc <- getBy $ Index pid id
  case sc of
    Nothing -> insert_ $ Score score pid id
    Just e  -> update (entityKey e) [ ScoreScore =. score ]

getScores = do
  pls <- selectList [] [Asc ScorePlayer]
  return $ map fromEntity pls
  where
    fromEntity entity =
      ((fromIntegral . fromSqlKey . scorePlayer $ entityVal entity), (scoreScore $ entityVal entity))
              
getHighscores :: ConnectionPool -> IO [(P.Player, Int)]
getHighscores pool = runInPool pool $ do
  grps <- groupBy ((==) `on` fst) <$> getScores
  let scrs = map (foldl' (\ (_,sc) (pid,c) -> (pid, sc+c)) (0,0)) grps
      srts = sortBy (compare `on` (negate . snd)) scrs
  forM srts (\ (pid, scs) -> do
                 (Just p) <- getPlayer' pid
                 return (p, scs))
  
