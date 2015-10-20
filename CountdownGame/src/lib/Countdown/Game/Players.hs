{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Countdown.Game.Players
       ( PlayerId
       , Player (..)
       , PlayersMap
       , list
       , Countdown.Game.Players.lookup
       , insert
       , update
       )where

import GHC.Generics (Generic)

import Data.Aeson (ToJSON)
import Data.Text (Text)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

type PlayerId = Integer

data Player =
  Player
  { nickName :: Text
  , playerId :: PlayerId
  } deriving (Generic, Show)

instance ToJSON Player

type PlayersMap = Map PlayerId Player

list :: PlayersMap -> [Player]
list = map snd . M.toAscList

lookup :: PlayerId -> PlayersMap -> Maybe Player
lookup = M.lookup

insert :: Text -> PlayersMap -> (PlayersMap, Player)
insert nick m =
  let id = fromIntegral $ M.size m
  in update id nick m

update :: PlayerId -> Text -> PlayersMap -> (PlayersMap, Player)
update id nick m =
  let p = Player nick id
  in (M.alter (const $ Just p) id m, p)
