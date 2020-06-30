{-# LANGUAGE DeriveGeneric #-}

module Web.Types where

import Data.Aeson
import qualified Data.Map as M
import Data.Map (Map)
import qualified Data.Set as S
import Data.Set (Set)
import Data.Text
import Data.Time.Clock
import Data.UUID
import Boggle.Board
import GHC.Generics (Generic)

import Control.Concurrent.STM.TVar

type UserId = UUID
type Submission = String
type SubmissionMap = Map UserId (Set Submission)
type ScoreMap = Map Submission Int

type LobbyCode = String
type LobbyMap = Map LobbyCode Lobby
type UserMap = Map UserId LobbyCode

data ServerState = ServerState
  { userMap :: TVar UserMap,
    lobbyMap :: TVar LobbyMap
  }

data LobbyState
  = InLobby
  | StartingGame UTCTime
  | InGame UTCTime Board SubmissionMap
  deriving (Generic, Show)

data LobbySettings = LobbySettings
  { size :: Int,
    timeInSeconds :: Int
  }
  deriving (Generic, Show)

data Lobby = Lobby
  { host :: UserId,
    players :: [UserId],
    nicknames :: Map UserId Text,
    settings :: LobbySettings,
    lobbyState :: LobbyState,
    joinCode :: LobbyCode,
    previousRoundScores :: Maybe (SubmissionMap, ScoreMap)
  }
  deriving (Generic, Show)

instance ToJSON LobbyState
instance ToJSON LobbySettings
instance FromJSON LobbySettings
instance ToJSON Lobby

--- INIT ---
newLobby :: UUID -> Text -> LobbyCode -> Lobby
newLobby uuid nick lobbyCode =
  Lobby
    { host = uuid,
      players = [uuid],
      nicknames = M.singleton uuid nick,
      settings = newSettings,
      lobbyState = InLobby,
      joinCode = lobbyCode,
      previousRoundScores = Nothing
    }

newSettings :: LobbySettings
newSettings = LobbySettings {size = 5, timeInSeconds = 180}

--- CHANGE PLAYERS ---
addPlayer :: UUID -> Text -> Lobby -> Lobby
addPlayer uuid nick lobby =
  lobby
    { players = players lobby ++ [uuid],
      nicknames = M.insert uuid nick (nicknames lobby)
    }
