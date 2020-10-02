{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

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
import Web.Internal.FormUrlEncoded

type UserId = UUID
type Submission = String
type SubmissionMap = Map UserId (Set Submission)
type ScoreMap = Map Submission Int

type LobbyCode = String

type ServerState = TVar [Lobby]

data LastRound = LastRound
  { submissionMap' :: SubmissionMap,
    scoreMap' :: ScoreMap,
    board' :: Board,
    wordsNotInGrid' :: Set Submission
  }
  deriving (Generic, Show, Eq)

instance ToJSON LastRound

data LobbyState
  = InLobby'
  | StartingGame' UTCTime
  | InGame' UTCTime Board SubmissionMap
  deriving (Generic, Show, Eq)

data LobbySettings = LobbySettings
  { size :: Int,
    timeInSeconds :: Int
  }
  deriving (Generic, Show, Eq)

data Lobby = Lobby
  { host :: UserId,
    players :: [UserId],
    nicknames :: Map UserId Text,
    settings :: LobbySettings,
    lobbyState :: LobbyState,
    joinCode :: LobbyCode,
    lastRound :: Maybe LastRound
  }
  deriving (Generic, Show, Eq)

instance ToJSON LobbyState
instance ToJSON LobbySettings
instance ToJSON Lobby
instance FromJSON LobbySettings

instance FromForm LobbySettings

---- Redacted State ----
data RedactedLobbyState
  = InLobby
  | StartingGame UTCTime
  | InGame UTCTime Board (Set Submission)
  deriving (Generic)

instance ToJSON RedactedLobbyState

redactLobbyState :: UUID -> LobbyState -> RedactedLobbyState
redactLobbyState uuid (InGame' endTime board submissionMap) =
  InGame endTime board $ if uuid `elem` (M.keys submissionMap)
                            then (submissionMap M.! uuid)
                            else S.empty
redactLobbyState _ InLobby' = InLobby
redactLobbyState _ (StartingGame' t) = StartingGame t

data RedactedLastRound = RedactedLastRound
  { submissionMap :: Map Text (Set Submission),
    scoreMap :: ScoreMap,
    board :: Board,
    wordsNotInGrid :: Set Submission
  }
  deriving (Generic, Show, Eq)

instance ToJSON RedactedLastRound

redactLastRound :: Map UserId Text -> LastRound -> RedactedLastRound
redactLastRound nicknames LastRound{..} =
  RedactedLastRound {submissionMap = M.mapKeys (nicknames M.!) submissionMap',
                     scoreMap = scoreMap',
                     board = board',
                     wordsNotInGrid = wordsNotInGrid'
                    }

data RedactedLobby = RedactedLobby
  { hostName :: Text,
    playerNames :: [Text],
    currentSettings :: LobbySettings,
    state :: RedactedLobbyState,
    lobbyCode :: LobbyCode,
    lastRoundScores :: Maybe RedactedLastRound
  }
  deriving (Generic)

instance ToJSON RedactedLobby

redactLobby :: UUID -> Lobby -> RedactedLobby
redactLobby uuid Lobby{..} =
  RedactedLobby {hostName = nicknames M.! host,
                 playerNames = fmap (nicknames M.!) players,
                 currentSettings = settings,
                 state = redactLobbyState uuid lobbyState,
                 lobbyCode = joinCode,
                 lastRoundScores = fmap (redactLastRound nicknames) lastRound
                }

---- Request Types ----
data NameRequest = NameRequest {name :: Text} deriving (Generic)
instance FromForm NameRequest

data JoinRequest = JoinRequest {jrName :: Text, jrLobbyCode :: LobbyCode}
instance FromForm JoinRequest where
  fromForm f = JoinRequest
    <$> parseUnique "name" f
    <*> parseUnique "code" f

data UUIDRequest = UUIDRequest {uuid :: UUID} deriving (Generic)
instance FromForm UUIDRequest

data SettingsRequest = SettingsRequest {srUUID :: UUID, srSize :: Int, srTimeInSeconds :: Int}
instance FromForm SettingsRequest where
  fromForm f = SettingsRequest
    <$> parseUnique "uuid" f
    <*> parseUnique "size" f
    <*> parseUnique "timeInSeconds" f

data WordRequest = WordRequest {wrUUID :: UUID, word :: String}
instance FromForm WordRequest where
  fromForm f = WordRequest
    <$> parseUnique "uuid" f
    <*> parseUnique "word" f
