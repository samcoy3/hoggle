{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Web.Server where

import Web.API
import Servant
import Servant.API
import Servant.API.Verbs
import Servant.Server
import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.STM.TVar
  ( TVar,
    newTVar,
    readTVar,
    writeTVar,
    modifyTVar,
  )
import Control.Exception (bracket)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.STM (atomically)
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Data.Aeson (FromJSON, ToJSON)
import Data.List
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Map as M
import Data.Map (Map)
import Data.UUID
import Data.UUID.V4
import GHC.Generics (Generic)

import Web.API
import Web.Types

lobbyServer :: ServerT LobbyAPI AppM
lobbyServer =
  newLobby
    :<|> joinLobby
    :<|> lobbyInfo
    :<|> lobbySettings
    -- :<|> startGame
    -- :<|> leaveLobby
  where
    newLobby :: NameRequest -> AppM UUID
    newLobby (NameRequest nick) = do
      lobbies <- ask
      newUUID <- liftIO nextRandom
      lobbyCode <- liftIO randomLobbyCode
      let lobby = makeNewLobby newUUID nick lobbyCode
      liftIO . atomically . modifyTVar lobbies $ (:) lobby
      liftIO $ (atomically $ readTVar lobbies) >>= putStrLn . show
      return newUUID

    joinLobby :: JoinRequest -> AppM UUID
    joinLobby (JoinRequest nick code) = do
      lobbies <- ask
      newUUID <- liftIO nextRandom
      foundLobby <- liftIO . findLobbyByCode code $ lobbies
      case foundLobby of
        Nothing -> throwError (err404 {errBody = "Invalid lobby code."})
        Just l ->
          if nick `elem` (M.elems . nicknames $ l)
            then throwError (err403 {errBody = "Cannot have duplicate nicknames"})
            else do
              liftIO . atomically . modifyTVar lobbies $ replace l
                ( l
                    { players = (players l) ++ [newUUID],
                      nicknames = M.insert newUUID nick (nicknames l)
                    }
                )
              liftIO $ (atomically $ readTVar lobbies) >>= putStrLn . show
              return newUUID

    lobbyInfo :: UUIDRequest -> AppM RedactedLobby
    lobbyInfo (UUIDRequest uuid) = do
      foundLobby <- ask >>= liftIO . findLobbyByUUID uuid
      case foundLobby of
        Nothing -> throwError (err404 {errBody = "Invalid UUID."})
        Just l -> return . redactLobby uuid $ l

    lobbySettings :: SettingsRequest -> AppM NoContent
    lobbySettings (SettingsRequest{..}) = do
      lobbies <- ask
      foundLobby <- liftIO . findLobbyByUUID srUUID $ lobbies
      case foundLobby of
        Nothing -> throwError (err404 {errBody = "Invalid UUID."})
        Just l -> if srUUID /= (host l)
          then throwError (err403 {errBody = "Only the host may change the settings."})
          else do
            liftIO . atomically . modifyTVar lobbies $ replace l
              ( l
                { settings = LobbySettings {size = srSize, timeInSeconds = srTimeInSeconds}
                })
            liftIO $ (atomically $ readTVar lobbies) >>= putStrLn . show
            return NoContent

    startGame = undefined
    leaveLobby = undefined

gameServer :: ServerT GameAPI AppM
gameServer = sendWord :<|> removeWord
  where
    sendWord = undefined
    removeWord = undefined

server :: ServerT ServerAPI AppM
server = lobbyServer -- :<|> gameServer

nt :: ServerState -> AppM a -> Handler a
nt = flip runReaderT

app :: ServerState -> Application
app s = serve api $ hoistServer api (nt s) server

--- LOBBY SERVER ---
makeNewLobby :: UUID -> Text -> LobbyCode -> Lobby
makeNewLobby uuid nick lobbyCode =
  Lobby
    { host = uuid,
      players = [uuid],
      nicknames = M.singleton uuid nick,
      settings = newSettings,
      lobbyState = InLobby',
      joinCode = lobbyCode,
      previousRoundScores = Nothing
    }

newSettings :: LobbySettings
newSettings = LobbySettings {size = 5, timeInSeconds = 180}

randomLobbyCode :: IO LobbyCode
randomLobbyCode = return "AAAA"

addPlayer :: UUID -> Text -> Lobby -> Lobby
addPlayer uuid nick lobby =
  lobby
    { players = players lobby ++ [uuid],
      nicknames = M.insert uuid nick (nicknames lobby)
    }

--- UTIL ---
findLobbyByCode :: LobbyCode -> TVar [Lobby] -> IO (Maybe Lobby)
findLobbyByCode code lobbies' = do
  lobbies <- atomically $ readTVar lobbies'
  let validLobbies = filter (\lobby -> code == joinCode lobby) lobbies
  if length validLobbies == 1
    then return . Just $ head validLobbies
    else return Nothing

findLobbyByUUID :: UUID -> TVar [Lobby] -> IO (Maybe Lobby)
findLobbyByUUID uuid lobbies' = do
  lobbies <- atomically $ readTVar lobbies'
  let validLobbies = filter (\lobby -> uuid `elem` (players lobby)) lobbies
  if length validLobbies == 1
    then return . Just $ head validLobbies
    else return Nothing

replace :: (Eq a) => a -> a -> [a] -> [a]
replace f r xs =
  fmap (\x -> if x == f then r else x) xs
