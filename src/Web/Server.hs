{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Web.Server where

import Servant
import Control.Concurrent.STM.TVar
  ( TVar,
    modifyTVar,
    readTVarIO,
  )
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.STM (atomically)
import Control.Monad.Trans.Reader (ask, runReaderT)
import Data.Char
import Data.List
import Data.Text (Text)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.UUID
import Data.UUID.V4
import System.Random
import Data.Time.Clock

import Web.API
import Web.Types
import Boggle.Board
import Boggle.Scoring

lobbyServer :: ServerT LobbyAPI AppM
lobbyServer =
  newLobby
    :<|> joinLobby
    :<|> lobbyInfo
    :<|> lobbySettings
    :<|> startGame
    :<|> leaveLobby
  where

    newLobby :: NameRequest -> AppM UUID
    newLobby (NameRequest nick) = do
      lobbies <- ask
      newUUID <- liftIO nextRandom
      lobbyCode <- liftIO randomLobbyCode
      let lobby = makeNewLobby newUUID nick lobbyCode
      liftIO . atomically . modifyTVar lobbies $ (:) lobby
      liftIO $ (readTVarIO lobbies) >>= putStrLn . show
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
              liftIO . atomically . modifyTVar lobbies $
                replace
                  l
                  ( l
                      { players = (players l) ++ [newUUID],
                        nicknames = M.insert newUUID nick (nicknames l)
                      }
                  )
              liftIO $ (readTVarIO lobbies) >>= putStrLn . show
              return newUUID
    lobbyInfo :: UUIDRequest -> AppM RedactedLobby
    lobbyInfo (UUIDRequest uuid) = do
      lobbies <- ask
      foundLobby <- liftIO . findLobbyByUUID uuid $ lobbies
      case foundLobby of
        Nothing -> throwError (err404 {errBody = "Invalid UUID."})
        Just l' -> do
          l <- (liftIO . touchLobby) l'
          liftIO . atomically . modifyTVar lobbies $ replace l' l
          return . redactLobby uuid $ l
    lobbySettings :: SettingsRequest -> AppM NoContent
    lobbySettings (SettingsRequest {..}) = do
      lobbies <- ask
      foundLobby <- liftIO . findLobbyByUUID srUUID $ lobbies
      case foundLobby of
        Nothing -> throwError (err404 {errBody = "Invalid UUID."})
        Just l ->
          if srUUID /= (host l)
            then throwError (err403 {errBody = "Only the host may change the settings."})
            else do
              liftIO . atomically . modifyTVar lobbies $
                replace
                  l
                  ( l
                      { settings = LobbySettings {size = srSize, timeInSeconds = srTimeInSeconds}
                      }
                  )
              liftIO $ (readTVarIO lobbies) >>= putStrLn . show
              return NoContent
    startGame :: UUIDRequest -> AppM NoContent
    startGame (UUIDRequest uuid) = do
      lobbies <- ask
      foundLobby <- liftIO . findLobbyByUUID uuid $ lobbies
      case foundLobby of
        Nothing -> throwError (err404 {errBody = "Invalid UUID."})
        Just l -> if uuid /= host l
            then throwError (err403 {errBody = "Only the host may start a game."})
            else if lobbyState l /= InLobby'
                   then throwError (err403 {errBody = "Can't request to start a game when a game is in progress or about to start."})
                   else do
          timeToStartGame <- liftIO (getCurrentTime >>= return . addUTCTime 12)
          liftIO . atomically . modifyTVar lobbies $
            replace l
            (l {
                lobbyState = StartingGame' timeToStartGame
               })
          liftIO $ (readTVarIO lobbies) >>= putStrLn . show
          return NoContent
    leaveLobby :: UUIDRequest -> AppM NoContent
    leaveLobby (UUIDRequest uuid) = do
      lobbies <- ask
      foundLobby <- liftIO . findLobbyByUUID uuid $ lobbies
      case foundLobby of
        Nothing -> throwError (err404 {errBody = "Invalid UUID."})
        Just l -> (do
            ( liftIO . atomically . modifyTVar lobbies $
                if (length . players $ l) == 1
                  then delete l
                  else
                    replace
                      l
                      ( l
                          { host = if uuid == host l then (players l) !! 1 else host l,
                            players = delete uuid $ players l,
                            nicknames = M.delete uuid $ nicknames l,
                            previousRoundScores = case (previousRoundScores l) of
                              Nothing -> Nothing
                              Just (subMap, scoreMap) -> Just (M.delete uuid subMap, scoreMap)
                          }
                      )
              )
            liftIO $ (readTVarIO lobbies) >>= putStrLn . show
            return NoContent)

gameServer :: ServerT GameAPI AppM
gameServer = sendWord :<|> removeWord
  where
    sendWord :: WordRequest -> AppM NoContent
    sendWord (WordRequest uuid word) = do
      lobbies <- ask
      foundLobby <- liftIO . findLobbyByUUID uuid $ lobbies
      case foundLobby of
        Nothing -> throwError (err404 {errBody = "Invalid UUID."})
        Just l' -> do
          l <- liftIO $ touchLobby l'
          case lobbyState l of
            (InGame' t b subMap) -> do
              liftIO . atomically . modifyTVar lobbies $ replace l'
                (l {lobbyState = InGame' t b $ M.insertWith S.union uuid (S.singleton . (map toUpper) $ word) subMap})
              liftIO $ (readTVarIO lobbies) >>= putStrLn . show
              return NoContent
            _ -> (liftIO . atomically . modifyTVar lobbies $ replace l' l) >> return NoContent
    removeWord :: WordRequest -> AppM NoContent
    removeWord (WordRequest uuid word) = do
      lobbies <- ask
      foundLobby <- liftIO . findLobbyByUUID uuid $ lobbies
      case foundLobby of
        Nothing -> throwError (err404 {errBody = "Invalid UUID."})
        Just l' -> do
          l <- liftIO $ touchLobby l'
          case lobbyState l of
            (InGame' t b subMap) -> do
              liftIO . atomically . modifyTVar lobbies $ replace l'
                (l {lobbyState = InGame' t b $ M.adjust (S.delete . (map toUpper) $ word) uuid subMap})
              liftIO $ (readTVarIO lobbies) >>= putStrLn . show
              return NoContent
            _ -> (liftIO . atomically . modifyTVar lobbies $ replace l' l) >> return NoContent

server :: ServerT ServerAPI AppM
server = lobbyServer :<|> gameServer

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
randomLobbyCode = replicateM 4 (randomRIO ('A', 'Z'))

addPlayer :: UUID -> Text -> Lobby -> Lobby
addPlayer uuid nick lobby =
  lobby
    { players = players lobby ++ [uuid],
      nicknames = M.insert uuid nick (nicknames lobby)
    }

--- UTIL ---
findLobbyByCode :: LobbyCode -> TVar [Lobby] -> IO (Maybe Lobby)
findLobbyByCode code lobbies' = do
  lobbies <- readTVarIO lobbies'
  let validLobbies = filter (\lobby -> code == joinCode lobby) lobbies
  if length validLobbies == 1
    then return . Just $ head validLobbies
    else return Nothing

findLobbyByUUID :: UUID -> TVar [Lobby] -> IO (Maybe Lobby)
findLobbyByUUID uuid lobbies' = do
  lobbies <- readTVarIO lobbies'
  let validLobbies = filter (\lobby -> uuid `elem` (players lobby)) lobbies
  if length validLobbies == 1
    then return . Just $ head validLobbies
    else return Nothing

replace :: (Eq a) => a -> a -> [a] -> [a]
replace f r =
  fmap (\x -> if x == f then r else x)

removePlayerMidGame :: UUID -> LobbyState -> LobbyState
removePlayerMidGame uuid (InGame' t b subMap) =
  InGame' t b $ M.delete uuid subMap
removePlayerMidGame _ l = l

touchLobby :: Lobby -> IO Lobby
touchLobby l = case (lobbyState l) of
  InLobby' -> return l
  (StartingGame' t) -> do
    currentTime <- getCurrentTime
    if currentTime < t
      then return l
      else do
        board <- randomBoard (size . settings $ l)
        return $ l {lobbyState = InGame' (addUTCTime (fromIntegral . timeInSeconds . settings $ l) t) board M.empty}
  (InGame' t board submissionMap) -> do
    currentTime <- getCurrentTime
    if currentTime < t
      then return l
      else return $ l { lobbyState = InLobby',
                     previousRoundScores = Just (submissionMap, scoreWords board . foldr (\s l -> (S.toList s) ++ l) [] $ M.elems submissionMap)
                   }
