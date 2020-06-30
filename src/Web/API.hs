{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Web.API where

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.STM.TVar
  ( TVar,
    newTVar,
    readTVar,
    writeTVar,
  )
import Control.Exception (bracket)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.STM (atomically)
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text
import Data.UUID
import GHC.Generics (Generic)
import Network.Wai.Handler.Warp (run)
import Servant
import Servant.API
import Servant.API.Verbs
import Servant.Server

import Web.Types

type AppM = ReaderT ServerState Handler

type LobbyAPI =
  "new" :> QueryParam "nick" Text :> Post '[JSON] UUID
    :<|> "join" :> QueryParam "nick" Text :> QueryParam "lobbycode" String :> Post '[JSON] UUID
    :<|> "info" :> QueryParam "uuid" UUID :> Post '[JSON] Lobby
    :<|> "settings" :> QueryParam "uuid" UUID :> QueryParam "size" Int :> QueryParam "time" Int :> PostNoContent
    :<|> "startgame" :> QueryParam "uuid" UUID :> PostNoContent
    :<|> "leave" :> QueryParam "uuid" UUID :> DeleteNoContent

type GameAPI =
  "sendword" :> QueryParam "uuid" UUID :> QueryParam "word" String :> PostNoContent
  :<|> "removeword" :> QueryParam "uuid" UUID :> QueryParam "word" String :> PostNoContent

type ServerAPI = "lobby" :> LobbyAPI :<|> "game" :> GameAPI

api :: Proxy ServerAPI
api = Proxy
