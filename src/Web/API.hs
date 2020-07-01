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
  "new" :> ReqBody '[FormUrlEncoded] NameRequest :> Post '[JSON] UUID
    :<|> "join" :> ReqBody '[FormUrlEncoded] JoinRequest :> Post '[JSON] UUID
    :<|> "info" :> ReqBody '[FormUrlEncoded] UUIDRequest :> Post '[JSON] RedactedLobby
    :<|> "settings" :> ReqBody '[FormUrlEncoded] SettingsRequest :> Post '[JSON] NoContent
    -- :<|> "startgame" :> QueryParam "uuid" UUID :> PostNoContent
    -- :<|> "leave" :> QueryParam "uuid" UUID :> DeleteNoContent

type GameAPI =
  "sendword" :> QueryParam "uuid" UUID :> QueryParam "word" String :> PostNoContent
  :<|> "removeword" :> QueryParam "uuid" UUID :> QueryParam "word" String :> PostNoContent

type ServerAPI = "lobby" :> LobbyAPI -- :<|> "game" :> GameAPI

api :: Proxy ServerAPI
api = Proxy
