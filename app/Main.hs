module Main where

import Data.Map (empty)

import Control.Concurrent.STM
import Network.Wai.Handler.Warp (run)

import Web.API
import Web.Types

main :: IO ()
main = do
  let port = 9091
  startingUserMap <-
    atomically . newTVar $ empty
  startingLobbyMap <-
    atomically . newTVar $ empty
  run port . app $
    ServerState {userMap = startingUserMap, lobbyMap = startingLobbyMap}

