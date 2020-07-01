module Main where

import Data.Map (empty)

import Control.Concurrent.STM
import Network.Wai.Handler.Warp (run)

import Web.API
import Web.Types
import Web.Server

main :: IO ()
main = do
  let port = 9091
  lobbies <- atomically $ newTVar []
  run port . app $ lobbies

