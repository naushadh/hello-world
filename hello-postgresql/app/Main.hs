module Main where

import qualified Lib
import qualified Control.Concurrent as Concurrent
import qualified System.Environment as Env

main :: IO ()
main = do
  -- minor hack to allow database to come up
  Concurrent.threadDelay $ 2 * 1000 * 1000
  -- source credentials from ENV
  (dbHost,dbUser,dbPass,dbName)
    <- (,,,)
    <$> Env.getEnv "APP_DB_HOST"
    <*> Env.getEnv "APP_DB_USER"
    <*> Env.getEnv "APP_DB_PASS"
    <*> Env.getEnv "APP_DB_NAME"
  let connectInfo = Lib.defaultConnectInfo
        { Lib.connectHost    = dbHost
        , Lib.connectUser    = dbUser
        , Lib.connectPassword= dbPass
        , Lib.connectDatabase= dbName
        }
  -- fetch something from DB
  Lib.dbFour connectInfo
