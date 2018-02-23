{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( dbFour
  , PSQL.defaultConnectInfo
  , PSQL.ConnectInfo(..)
  ) where

import qualified Database.PostgreSQL.Simple as PSQL

dbFour :: PSQL.ConnectInfo -> IO ()
dbFour connectInfo = do
  conn <- PSQL.connect connectInfo
  [PSQL.Only i] <- PSQL.query_ conn "select 2 + 2"
  putStrLn "dbFour"
  putStrLn . show $ (i :: Int)
  return ()