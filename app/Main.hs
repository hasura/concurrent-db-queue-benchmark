{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Monad
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Foldable
import Data.IORef
import Data.String (IsString (..))
import Data.Traversable
import Data.UUID (UUID)
import Database.PostgreSQL.Simple
import Formatting
import Formatting.Clock
import System.Clock
import System.Environment (getArgs)
import System.IO.Unsafe

yb :: ByteString
yb = "postgresql://yugabyte:yugabyte@localhost:5433/db"

pg :: ByteString
pg = "postgresql://postgres:password@localhost:8000/postgres"

-- | 2 KB of garbage
stuff :: ByteString
stuff = fromString $ replicate 2048 'a'

runSql f conn = void do
  q <- fromString <$> readFile f
  execute_ conn q

fetch1 :: Connection -> IO ()
fetch1 = runSql "fetch1.sql"

fetch2 :: Connection -> IO ()
fetch2 = runSql "fetch2.sql"

micro :: Int
micro = 1000000

processEvents :: Connection -> TVar Int -> String -> IO ()
processEvents conn v q = go
  where
    go = forever do
      -- fetch conn >>= print
      runSql q conn
      atomically (modifyTVar' v (+ 1))

-- putStrLn $ "grab event id " <> show e
-- threadDelay processingDelay
-- putStrLn $ "done event id " <> show e

client :: ByteString -> IO ()
client connString = void do
  print connString
  runTest True
  runTest False
  where
    runTest x = do
      putStr "initialising db... "
      conn1 <- connectPostgreSQL connString
      conn2 <- connectPostgreSQL connString
      initDb conn1

      putStrLn "done."

      counter1 <- newTVarIO 0
      counter2 <- newTVarIO 0
      handle1 <- async do processEvents conn1 counter1 "fetch1.sql"
      handle2 <- async do processEvents conn2 counter2 (if x then "fetch1.sql" else "fetch2.sql")
      start <- getTime Monotonic
      threadDelay (120 * micro)
      cancel handle1
      cancel handle2
      end <- getTime Monotonic
      n1 <- readTVarIO counter1
      n2 <- readTVarIO counter2
      putStrLn $
        (if x then "same" else "different") <> " project ids: "
          <> "processed "
          <> show n1
          <> " + "
          <> show n2
          <> " = "
          <> show (n1 + n2)
          <> " fetches in "
          <> show (toNanoSecs (diffTimeSpec end start) `div` 1_000_000)
          <> " ms"

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["pg"] -> client pg
    ["yb"] -> client yb
    _ -> putStrLn "error: expecting either 'pg' or 'yb' as a command-line argument"

initDb :: Connection -> IO ()
initDb conn = do
  q <- fromString <$> readFile "project_id_setup.sql"
  execute_ conn q
  pure ()
