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
import System.IO.Unsafe

hello :: IO Int
hello = do
  conn <- connectPostgreSQL pg
  [Only i] <- query_ conn "select 2 + 2"
  return i

yb :: ByteString
yb = "postgresql://yugabyte:yugabyte@localhost:5433/db"

pg :: ByteString
pg = "postgresql://postgres:password@localhost:8000/postgres"

connString :: ByteString
connString = yb

-- | 2 KB of garbage
stuff :: ByteString
stuff = fromString $ replicate 2048 'a'

initDb :: Connection -> IO ()
initDb conn = do
  q <- fromString <$> readFile "setup.sql"
  execute_ conn q
  pure ()

createEvent :: Connection -> IO ()
createEvent conn = do
  q <- fromString <$> readFile "create_event.sql"
  execute conn q (Only stuff)
  pure ()

grabEvent :: Connection -> IO (Maybe UUID)
grabEvent conn = do
  q <- fromString <$> readFile "grab_event.sql"
  query_ conn q >>= \case
    [Only eventId] -> pure eventId
    x -> do
      print x
      pure Nothing

markEventCompleted :: Connection -> UUID -> IO ()
markEventCompleted conn id = do
  q <- fromString <$> readFile "complete_event.sql"
  execute conn q (Only id)
  -- putStrLn $ "marked complete " <> show id
  pure ()

micro :: Int
micro = 1000000

processingDelay :: Int
processingDelay = 2 * micro

eventsConsumedPerSecond :: Int
eventsConsumedPerSecond = 5

eventsProducedPerSecond :: Int
eventsProducedPerSecond = 200

consumerSpawnDelay :: Int
consumerSpawnDelay = micro `div` eventsConsumedPerSecond

produceDelay :: Int
produceDelay = micro `div` eventsProducedPerSecond

numProducers = 5

numConsumers = 20

processEvents :: Connection -> TVar Int -> IO ()
processEvents conn v = go
  where
    go = do
      eventId <- grabEvent conn
      for eventId \e -> do
        markEventCompleted conn e
        atomically (modifyTVar' v (+ 1))
        go
      pure ()

-- putStrLn $ "grab event id " <> show e
-- threadDelay processingDelay
-- putStrLn $ "done event id " <> show e

client :: IO ()
client = do
  conn <- connectPostgreSQL connString
  -- putStrLn "initialising db"
  initDb conn

  producers <- for [1 .. numProducers] \j -> do
    -- putStrLn $ "producer " <> show j <> " spawn"
    handle <- async do
      forever do
        createEvent conn
        threadDelay produceDelay
    pure handle

  threadDelay (2 * micro)

  counter <- newTVarIO 0

  consumers <- for [1 .. numConsumers] \j -> do
    -- putStrLn $ "consumer " <> show j <> " spawn"
    async do processEvents conn counter

  start <- getTime Monotonic

  threadDelay (10 * micro)
  -- putStrLn $ "killing consumers"
  traverse_ cancel consumers

  end <- getTime Monotonic
  -- fprint (timeSpecs % "\n") start end
  n <- readTVarIO counter
  putStrLn $
    "processed " <> show n <> " events in "
      <> show (toNanoSecs (diffTimeSpec end start) `div` 1_000_000)
      <> " ms with " <> show numConsumers <> " consumers"

  -- putStrLn $ "killing producers"
  traverse_ cancel producers

  pure ()

main :: IO ()
main = do
  print connString
  client
