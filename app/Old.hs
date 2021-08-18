{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TMVar
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

citus1 :: ByteString
citus1 = "postgresql://postgres:password@localhost:5434/postgres"

citus :: ByteString
citus = "postgresql://postgres:password@localhost:5440/postgres"

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

data LockState = YourMove
  deriving (Show)

data Client = Alicia | Bobert
  deriving (Show)

processEvents ::
  Client ->
  Connection ->
  TVar Int ->
  Maybe (TMVar LockState, TMVar LockState) ->
  String ->
  IO ()
processEvents ident conn counter Nothing q = go
  where
    go = forever do
      -- putStrLn $ show ident <> " start"
      runSql q conn
      -- putStrLn $ show ident <> " done"
      atomically do
        modifyTVar' counter (+ 1)
processEvents ident conn counter (Just (ownLock, rivalLock)) q = go
  where
    go = forever do
      n <- readTVarIO counter
      -- putStrLn $ show ident <> " #" <> show n <> "   pre: start, waiting to take rival"
      states <- atomically do
        YourMove <- takeTMVar rivalLock
        (,) <$> isEmptyTMVar ownLock <*> isEmptyTMVar rivalLock
      -- putStrLn $ show ident <> " #" <> show n <> "   pre: took from rival lock"
      runSql q conn
      -- putStrLn $ show ident <> " #" <> show n <> "  post: waiting to write own"
      states <- atomically do
        modifyTVar' counter (+ 1)
        putTMVar ownLock YourMove
        (,) <$> isEmptyTMVar ownLock <*> isEmptyTMVar rivalLock
      pure ()

-- putStrLn $ show ident <> " #" <> show n <> "  post: wrote to own lock, done"

-- putStrLn $ "grab event id " <> show e
-- threadDelay processingDelay
-- putStrLn $ "done event id " <> show e

data Locking = UseLock | NoLock

data Filter = Conflict | Independent

client :: ByteString -> Locking -> IO ()
client connString _ = void do
  putStrLn $ "using conn string: " <> show connString
  runTest UseLock Conflict
  runTest NoLock Conflict
  where
    runTest l filt = do
      putStrLn "initialising db... "
      conn1 <- connectPostgreSQL connString
      conn2 <- connectPostgreSQL connString
      start <- getTime Monotonic
      initDb conn1
      end <- getTime Monotonic
      putStrLn $
        "init took "
          <> show (toNanoSecs (diffTimeSpec end start) `div` 1_000_000_000)
          <> " seconds"

      counter1 <- newTVarIO 0
      counter2 <- newTVarIO 0

      (locks1, locks2) <- case l of
        UseLock -> atomically do
          lock <- newEmptyTMVar
          lock' <- newTMVar YourMove -- CRITICAL that only one tmvar is filled
          pure (Just (lock, lock'), Just (lock', lock)) -- TODO this looks sketchy lol
        NoLock -> pure (Nothing, Nothing)
      handle1 <- async do processEvents Alicia conn1 counter1 locks1 "fetch1.sql"
      handle2 <- async do
        processEvents
          Bobert
          conn2
          counter2
          locks2
          ( case filt of
              Conflict -> "fetch1.sql"
              Independent -> "fetch2.sql"
          )
      start <- getTime Monotonic
      threadDelay (120 * micro)
      cancel handle1
      cancel handle2
      end <- getTime Monotonic
      n1 <- readTVarIO counter1
      n2 <- readTVarIO counter2
      putStrLn $
        ( case filt of
            Conflict -> "same"
            Independent -> "different"
        )
          <> " project ids: "
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
    [db, locking] -> do
      let connString = case db of
            "pg" -> pg
            "yb" -> yb
            "citus" -> citus
            "citus1" -> citus1
      let useLocks = case locking of
            "enable-locking" -> UseLock
            "disable-locking" -> NoLock
      client connString useLocks
    _ -> putStrLn "error: bad args"

initDb :: Connection -> IO ()
initDb conn = do
  q <- fromString <$> readFile "project_id_setup.sql"
  execute_ conn q
  pure ()
