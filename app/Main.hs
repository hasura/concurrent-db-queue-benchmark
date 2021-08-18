{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Main where

import Control.Concurrent.Async (forConcurrently_)
import Control.Monad.Base
import Control.Monad.Trans.Control
import Data.Coerce (coerce)
import qualified Data.Fixed as Fixed
import Data.Pool (Pool)
import qualified Data.Pool as Pool
import qualified Data.String
import qualified Data.Text as T
import Data.UUID (UUID)
import qualified Data.UUID.V4 as UUID
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Database.PostgreSQL.Simple as PG
import Protolude
import Protolude.Conv
import qualified System.Clock as Clock

data Bench = BenchInsert | BenchFetch | BenchDelete
  deriving (Show)

newtype ProjectId = ProjectId UUID
  deriving (Show)

--------------------------------------------------------------------------------
-- the monad we run benchmarks in

newtype BenchEnv = BenchEnv {benchConn :: Pool PG.Connection}

newtype BenchM a = BenchM {unBenchM :: ReaderT BenchEnv IO a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadBase IO,
      MonadBaseControl IO,
      MonadReader BenchEnv
    )
    via (ReaderT BenchEnv IO)

runBenchM :: BenchEnv -> BenchM a -> IO a
runBenchM env k = runReaderT (unBenchM k) env

--------------------------------------------------------------------------------
-- database connections and connection pools

withConn :: (PG.Connection -> BenchM a) -> BenchM a
withConn k = do
  pool <- asks benchConn
  Pool.withResource pool k

initPool :: ConnString -> IO (Pool PG.Connection)
initPool s = Pool.createPool (PG.connectPostgreSQL s) PG.close 4 60 10

type ConnString = ByteString

connStrLocalPG :: ConnString
connStrLocalPG = "postgresql://postgres:password@localhost:5432/test"

readQuery :: MonadIO m => FilePath -> m PG.Query
readQuery f = Data.String.fromString . T.unpack <$> liftIO (readFile f)

--------------------------------------------------------------------------------
-- timing

-- | an interval in seconds with two digits of precision
type Interval = Fixed.Fixed Fixed.E2

newtype ElapsedTime = ElapsedTime Interval deriving (Show)

measureElapsed :: MonadIO m => m a -> m (ElapsedTime, a)
measureElapsed k = do
  let getTime = liftIO (Clock.getTime Clock.Monotonic)
  start <- getTime
  ret <- k
  end <- getTime
  let d = fromInteger @Interval (Clock.toNanoSecs (Clock.diffTimeSpec end start))
  pure (ElapsedTime (d / 1_000_000_000), ret)

--------------------------------------------------------------------------------
-- database operations

schemaSetup :: BenchM ()
schemaSetup = withConn \conn -> void do
  putText "setting up schema"
  setup <- readQuery "sql/setup.sql"
  liftIO (PG.execute_ conn setup)

createEventsFor :: ProjectId -> Int -> BenchM ()
createEventsFor (ProjectId projectId) numEventsPerProject = withConn \conn -> do
  q <- readQuery "sql/create_events.sql"
  r <- liftIO (PG.execute conn q (projectId, numEventsPerProject))
  putText ("created " <> show r <> " events")

fetchEventsFor :: ProjectId -> Int -> BenchM ()
fetchEventsFor (ProjectId proj) numEventsPerFetch = withConn \conn -> do
  -- putText ("fetching events for " <> show proj)
  q <- readQuery "sql/fetch.sql"
  r <- liftIO (PG.execute conn q (proj, numEventsPerFetch, proj))
  putText ("fetched " <> show r <> " events")

projectWorker :: ProjectId -> Int -> BenchM ()
projectWorker projId numEventsPerFetch = do
  fetchEventsFor projId numEventsPerFetch
  liftIO (threadDelay (1 * 1_000_000))

--------------------------------------------------------------------------------
-- toplevel

main :: IO ()
main = void do
  putText "starting benchmark"
  env <- BenchEnv <$> initPool connStrLocalPG
  let numProjects = 100
      numFetches = 10
      numEventsPerFetch = 100
      numEventsPerProject = numEventsPerFetch * numFetches

  projIds <- replicateM numProjects (ProjectId <$> liftIO UUID.nextRandom)

  runBenchM env do schemaSetup

  (t, _) <- measureElapsed do
    forConcurrently_ projIds \projId -> do
      runBenchM env $
        createEventsFor projId numEventsPerProject
  print t

  (t, _) <- measureElapsed do
    forConcurrently_ projIds \projId -> do
      runBenchM env $
        replicateM_ numFetches $
          projectWorker projId numEventsPerFetch
  print t
