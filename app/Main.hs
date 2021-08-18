{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Main where

import Control.Concurrent.Async
import Control.Monad.Base
import Control.Monad.Trans.Control
import Data.Coerce (coerce)
import qualified Data.Fixed as Fixed
import Data.Pool (Pool)
import qualified Data.Pool as Pool
import Data.String (String)
import qualified Data.String
import qualified Data.Text as T
import Data.UUID (UUID)
import qualified Data.UUID.V4 as UUID
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Database.PostgreSQL.Simple as PG
import Protolude
import Protolude.Conv
import qualified Statistics.Sample as Statistics
import qualified System.Clock as Clock
import Text.Printf

data Bench = BenchInsert | BenchFetch | BenchDelete
  deriving (Show)

newtype ProjectId = ProjectId UUID
  deriving (Show)

mkProjectId :: MonadIO m => m ProjectId
mkProjectId = ProjectId <$> liftIO UUID.nextRandom

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
readQuery f = Data.String.fromString @PG.Query . T.unpack <$> liftIO (readFile f)

--------------------------------------------------------------------------------
-- timing

-- | an interval in milliseconds with two digits of precision
type Interval = Fixed.Fixed Fixed.E2

newtype ElapsedTime = ElapsedTime Interval deriving (Show)

measureElapsed :: MonadIO m => m a -> m (ElapsedTime, a)
measureElapsed k = do
  let getTime = liftIO (Clock.getTime Clock.Monotonic)
  start <- getTime
  ret <- k
  end <- getTime
  let d = fromInteger @Interval (Clock.toNanoSecs (Clock.diffTimeSpec end start))
  pure (ElapsedTime (d / 1_000_000), ret)

elapsedTime :: MonadIO m => m a -> m ElapsedTime
elapsedTime k = fst <$> measureElapsed k

--------------------------------------------------------------------------------
-- statistics

data Stats a = Stats
  { statsCount :: Int,
    statsMean :: a,
    statsStdDev :: a,
    statsSkewness :: a
  }
  deriving (Show)

toElapsedTime :: Double -> ElapsedTime
toElapsedTime = ElapsedTime . realToFrac

fromElapsedTime :: ElapsedTime -> Double
fromElapsedTime (ElapsedTime e) = realToFrac e

instance PrintfArg ElapsedTime where
  formatArg = formatArg . fromElapsedTime

computeStats :: Vector ElapsedTime -> Stats ElapsedTime
computeStats xs = Stats count mean std skew
  where
    count = Vector.length xs
    [mean, std, skew] =
      map
        (\f -> toElapsedTime $ f $ Vector.map fromElapsedTime xs)
        [ Statistics.mean,
          Statistics.stdDev,
          Statistics.skewness
        ]

ppStats :: MonadIO m => Vector ElapsedTime -> m ()
ppStats xs =
  let Stats {..} = computeStats xs
   in liftIO $
        printf
          "n = %3d, mean %8.1f ms, stddev %8.1f ms"
          statsCount
          statsMean
          statsStdDev

timedReplicate :: MonadIO m => Int -> m a -> m (Vector ElapsedTime)
timedReplicate n k =
  Vector.forM (Vector.fromList [1 .. n]) \_i ->
    elapsedTime k

--------------------------------------------------------------------------------
-- toplevel

main :: IO ()
main = void do
  putText "starting benchmark"
  env <- BenchEnv <$> initPool connStrLocalPG
  let numProjects = 1000
      numFetches = 10
      numExtraFetches = numFetches
      numEventsPerFetch = 100
      numEventsPerProject = numEventsPerFetch * (numFetches + numExtraFetches)

  projIds <- replicateM numProjects mkProjectId

  putText "setting up schema"
  runBenchM env do schemaSetup

  putStr @String "creating events"
  createEventsTimes <-
    Vector.fromList
      <$> forConcurrently (zip [1 ..] projIds) \(i, projId) -> do
        runBenchM env $ do
          elapsedTime do
            putStr @String ("\rcreating events: " <> show @Int i)
            createEventsFor projId numEventsPerProject
  putText ""
  putText "event creation times:"
  ppStats createEventsTimes
  putText ""

  putText "fetching events"
  batchFetchTimes <-
    Vector.concat <$> forConcurrently projIds \projId -> do
      runBenchM env do
        timedReplicate numFetches (projectWorker projId numEventsPerFetch)

  putText "batch fetch times: "
  ppStats batchFetchTimes
  putText ""

--------------------------------------------------------------------------------
-- database operations

schemaSetup :: BenchM ()
schemaSetup = withConn \conn -> void do
  setup <- readQuery "sql/setup.sql"
  liftIO (PG.execute_ conn setup)

-- putText ("created schema: " <> show r)

createEventsFor :: ProjectId -> Int -> BenchM ()
createEventsFor (ProjectId projectId) numEventsPerProject = withConn \conn -> void do
  q <- readQuery "sql/create_events.sql"
  liftIO (PG.execute conn q (projectId, numEventsPerProject))

-- putText ("created " <> show r <> " events")

fetchEventsFor :: ProjectId -> Int -> BenchM ()
fetchEventsFor (ProjectId proj) numEventsPerFetch = withConn \conn -> void do
  -- putText ("fetching events for " <> show proj)
  q <- readQuery "sql/fetch.sql"
  liftIO (PG.execute conn q (proj, numEventsPerFetch, proj))

-- putText ("fetched " <> show r <> " events")

projectWorker :: ProjectId -> Int -> BenchM ()
projectWorker projId numEventsPerFetch = do
  fetchEventsFor projId numEventsPerFetch
  liftIO (threadDelay (1 * 1_000_000))
