{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Control.Concurrent.Async (forConcurrently_)
import Control.Monad.Trans.Control
import Control.Monad.Base
import Data.Coerce (coerce)
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

data Bench = BenchInsert | BenchFetch | BenchDelete
  deriving (Show)

-- runSql f conn = void do
--   q <- strConv Strict <$> readFile f
--   PG.execute_ conn q

newtype BenchEnv = BenchEnv {benchConn :: Pool PG.Connection}

newtype BenchM a = BenchM {unBenchM :: ReaderT BenchEnv IO a}
  deriving
    (Functor, Applicative, Monad, MonadIO, MonadBase IO, MonadBaseControl IO, MonadReader BenchEnv)
    via (ReaderT BenchEnv IO)

withConn :: (PG.Connection -> BenchM a) -> BenchM a
withConn k = do
  pool <- asks benchConn
  Pool.withResource pool k

readQuery :: MonadIO m => FilePath -> m PG.Query
readQuery f = Data.String.fromString . T.unpack <$> liftIO (readFile f)

schemaSetup :: BenchM ()
schemaSetup = withConn \conn -> do
  putText "setting up schema"
  setup <- readQuery "sql/setup.sql"
  print =<< liftIO (PG.execute_ conn setup)

createProjects :: Int -> Int -> BenchM [ProjectId]
createProjects numProjects numEventsPerProject = withConn \conn -> do
  putText "creating projects"

  insertProjectIds <- readQuery "sql/insert_ids.sql"
  uuid <- replicateM numProjects (ProjectId <$> liftIO UUID.nextRandom)
  print =<< liftIO (PG.executeMany conn insertProjectIds (coerce uuid :: [PG.Only UUID]))

  createEvents <- readQuery "sql/populate.sql"
  print =<< liftIO (PG.execute conn createEvents (PG.Only numEventsPerProject))

  pure uuid

fetchEventsFor :: ProjectId -> BenchM ()
fetchEventsFor (ProjectId proj) = withConn \conn -> do
  putText ("fetching events for " <> show proj)
  q <- readQuery "sql/fetch.sql"
  print =<< liftIO (PG.execute conn q (proj, proj))

type ConnString = ByteString

connStrLocalPG :: ConnString
connStrLocalPG = "postgresql://postgres:password@localhost:5432/test"

newtype ProjectId = ProjectId UUID
  deriving (Show)

runBenchM :: BenchEnv -> BenchM a -> IO a
runBenchM env k = runReaderT (unBenchM k) env

initPool :: ConnString -> IO (Pool PG.Connection)
initPool s = Pool.createPool (PG.connectPostgreSQL s) PG.close 4 60 100

main :: IO ()
main = void do
  putText "starting benchmark"
  env <- BenchEnv <$> initPool connStrLocalPG

  projIds <- runBenchM env do
    schemaSetup
    createProjects 400 400

  forConcurrently_ projIds \projId -> do
    runBenchM env do
      fetchEventsFor projId
