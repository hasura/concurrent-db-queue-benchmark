{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Main where

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

newtype BenchEnv = BenchEnv {benchConn :: PG.Connection}

newtype BenchM a = BenchM {unBenchM :: ReaderT BenchEnv IO a}
  deriving
    (Functor, Applicative, Monad, MonadIO, MonadReader BenchEnv)
    via (ReaderT BenchEnv IO)

readQuery :: MonadIO m => FilePath -> m PG.Query
readQuery f = Data.String.fromString . T.unpack <$> liftIO (readFile f)

schemaSetup :: BenchM ()
schemaSetup = do
  putText "setting up schema"
  conn <- asks benchConn
  setup <- readQuery "sql/setup.sql"
  print =<< liftIO (PG.execute_ conn setup)

createProjects :: Int -> Int -> BenchM [ProjectId]
createProjects numProjects numEventsPerProject = do
  putText "creating projects"
  conn <- asks benchConn

  insertProjectIds <- readQuery "sql/insert_ids.sql"
  uuid <- replicateM numProjects (ProjectId <$> liftIO UUID.nextRandom)
  print =<< liftIO (PG.executeMany conn insertProjectIds (coerce uuid :: [PG.Only UUID]))

  createEvents <- readQuery "sql/populate.sql"
  print =<< liftIO (PG.execute conn createEvents (PG.Only numEventsPerProject))

  pure uuid

fetchEvents :: ProjectId -> BenchM ()
fetchEvents (ProjectId proj) = do
  putText ("fetching events for " <> show proj)
  q <- readQuery "sql/fetch.sql"
  conn <- asks benchConn
  print =<< liftIO (PG.execute conn q (proj, proj))

connStrLocalPG :: ByteString
connStrLocalPG = "postgresql://postgres:password@localhost:5432/test"

newtype ProjectId = ProjectId UUID
  deriving (Show)

runBenchM :: BenchEnv -> BenchM a -> IO a
runBenchM env k = runReaderT (unBenchM k) env

main :: IO ()
main = void do
  putText "starting benchmark"
  conn <- PG.connectPostgreSQL connStrLocalPG
  let env = BenchEnv conn
  runBenchM env do
    schemaSetup
    projIds <- createProjects 100 100
    for_ projIds \projId -> do
      fetchEvents projId
