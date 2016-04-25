{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Juno.Persistence.SQLite where

import Control.Concurrent.Chan.Unagi
import Database.SQLite.Simple
import Database.SQLite.Simple.Ok
import Database.SQLite.Simple.ToField
import Database.SQLite.Simple.FromField
import Data.Typeable
import Data.Set
import Data.Serialize
import Data.Sequence
import Data.ByteString hiding (concat)
import qualified Data.Text as T

import Juno.Types

-- These live here as orphans, and not in Types, because trying to Serialize these things should be a type level error
-- with rare exception (i.e. for hashing the log entry). Moreover, accidentally sending Provenance over the wire could
-- be hard to catch. Best to make it impossible.
instance Serialize Command
instance Serialize Provenance
instance Serialize LogEntry
instance Serialize RequestVoteResponse

instance ToField NodeID where
  toField n = toField $ encode n
instance FromField NodeID where
  fromField f = do
    s :: ByteString <- fromField f
    case decode s of
      Left err -> returnError ConversionFailed f ("Couldn't deserialize sequence: " ++ err)
      Right n -> Ok n

instance ToField LogIndex where
  toField (LogIndex i) = toField i
instance FromField LogIndex where
  fromField a = LogIndex <$> fromField a

instance ToField LogEntry where
  toField LogEntry{..} = toField $ encode (_leTerm, _leCommand, _leHash)

instance ToField Term where
  toField (Term a) = toField a
instance FromField Term where
  fromField a = Term <$> fromField a

instance Serialize a => ToField (Seq a) where
  toField s = toField $ encode s
instance (Typeable a, Serialize a) => FromField (Seq a) where
  fromField f = do
    s :: ByteString <- fromField f
    case decode s of
      Left err -> returnError ConversionFailed f ("Couldn't deserialize sequence: " ++ err)
      Right v -> Ok v

instance (Ord a, Serialize a) => ToField (Set a) where
  toField s = toField $ encode s
instance (Ord a, Typeable a, Serialize a) => FromField (Set a) where
  fromField f = do
    s :: ByteString <- fromField f
    case decode s of
      Left err -> returnError ConversionFailed f ("Couldn't deserialize sequence: " ++ err)
      Right v -> Ok v

instance ToField Provenance where
  toField = toField . encode
instance FromField Provenance where
  fromField f = do
    s :: ByteString <- fromField f
    case decode s of
      Left err -> returnError ConversionFailed f ("Couldn't deserialize sequence: " ++ err)
      Right v -> Ok v

instance ToRow AppendEntries where
  toRow AppendEntries{..} = [toField _aeTerm
                            ,toField _leaderId
                            ,toField _prevLogIndex
                            ,toField _prevLogTerm
                            ,toField _aeEntries
                            ,toField _aeQuorumVotes
                            ,toField _aeProvenance ]



instance FromRow AppendEntries where
    fromRow = AppendEntries <$> field <*> field <*> field <*> field <*> field <*> field <*> field

sqlDbSchema :: Query
sqlDbSchema = Query $ T.pack $ concat
    ["CREATE TABLE IF NOT EXISTS 'main'.'appendEntries' "
    ,"( 'term' INTEGER PRIMARY KEY  NOT NULL  UNIQUE"
    ,", 'leader_id' TEXT"
    ,", 'prev_log_index' INTEGER"
    ,", 'prev_log_term' INTEGER"
    ,", 'entries' BLOB"
    ,", 'quorum_votes' BLOB"
    ,", 'sig' BLOB"
    ,")"]

sqlInsertAE :: Query
sqlInsertAE = Query $ T.pack $ concat
    ["INSERT INTO 'main'.'appendEntries' "
    ,"( 'term'"
    ," 'leader_id'"
    ," 'prev_log_index'"
    ," 'pref_log_term'"
    ," 'entries'"
    ," 'quorum_votes'"
    ," 'sig')"
    ," VALUES (?,?,?,?,?,?,?)"]

sqlSelectAllAE :: Query
sqlSelectAllAE = Query $ T.pack $ concat
    ["SELECT term, leader_id, prev_log_index, prev_log_term, entries, quorum_votes, sig"
    ,"FROM 'main'.'appendEntries'"
    ,"ORDER BY term ASC"]

sqlSelectLastAE :: Query
sqlSelectLastAE = Query $ T.pack $ concat
    ["SELECT term, leader_id, prev_log_index, prev_log_term, entries, quorum_votes, sig"
    ,"FROM 'main'.'appendEntries'"
    ,"ORDER BY term DESC"
    ,"LIMIT 1"]

createDB :: FilePath -> IO Connection
createDB f = do
  conn <- open f
  execute_ conn sqlDbSchema
  return conn

insertAppendEntries :: Connection -> AppendEntries -> IO ()
insertAppendEntries conn ae = execute conn sqlInsertAE ae

persistenceThread :: OutChan (Either String AppendEntries) -> Connection -> IO ()
persistenceThread toDB conn = do
  ae <- readChan toDB
  case ae of
    Right ae' -> withTransaction conn $ insertAppendEntries conn ae' >> persistenceThread toDB conn
    Left _ -> close conn
