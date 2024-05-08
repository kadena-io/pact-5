{-# LANGUAGE OverloadedStrings #-}

-- |

module Main where

import Control.Monad.Catch
import qualified Database.SQLite3 as SQL
import Data.Text (Text)
import qualified Data.Text as T
import Data.ByteString (ByteString)
import Data.Int (Int64)
import Control.Monad
import Pact.Core.Serialise.LegacyPact
import Pact.Core.Hash

import System.Environment
import qualified Pact.JSON.Decode as JD

withStmt :: SQL.Database -> Text -> (SQL.Statement -> IO a) -> IO a
withStmt conn sql = bracket (SQL.prepare conn sql) SQL.finalize

userTables :: SQL.Database -> IO [Text]
userTables con = withStmt con qry $ go []
  where
  qry = "select name from sqlite_master where type='table' and name like '%#-table' escape '#'"
  go acc stmt =  SQL.step stmt >>= \case
    SQL.Done -> pure acc
    SQL.Row -> do
      [SQL.SQLText tbl] <- SQL.columns stmt
      go (tbl: acc) stmt

data RawRow
  = RawRow
  { _rrKey :: Text
  , _rrTxId :: Int64
  , _rrPayload :: ByteString
  }

getRawData :: SQL.Database -> Text -> IO [RawRow]
getRawData con tbl = withStmt con qry $ go []
  where
  qry = "select rowkey,txid,rowdata from [" <> tbl <> "]"
  go acc stmt =  SQL.step stmt >>= \case
    SQL.Done -> pure acc
    SQL.Row -> do
      [SQL.SQLText key, SQL.SQLInteger txid, SQL.SQLBlob value] <- SQL.columns stmt
      go (RawRow key txid value : acc) stmt


rawTest :: JD.FromJSON a => SQL.Database -> Text -> (a -> Either String c) -> IO ()
rawTest db tbl fromLegacy = do
  print tbl
  keys <- getRawData db tbl
  forM_ keys $ \(RawRow i txid payload) -> case JD.eitherDecodeStrict' payload of
      Right lo -> case fromLegacy lo of
        Left e ->
          putStrLn $ "\t" <> show i <> " " <> show txid <> " " <> e
        Right _ -> pure ()
      Left err -> putStrLn $ "Fatal: decoding into legacy format failed: "
          <> show tbl <> " = " <> show i <> " " <> show txid <> " with : " <> err



main :: IO ()
main = getArgs >>= \case
  [dbstr] -> bracket (SQL.open (T.pack dbstr)) SQL.close $ \db -> do

    rawTest db "SYS:Modules" (runTranslateM . fromLegacyModuleData placeholderHash)
    rawTest db "SYS:KeySets" fromLegacyKeySet
    rawTest db "SYS:Namespaces" fromLegacyNamespace    -- rawTest db "SYS:Pacts" fromLegacyDefPactExec

    tbls <- userTables db
    forM_ tbls $ \tbl -> do
      print tbl
      ud <- getRawData db tbl
      forM_ ud $ \(RawRow k txid payload) -> case JD.decodeStrict' payload of
        Just lo -> case fromLegacyRowData lo of
          Left e -> putStrLn $ "\t" <> show k <> " " <> show txid <> " : " <> e
          Right _ -> pure ()
        Nothing -> putStrLn $ "Fatal: decoding into legacy format failed at: "
          <> show k <> " " <> show txid

  _ -> error "Wrong number of arguments, expected 'file.sqlite'"
