{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module PG.Store
    ( newPgSqlStore
    , Store
    , putEntry, searchEntry
    )
where

import PG.Types

import Control.Logger.Simple
import Control.Monad.Trans
import Data.Functor.Contravariant
import Data.Option
import Data.Text.ToFromText
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E
import qualified Hasql.Migration as M
import qualified Hasql.Pool as P
import qualified Hasql.Query as Q
import qualified Hasql.Session as S
import qualified Hasql.Transaction as Tx
import qualified Hasql.Transaction.Sessions as Tx

newtype Store
    = Store
    { unStore :: P.Pool }

dbTx :: Tx.IsolationLevel -> Tx.Mode -> Tx.Transaction a -> S.Session a
dbTx = Tx.transaction

withPool :: Store -> S.Session a -> IO a
withPool pss sess =
    do res <- P.use (unStore pss) sess
       case res of
         Left err ->
           do logError (showText err)
              fail (show err)
         Right ok -> pure ok

newPgSqlStore :: BS.ByteString -> IO Store
newPgSqlStore connStr =
    do pool <- P.acquire (20, 60 * 5, connStr)
       let store = Store pool
       logInfo "Loading and running migrations from db/ ..."
       migs <- M.loadMigrationsFromDirectory "db"
       withPool store $
           let loop [] = pure ()
               loop (mig : more) =
                  do liftIO $ logNote ("Will check and or run " <> niceMigration mig)
                     res <-
                         dbTx Tx.ReadCommitted Tx.Write $ M.runMigration mig
                     case res of
                       M.MigrationError err ->
                           fail err
                       M.MigrationSuccess ->
                           do liftIO $ logNote ("Finished " <> niceMigration mig)
                              loop more
           in loop (M.MigrationInitialization : migs)
       logInfo "Migrations complete, store ready"
       pure store

niceMigration :: M.MigrationCommand -> T.Text
niceMigration cmd =
    case cmd of
      M.MigrationInitialization -> "init migration"
      M.MigrationScript s _ -> "script " <> T.pack s
      M.MigrationValidation mc -> "validate (" <> niceMigration mc <> ")"

putEntry :: Store -> Entry -> IO ()
putEntry store entry =
    withPool store $
    dbTx Tx.ReadCommitted Tx.Write $
    Tx.query entry putEntryQ

putEntryQ :: Q.Query Entry ()
putEntryQ =
    Q.statement sql encoder decoder True
    where
      sql =
          "INSERT INTO entry "
          <> "(key, ty, authors, title, year, journal, url, ee, pages, volume, editor, series)"
          <> " VALUES "
          <> "($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12) "
          <> "ON CONFLICT (key) DO NOTHING;"
      encoder =
          contramap e_key (E.value E.text)
          <> contramap (toText . e_type) (E.value E.text)
          <> contramap e_authors (E.value (E.array (E.arrayDimension V.foldl' (E.arrayValue E.text))))
          <> contramap e_title (E.value E.text)
          <> contramap (fmap fromIntegral . optionToMaybe . e_year) (E.nullableValue E.int4)
          <> contramap (optionToMaybe . e_journal) (E.nullableValue E.text)
          <> contramap (optionToMaybe . e_url) (E.nullableValue E.text)
          <> contramap (optionToMaybe . e_ee) (E.nullableValue E.text)
          <> contramap (optionToMaybe . e_pages) (E.nullableValue E.text)
          <> contramap (optionToMaybe . e_volume) (E.nullableValue E.text)
          <> contramap (optionToMaybe . e_editor) (E.nullableValue E.text)
          <> contramap (optionToMaybe . e_series) (E.nullableValue E.text)
      decoder =
          D.unit

searchEntry :: Store -> T.Text -> IO (V.Vector RankedEntry)
searchEntry store entry =
    withPool store $
    dbTx Tx.ReadCommitted Tx.Write $
    Tx.query entry searchEntryQ

searchEntryQ :: Q.Query T.Text (V.Vector RankedEntry)
searchEntryQ =
    Q.statement sql encoder decoder True
    where
      sql =
          "SELECT "
          <> "(key, ty, authors, title, year, journal, url, ee, pages, volume, editor, series, "
          <> "  ts_rank_cd(tsv, query) AS rank)"
          <> " FROM "
          <> " entry, to_tsquery($1) query"
          <> " WHERE query @@ tsv ORDER BY rank DESC LIMIT 10"
      encoder =
          contramap mkQuery (E.value E.text)
      decoder =
          D.rowsVector $
          do e_key <- D.value D.text
             e_type <- D.value D.text >>= fromText
             e_authors <- D.value (D.array (D.arrayDimension V.replicateM (D.arrayValue D.text)))
             e_title <- D.value D.text
             e_year <- fmap fromIntegral . maybeToOption <$> D.nullableValue D.int4
             e_journal <- maybeToOption <$> D.nullableValue D.text
             e_url <- maybeToOption <$> D.nullableValue D.text
             e_ee <- maybeToOption <$> D.nullableValue D.text
             e_pages <- maybeToOption <$> D.nullableValue D.text
             e_volume <- maybeToOption <$> D.nullableValue D.text
             e_editor <- maybeToOption <$> D.nullableValue D.text
             e_series <- maybeToOption <$> D.nullableValue D.text
             re_rank <- D.value D.float8
             let re_entry = Entry {..}
             pure RankedEntry {..}

mkQuery :: T.Text -> T.Text
mkQuery =
    T.intercalate " | " . map (<> ":*") . T.words