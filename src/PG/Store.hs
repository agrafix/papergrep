{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module PG.Store
    ( newPgSqlStore, withTempStore
    , Store
    , putEntry, searchEntry
    )
where

import PG.Types

import Control.Error
import Control.Exception
import Control.Logger.Simple
import Control.Monad.Trans
import Data.Char
import Data.Functor.Contravariant
import Data.Int
import Data.Option
import Data.Text.ToFromText
import System.Random
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Hasql.Connection as C
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
         Left errMsg ->
           do logError (showText errMsg)
              fail (show errMsg)
         Right ok -> pure ok

withTempStore :: (Store -> IO a) -> IO a
withTempStore run =
    bracket allocDb removeDb $ \(_, dbname) ->
    bracket (newPgSqlStore $ "dbname=" <> dbname) (\_ -> pure ()) $ run
    where
        assertRight y =
            case y of
              Right x -> pure x
              Left errMsg -> fail (show errMsg)
        removeDb (globalConn, dbname) =
            do logInfo ("Removing temporary database" <> showText dbname)
               runRes2 <-
                   flip S.run globalConn $ S.sql $ "DROP DATABASE IF EXISTS " <> dbname
               assertRight runRes2
               C.release globalConn
        allocDb =
            do globalConnE <- C.acquire ""
               globalConn <- assertRight globalConnE
               dbnameSuffix <-
                   BSC.pack . take 10 . randomRs ('a', 'z') <$>
                   newStdGen
               let dbname = "chrtest" <> dbnameSuffix
               logInfo ("Creating temporary database" <> showText dbname)
               runRes <-
                   flip S.run globalConn $
                   do S.sql $ "DROP DATABASE IF EXISTS " <> dbname
                      S.sql $ "CREATE DATABASE " <> dbname
               assertRight runRes
               localConnE <- C.acquire $ "dbname=" <> dbname
               localConn <- assertRight localConnE
               runRes' <-
                   flip S.run localConn $
                   do S.sql $ "CREATE EXTENSION hstore"
               assertRight runRes'
               C.release localConn
               pure (globalConn, dbname)

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
                       M.MigrationError errMsg ->
                           fail errMsg
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
          <> contramap (optionToMaybe . e_title) (E.nullableValue E.text)
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
          <> "key, ty, authors, title, year, journal, url, ee, pages, volume, editor, series, "
          <> " (ts_rank_cd(tsv, query)::float8 + similarity(search_string, $2)::float8)::float8 AS rank"
          <> " FROM "
          <> " entry, to_tsquery($1) query"
          <> " WHERE"
          <> " ( query @@ tsv "
          <> "   OR (search_string % $2 AND similarity(search_string, $2) > 0.2)"
          <> "   OR (title % $2 AND similarity(title, $2) > 0.2)"
          <> "   OR (author_list % $2 AND similarity(author_list, $2) > 0.2)"
          <> " ) AND ($3 = True OR year = ANY($4))"
          <> " ORDER BY rank DESC LIMIT 25"
      encoder =
          contramap mkQuery (E.value E.text)
          <> contramap id (E.value E.text)
          <> contramap (V.null . extractFullYears) (E.value E.bool)
          <> contramap extractFullYears (E.value (E.array (E.arrayDimension V.foldl' (E.arrayValue E.int4))))
      decoder =
          D.rowsVector $
          do e_key <- D.value D.text
             e_type <- D.value D.text >>= fromText
             e_authors <- D.value (D.array (D.arrayDimension V.replicateM (D.arrayValue D.text)))
             e_title <- maybeToOption <$> D.nullableValue D.text
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
    T.intercalate " & "
    . map (<> ":*")
    . filter (\x -> T.length x >= 2)
    . filter (T.all isAlpha)
    . T.words

extractFullYears :: T.Text -> V.Vector Int32
extractFullYears =
    V.fromList
    . filter (\x -> x > 1900 && x < 2100)
    . mapMaybe (readMay . T.unpack)
    . filter (\x -> T.length x == 4)
    . filter (T.all isNumber)
    . T.words
