{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module PG.Search
    ( withSearchEngine, Search
    , searchEntry, RankedEntry(..), SearchResults(..)
    )
where

import PG.Types

import Control.Monad
import Control.Monad.Catch
import Control.Monad.Trans
import Data.Conduit
import Data.Ix
import Data.Maybe
import Data.Option
import Data.SearchEngine hiding (query)
import NLP.Snowball (stems, Algorithm(English))
import NLP.Tokenize.Text (tokenize)
import System.FilePath
import System.IO.Temp
import qualified Data.Store as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Traversable as T
import qualified Database.LevelDB.Base as L

data SearchField
    = SfKey
    | SfAuthors
    | SfTitle
    | SfYear
    | SfJournal
    | SfUrl
    | SfEE
    | SfPages
    | SfVolume
    | SfEditor
    | SfSeries
    deriving (Ord, Eq, Ix, Bounded)

data Search
    = Search
    { d_search :: !(SearchEngine Entry T.Text SearchField NoFeatures)
    , d_handle :: !L.DB
    }

withSearchEngine :: (MonadIO m, MonadMask m) => Source m Entry -> (Search -> m a) -> m a
withSearchEngine src go =
    withSystemTempDirectory "pgsearch" $ \searchDir ->
    L.withDB (searchDir </> "dbase.db") (L.defaultOptions { L.createIfMissing = True }) $ \dbHdl ->
    do se <- src $$ sink dbHdl
       go (Search se dbHdl)
    where
      sink hdl =
          loop hdl (initSearchEngine searchConfig rankParams)
      loop hdl !s =
          do x <- await
             case x of
               Nothing -> pure s
               Just e ->
                   do let k = T.encodeUtf8 $ e_key e
                          v = S.encode e
                      L.put hdl L.defaultWriteOptions k v
                      let addIndex = insertDocs [e] s
                          noIndex = s
                      loop hdl $
                          case e_type e of
                            EtWWW -> noIndex
                            EtMaster -> noIndex
                            EtBook -> noIndex
                            _ -> addIndex
      searchConfig =
          SearchConfig
          { documentKey = e_key
          , extractDocumentTerms = extractTerms
          , transformQueryTerm = const
          , documentFeatureValue = const noFeatures
          }
      rankParams =
          SearchRankParameters
          { paramK1 = 1.5
          , paramB = const 0.75
          , paramFieldWeights =
              \f ->
                  case f of
                    SfKey -> 1.0
                    SfAuthors -> 10.0
                    SfEditor -> 8.0
                    SfTitle -> 10.0
                    SfYear -> 5.0
                    SfJournal -> 8.0
                    SfSeries -> 8.0
                    SfUrl -> 1.0
                    SfEE -> 1.0
                    SfPages -> 0.1
                    SfVolume -> 0.1
          , paramFeatureWeights = noFeatures
          , paramFeatureFunctions = noFeatures
          , paramResultsetSoftLimit = 2000
          , paramResultsetHardLimit = 2000
          , paramAutosuggestPrefilterLimit = 2000
          , paramAutosuggestPostfilterLimit = 2000
          }
      extractTerms e f =
          case f of
            SfKey -> [e_key e]
            SfAuthors -> concatMap handleAuthor (e_authors e)
            SfEditor -> concatMap handleAuthor $ maybeToList $ optionToMaybe (e_editor e)
            SfSeries -> optMiscField (e_series e)
            SfTitle -> handleTitle (e_title e)
            SfYear -> optMiscField (e_year e)
            SfJournal -> optMiscField (e_journal e)
            SfUrl -> []
            SfEE -> []
            SfPages -> []
            SfVolume -> []

handleAuthor :: T.Text -> [Term]
handleAuthor = map T.toLower . filter ((>= 2) . T.length) . tokenize

handleTitle :: T.Text -> [Term]
handleTitle = map T.toLower . stems English . tokenize

optMiscField :: Option T.Text -> [Term]
optMiscField o =
    case o of
      None -> []
      Some x -> [T.strip $ T.toLower x]

data RankedEntry e
    = RankedEntry
    { re_rank :: Float
    , re_value :: e
    } deriving (Show, Eq)

data SearchResults
   = SearchResults
   { sr_completions :: [RankedEntry T.Text]
   , sr_entries :: [RankedEntry Entry]
   } deriving (Show, Eq)

searchEntry :: MonadIO m => Search -> T.Text -> m SearchResults
searchEntry s query =
    do let queryWords =
               map T.toLower $ stems English $ tokenize query
           (lastWord, full) =
               case reverse queryWords of
                 [] -> ("", [])
                 (a : b) -> (a, b)
           (completions, results) =
               queryAutosuggest (d_search s) NoFilter full lastWord

       entries <-
           forM results $ \(key, score) ->
           do val <-
                  liftIO $
                  do bs <- L.get (d_handle s) L.defaultReadOptions (T.encodeUtf8 key)
                     T.mapM S.decodeIO bs
              pure $ RankedEntry score <$> val
       let compls =
               map (\(term, score) -> RankedEntry score term) completions
       pure (SearchResults compls $ catMaybes entries)
