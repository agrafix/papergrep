{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module PG.Api where

import Servant.API
import SuperRecord
import qualified Data.Text as T
import qualified Data.Vector as V

type SearchEntry
    = Record
    '[ "key" := T.Text
     , "ty" := T.Text
     , "authors" := V.Vector T.Text
     , "title" := Maybe T.Text
     , "year" := Maybe Int
     , "journal" := Maybe T.Text
     , "url" := Maybe T.Text
     , "ee" := Maybe T.Text
     , "pages" := Maybe T.Text
     , "volume" := Maybe T.Text
     , "editor" := Maybe T.Text
     , "series" := Maybe T.Text
     ]

type SearchResult
    = Record
    '[ "rank" := Double
     , "entry" := SearchEntry
     ]

type SearchResults
    = Record
    '[ "results" := V.Vector SearchResult
     ]

type PaperGrepApi
    = "search" :> QueryParam "q" T.Text :> Get '[JSON] SearchResults
    :<|> "get" :> QueryParam "id" T.Text :> Get '[JSON] SearchEntry
