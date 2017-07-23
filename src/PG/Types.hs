{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module PG.Types where

import Data.Option
import Data.Text.ToFromText
import GHC.Generics
import qualified Data.Text as T
import qualified Data.Vector as V

data EntryType
    = EtWWW
    | EtPhd
    | EtInProc
    | EtInColl
    | EtProc
    | EtBook
    | EtMaster
    | EtArticle
    deriving (Show, Eq, Bounded, Enum, Generic)

instance ToFromText EntryType where
    toText et =
        case et of
          EtWWW -> "www"
          EtPhd -> "phdthesis"
          EtInProc -> "inproceedings"
          EtInColl -> "incollection"
          EtProc -> "proceedings"
          EtBook -> "book"
          EtMaster -> "mastersthesis"
          EtArticle -> "article"

data RankedEntry
    = RankedEntry
    { re_rank :: !Double
    , re_entry :: !Entry
    } deriving (Show, Eq, Generic)

data Entry
    = Entry
    { e_key :: {-# UNPACK #-} !T.Text
    , e_type :: !EntryType
    , e_authors :: {-# UNPACK #-} !(V.Vector T.Text)
    , e_title :: {-# UNPACK #-} !T.Text
    , e_year :: !(Option Int)
    , e_journal :: !(Option T.Text)
    , e_url :: !(Option T.Text)
    , e_ee :: !(Option T.Text)
    , e_pages :: !(Option T.Text)
    , e_volume :: !(Option T.Text)
    , e_editor :: !(Option T.Text)
    , e_series :: !(Option T.Text)
    } deriving (Show, Eq, Generic)
