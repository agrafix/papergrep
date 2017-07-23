{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
module PG.Types where

import Data.Option
import Data.Store (Store)
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
    deriving (Show, Eq, Generic)

instance Store EntryType

data Entry
    = Entry
    { e_key :: {-# UNPACK #-} !T.Text
    , e_type :: !EntryType
    , e_authors :: {-# UNPACK #-} !(V.Vector T.Text)
    , e_title :: {-# UNPACK #-} !T.Text
    , e_year :: !(Option T.Text)
    , e_journal :: !(Option T.Text)
    , e_url :: !(Option T.Text)
    , e_ee :: !(Option T.Text)
    , e_pages :: !(Option T.Text)
    , e_volume :: !(Option T.Text)
    , e_editor :: !(Option T.Text)
    , e_series :: !(Option T.Text)
    } deriving (Show, Eq, Generic)

instance Store Entry

instance Store a => Store (Option a)
