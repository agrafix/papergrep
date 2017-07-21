{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module PG.Import
    ( Entry(..)
    , fromBS, fromFile
    )
where

import Control.Error
import Data.Functor.Identity
import Data.Option
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V
import qualified Text.XML.Hexml as Xml

data Entry
    = Entry
    { e_key :: {-# UNPACK #-} !T.Text
    , e_authors :: {-# UNPACK #-} !(V.Vector T.Text)
    , e_title :: {-# UNPACK #-} !T.Text
    , e_year :: {-# UNPACK #-} !(Option T.Text)
    , e_journal :: {-# UNPACK #-} !(Option T.Text)
    , e_url :: {-# UNPACK #-} !(Option T.Text)
    , e_ee :: {-# UNPACK #-} !(Option T.Text)
    , e_pages :: {-# UNPACK #-}  !(Option T.Text)
    , e_volume :: {-# UNPACK #-} !(Option T.Text)
    } deriving (Show, Eq)

fromFile :: FilePath -> IO (Either BS.ByteString (V.Vector Entry))
fromFile fp =
    do bs <- BS.readFile fp
       pure (fromBS bs)

fromBS :: BS.ByteString -> Either BS.ByteString (V.Vector Entry)
fromBS bs =
    runIdentity $ runExceptT $
    do node <- ExceptT $ pure (Xml.parse $ BSC.unlines $ drop 2 $ BSC.lines bs)
       let ch = Xml.children node
       es <- V.fromList <$> mapM parseEntry ch
       pure es

parseEntry :: Monad m => Xml.Node -> ExceptT BS.ByteString m Entry
parseEntry n =
    do e_key <-
           T.decodeLatin1 . Xml.attributeValue <$>
           Xml.attributeBy n "key" ?? "Missing key attribute in node"
       let e_authors =
               V.fromList $ map decodeTag $ Xml.childrenBy n "author"
       e_title <-
           decodeTag <$> (headMay (Xml.childrenBy n "title") ?? "Missing title")
       let e_year = optSingleFld "year"
           e_journal = optSingleFld "journal"
           e_url = optSingleFld "url"
           e_ee = optSingleFld "ee"
           e_pages = optSingleFld "pages"
           e_volume = optSingleFld "volume"
       pure Entry {..}
    where
        decodeTag = T.decodeLatin1 . Xml.inner
        optSingleFld x =
            maybeToOption $
            decodeTag <$> headMay (Xml.childrenBy n x)
