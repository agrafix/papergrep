{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module PG.Import
    ( Entry(..)
    , fromBS, fromFile
    )
where

import PG.Types

import Control.Monad
import Control.Monad.Catch
import Control.Monad.Trans.Resource
import Data.Conduit
import Data.Option
import Data.XML.Types
import Text.XML.Stream.Parse
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Vector as V

fromFile :: MonadResource m => FilePath -> Source m Entry
fromFile fp = parseFile opts fp =$= parseEntries

fromBS :: MonadThrow m => BS.ByteString -> Source m Entry
fromBS bs = yield bs =$ parseBytes opts =$= parseEntries

opts :: ParseSettings
opts =
    def
    { psDecodeEntities = decodeHtmlEntities
    }

parseEntries :: MonadThrow m => Conduit Event m Entry
parseEntries =
    void $ tagNoAttr "dblp" $ manyYield parseEntry

parseEntry :: MonadThrow m => Consumer Event m (Maybe Entry)
parseEntry =
    tag' (anyOf entryTags) (requireAttr "key" <* ignoreAttrs) $ \e_key ->
    do e_authors <- V.fromList <$> many (tagIgnoreAttrs "author" content)
       e_title <-
           tagIgnoreAttrs "title" titleParser >>= \t ->
           case t of
             Nothing -> throwM (XmlException "Missing title tag" Nothing)
             Just ok -> pure $ T.concat ok
       e_pages <- maybeToOption <$> tagIgnoreAttrs "pages" content
       e_year <- maybeToOption <$> tagIgnoreAttrs "year" content
       e_volume <- maybeToOption <$> tagIgnoreAttrs "volume" content
       e_journal <- maybeToOption <$> tagIgnoreAttrs "journal" content
       e_url <- maybeToOption <$> tagIgnoreAttrs "url" content
       e_ee <- maybeToOption <$> tagIgnoreAttrs "ee" content
       many_ ignoreAnyTreeContent
       pure Entry {..}
    where
      entryTags =
          [ "www", "phdthesis", "inproceedings", "incollection"
          , "proceedings", "book", "mastersthesis", "article"
          ]
      textTag x =
          tagIgnoreAttrs x content
      titleParser =
          many $
          contentMaybe
              `orE` textTag "i"
              `orE` textTag "sub"
              `orE` textTag "sup"
