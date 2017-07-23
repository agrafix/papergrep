{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
module PG.Import
    ( fromBS, fromFile
    )
where

import PG.Types

import Control.Monad
import Control.Monad.Catch
import Control.Monad.Trans.Resource
import Data.Char
import Data.Conduit
import Data.Maybe
import Data.Monoid
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
    tag (anyOf entryTags) (\n -> ((,) <$> pure n <*> requireAttr "key") <* ignoreAttrs) $
    \(ety, e_key) ->
    do e_type <-
           case ety of
             "www" -> pure EtWWW
             "phdthesis" -> pure EtPhd
             "inproceedings" -> pure EtInProc
             "incollection" -> pure EtInColl
             "proceedings" -> pure EtProc
             "book" -> pure EtBook
             "mastersthesis" -> pure EtMaster
             "article" -> pure EtArticle
             _ ->
                 throwM $
                 XmlException (T.unpack $ "Invalid entry type " <> nameLocalName ety <> " for " <> e_key)
                 Nothing
       tagKvs <-
           many $
           tag anyName (\n -> ignoreAttrs *> pure n) $ \n ->
           (,) <$> pure n <*> innerText
       let getMany x = map snd $ filter ((==) x . fst) tagKvs
           getOpt = maybeToOption . listToMaybe . getMany
           getReq x =
               case getOpt x of
                 Some y -> pure y
                 None ->
                     throwM $
                     XmlException (T.unpack $ "Missing " <> nameLocalName x <> " for " <> e_key)
                     Nothing
           e_authors = V.fromList $ getMany "author"
       e_title <- getReq "title"
       let e_pages = getOpt "pages"
           e_year = getOpt "year"
           e_volume = getOpt "volume"
           e_journal = getOpt "journal"
           e_ee = getOpt "ee"
           e_url = getOpt "url"
           e_editor = getOpt "editor"
           e_series = getOpt "series"
       pure Entry {..}
    where
      entryTags =
          [ "www", "phdthesis", "inproceedings", "incollection"
          , "proceedings", "book", "mastersthesis", "article"
          ]

innerText :: MonadThrow m => ConduitM Event o m T.Text
innerText =
    loop [] ""
    where
        loop !stack !out =
            do (x, _) <- dropWS []
               case x of
                 Nothing -> pure out
                 Just (EventContent (ContentText t)) -> loop stack (out <> t)
                 Just (EventBeginElement el _) -> loop (el : stack) out
                 Just (EventEndElement el) ->
                     case stack of
                       (expectedEl : rest)
                           | expectedEl == el -> loop rest out
                           | otherwise -> throwM (InvalidEndElement el x)
                       [] ->
                           do leftover (EventEndElement el)
                              pure out
                 Just _ -> loop stack out
        dropWS leftovers =
            do x <- await
               let leftovers' = maybe id (:) x leftovers
               case isWhitespace <$> x of
                 Just True -> dropWS leftovers'
                 _ -> return (x, leftovers')


isWhitespace :: Event -> Bool
isWhitespace EventBeginDocument = True
isWhitespace EventEndDocument = True
isWhitespace EventBeginDoctype{} = True
isWhitespace EventEndDoctype = True
isWhitespace EventInstruction{} = True
isWhitespace (EventContent (ContentText t)) = T.all isSpace t
isWhitespace EventComment{} = True
isWhitespace (EventCDATA t) = T.all isSpace t
isWhitespace _ = False
