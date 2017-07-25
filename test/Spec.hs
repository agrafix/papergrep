{-# LANGUAGE OverloadedStrings #-}
import PG.Import
import PG.Store
import PG.Types
import PG.Xml

import Control.Logger.Simple
import Control.Monad.Trans.Resource
import Data.Conduit
import Data.List
import Data.Option
import Test.Hspec
import qualified Data.Conduit.List as CL
import qualified Data.Text as T
import qualified Data.Vector as V

main :: IO ()
main =
    withGlobalLogging (LogConfig Nothing True) $
    hspec $
    do describe "parser" $
           it "parses dblp-mini.xml" $
           do res <-
                  runResourceT $
                  fromFile "test-data/dblp-mini.xml" $$ CL.consume
              res `shouldBe` dblpMini
       describe "search engine" $
           do it "finds author names" $
                  withTempStore $ \store ->
                  do importToStore store (fromFile "test-data/dblp-mini.xml")
                     assertSearch store "sanjeev" sanjeev
                     assertSearch store "sanje" sanjeev
                     assertSearch store "s. saxena" sanjeev
                     assertSearch store "saxena, sanjeev" sanjeev
                     assertSearchRank store "saxena, sanjeev" (\r -> r > 0.5)
                     assertSearchNoRes store "johannes thiemann"
                     assertSearchNoRes store "johannes"
                     assertSearchNoRes store "thiemann"
                     assertSearchNoRes store "johannes thiemann"
              it "finds by title" $
                  withTempStore $ \store ->
                  do importToStore store (fromFile "test-data/dblp-mini.xml")
                     assertSearch store "parallel integer sorting" sanjeev
                     assertSearch store "CRCW Models" sanjeev
                     assertSearch store "simulation models" sanjeev
                     assertSearch store "ks-free subgraphs folkman" dudek
                     assertSearch store "ks-free subgraphs, folkman" dudek
                     assertSearch store "ks-free subgraphs: folkman" dudek
              it "title and author" $
                  withTempStore $ \store ->
                  do importToStore store (fromFile "test-data/dblp-mini.xml")
                     assertSearch store "simulation models sanjeev" sanjeev
              it "title and author with noise" $
                  withTempStore $ \store ->
                  do importToStore store (fromFile "test-data/dblp-mini.xml")
                     assertSearch store "simulation models sanjeev foo bar baz" sanjeev
              it "understands years" $
                  withTempStore $ \store ->
                  do importToStore store (fromFile "test-data/dblp-mini.xml")
                     assertSearch store "sanjeev 1996" sanjeev
                     assertSearchNoRes store "sanjeev 2010"

assertSearchRank :: Store -> T.Text -> (Double -> Bool) -> IO ()
assertSearchRank store q c =
     do sr <- searchEntry store q
        case re_rank <$> sortOn re_rank (V.toList sr) of
            [] -> expectationFailure "No results"
            (r : _) -> r `shouldSatisfy` c

assertSearch :: Store -> T.Text -> Entry -> IO ()
assertSearch store q e =
     do sr <- searchEntry store q
        let es = re_entry <$> sortOn re_rank (V.toList sr)
        es `shouldBe` [e]

assertSearchNoRes :: Store -> T.Text -> IO ()
assertSearchNoRes store q =
    searchEntry store q `shouldReturn` V.empty


sanjeev :: Entry
sanjeev =
    Entry
    { e_key = "journals/acta/Saxena96"
    , e_type = EtArticle
    , e_authors = V.fromList ["Sanjeev Saxena"]
    , e_title = Some "Parallel Integer Sorting and Simulation Amongst CRCW Models."
    , e_year = Some 1996
    , e_journal = Some "Acta Inf."
    , e_url = Some "db/journals/acta/acta33.html#Saxena96"
    , e_ee = Some "https://doi.org/10.1007/BF03036466"
    , e_pages = Some "607-619"
    , e_volume = Some "33"
    , e_editor = None
    , e_series = None
    }

dudek :: Entry
dudek =
    Entry
    { e_key = "journals/combinatorica/DudekR11"
    , e_type = EtArticle
    , e_authors = V.fromList ["Andrzej Dudek","Vojtech R\246dl"]
    , e_title = Some "On Ks-free subgraphs in Ks+k-free graphs and vertex Folkman numbers."
    , e_year = Some 2011
    , e_journal = Some "Combinatorica"
    , e_url = Some "db/journals/combinatorica/combinatorica31.html#DudekR11"
    , e_ee = Some "https://doi.org/10.1007/s00493-011-2626-3"
    , e_pages = Some "39-53"
    , e_volume = Some "31"
    , e_editor = None
    , e_series = None
    }

dblpMini :: [Entry]
dblpMini =
    [ sanjeev
    , Entry
      { e_key = "journals/acta/BulychevDLL14"
      , e_type = EtArticle
      , e_authors = V.fromList ["Peter E. Bulychev","Alexandre David","Kim G. Larsen","Guangyuan Li"]
      , e_title = Some "Efficient controller synthesis for a fragment of MTL0,\8734."
      , e_year = Some 2014
      , e_journal = Some "Acta Inf."
      , e_url = Some "db/journals/acta/acta51.html#BulychevDLL14"
      , e_ee = Some "https://doi.org/10.1007/s00236-013-0189-z"
      , e_pages = Some "165-192"
      , e_volume = Some "51"
      , e_editor = None
      , e_series = None
      }
    , dudek
    , Entry
      { e_key = "journals/thipeac/2009-2"
      , e_type = EtProc
      , e_authors = V.empty
      , e_title = Some "Transactions on High-Performance Embedded Architectures and Compilers II"
      , e_year = Some 2009
      , e_journal = None
      , e_url = Some "db/journals/thipeac/thipeac2.html"
      , e_ee = Some "https://doi.org/10.1007/978-3-642-00904-4"
      , e_pages = None
      , e_volume = Some "5470"
      , e_editor = Some "Per Stenstr\246m"
      , e_series = Some "Lecture Notes in Computer Science"
      }
    , Entry
      { e_key = "homepages/96/2735"
      , e_type = EtWWW
      , e_authors = V.empty
      , e_title = None
      , e_year = None
      , e_journal = None
      , e_url = None
      , e_ee = None
      , e_pages = None
      , e_volume = None
      , e_editor = None
      , e_series = None
      }
    ]
