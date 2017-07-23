{-# LANGUAGE OverloadedStrings #-}
import PG.Import
import PG.Search
import PG.Types

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
    hspec $
    do describe "parser" $
           it "parses dblp-mini.xml" $
           do res <-
                  runResourceT $
                  fromFile "test-data/dblp-mini.xml" $$ CL.consume
              res `shouldBe` dblpMini
       describe "search engine" $
           do it "finds author names" $
                  do assertSearch "sanjeev"
                     assertSearch "sanje"
                     assertSearch "s. saxena"
                     assertSearch "saxena, sanjeev"
              it "finds by title" $
                  do assertSearch "parallel integer sorting"
                     assertSearch "CRCW Models"
                     assertSearch "simulation models"
              it "title and author" $
                  do assertSearch "simulation models sanjeev"
--              it "title and author with noise" $
--                  do assertSearch "simulation models sanjeev foo bar baz"

assertSearch :: T.Text -> IO ()
assertSearch q =
     do sr <-
            runResourceT $
            withSearchEngine (fromFile "test-data/dblp-mini.xml") $ \se ->
            searchEntry se q
        let es = re_value <$> sortOn re_rank (sr_entries sr)
        es `shouldBe` [sanjeev]

sanjeev :: Entry
sanjeev =
    Entry
    { e_key = "journals/acta/Saxena96"
    , e_type = EtArticle
    , e_authors = V.fromList ["Sanjeev Saxena"]
    , e_title = "Parallel Integer Sorting and Simulation Amongst CRCW Models."
    , e_year = Some "1996"
    , e_journal = Some "Acta Inf."
    , e_url = Some "db/journals/acta/acta33.html#Saxena96"
    , e_ee = Some "https://doi.org/10.1007/BF03036466"
    , e_pages = Some "607-619"
    , e_volume = Some "33"
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
      , e_title = "Efficient controller synthesis for a fragment of MTL0,\8734."
      , e_year = Some "2014"
      , e_journal = Some "Acta Inf."
      , e_url = Some "db/journals/acta/acta51.html#BulychevDLL14"
      , e_ee = Some "https://doi.org/10.1007/s00236-013-0189-z"
      , e_pages = Some "165-192"
      , e_volume = Some "51"
      , e_editor = None
      , e_series = None
      }
    , Entry
      { e_key = "journals/combinatorica/DudekR11"
      , e_type = EtArticle
      , e_authors = V.fromList ["Andrzej Dudek","Vojtech R\246dl"]
      , e_title = "On Ks-free subgraphs in Ks+k-free graphs and vertex Folkman numbers."
      , e_year = Some "2011"
      , e_journal = Some "Combinatorica"
      , e_url = Some "db/journals/combinatorica/combinatorica31.html#DudekR11"
      , e_ee = Some "https://doi.org/10.1007/s00493-011-2626-3"
      , e_pages = Some "39-53"
      , e_volume = Some "31"
      , e_editor = None
      , e_series = None
      }
    , Entry
      { e_key = "journals/thipeac/2009-2"
      , e_type = EtProc
      , e_authors = V.empty
      , e_title = "Transactions on High-Performance Embedded Architectures and Compilers II"
      , e_year = Some "2009"
      , e_journal = None
      , e_url = Some "db/journals/thipeac/thipeac2.html"
      , e_ee = Some "https://doi.org/10.1007/978-3-642-00904-4"
      , e_pages = None
      , e_volume = Some "5470"
      , e_editor = Some "Per Stenstr\246m"
      , e_series = Some "Lecture Notes in Computer Science"
      }
    ]
