{-# LANGUAGE OverloadedStrings #-}
import PG.Import

import Control.Monad.Trans.Resource
import Data.Conduit
import Data.Option
import Test.Hspec
import qualified Data.Conduit.List as CL
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

dblpMini :: [Entry]
dblpMini =
    [ Entry
      { e_key = "journals/acta/Saxena96"
      , e_authors = V.fromList ["Sanjeev Saxena"]
      , e_title = "Parallel Integer Sorting and Simulation Amongst CRCW Models."
      , e_year = Some "1996"
      , e_journal = Some "Acta Inf."
      , e_url = Some "db/journals/acta/acta33.html#Saxena96"
      , e_ee = Some "https://doi.org/10.1007/BF03036466"
      , e_pages = Some "607-619"
      , e_volume = Some "33"
      }
    , Entry
      { e_key = "journals/acta/BulychevDLL14"
      , e_authors = V.fromList ["Peter E. Bulychev","Alexandre David","Kim G. Larsen","Guangyuan Li"]
      , e_title = "Efficient controller synthesis for a fragment of MTL0,\8734."
      , e_year = Some "2014"
      , e_journal = Some "Acta Inf."
      , e_url = Some "db/journals/acta/acta51.html#BulychevDLL14"
      , e_ee = Some "https://doi.org/10.1007/s00236-013-0189-z"
      , e_pages = Some "165-192"
      , e_volume = Some "51"}
    , Entry
      {e_key = "journals/combinatorica/DudekR11"
      , e_authors = V.fromList ["Andrzej Dudek","Vojtech R\246dl"]
      , e_title = "On Ks-free subgraphs in Ks+k-free graphs and vertex Folkman numbers."
      , e_year = Some "2011"
      , e_journal = Some "Combinatorica"
      , e_url = Some "db/journals/combinatorica/combinatorica31.html#DudekR11"
      , e_ee = Some "https://doi.org/10.1007/s00493-011-2626-3"
      , e_pages = Some "39-53"
      , e_volume = Some "31"
      }
    , Entry
      { e_key = "journals/thipeac/2009-2"
      , e_authors = V.empty
      , e_title = "Transactions on High-Performance Embedded Architectures and Compilers II"
      , e_year = Some "2009"
      , e_journal = None
      , e_url = Some "db/journals/thipeac/thipeac2.html"
      , e_ee = Some "https://doi.org/10.1007/978-3-642-00904-4"
      , e_pages = None
      , e_volume = Some "5470"
      }
    ]
