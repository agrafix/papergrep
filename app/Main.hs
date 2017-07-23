{-# LANGUAGE OverloadedStrings #-}
module Main where

import PG.Import
import PG.Search

import Control.Monad.Trans
import Control.Monad.Trans.Resource

main :: IO ()
main =
    runResourceT $
    withSearchEngine (fromFile "dblp.xml") $ \s ->
    do res <- searchEntry s "Peter Thiemann"
       liftIO $ print res
