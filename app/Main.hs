{-# LANGUAGE OverloadedStrings #-}
module Main where

import PG.Import
import PG.Store
import PG.Xml

import Control.Logger.Simple
import System.Environment
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text as T

main :: IO ()
main =
    withGlobalLogging (LogConfig Nothing True) $
    do args <- getArgs
       case args of
         ["import", file, dbSpec] ->
             do store <- newPgSqlStore (BSC.pack dbSpec)
                importToStore store (fromFile file)
         ["search", dbSpec, query] ->
             do store <- newPgSqlStore (BSC.pack dbSpec)
                res <- searchEntry store (T.pack query)
                print res
         _ ->
             putStrLn "Bad usage"
