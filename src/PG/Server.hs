{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
module PG.Server
    ( launch )
where

import Data.Option
import PG.Api
import PG.Store
import PG.Types

import Control.Monad.Reader
import Data.Monoid
import Data.Text.ToFromText
import Data.Typeable
import Network.Wai.Handler.Warp
import Servant
import Servant.JS (jsForAPI, jqueryWith, defCommonGeneratorOptions, CommonGeneratorOptions(..))
import SuperRecord
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V
import qualified Network.HTTP.Media as M
import qualified Text.Jasmine as JS

type Env
    = Record
    '[ "store" := Store
     ]

type EndpointM = ReaderT Env Handler

epToHandler' :: Env -> EndpointM a -> Handler a
epToHandler' se r = runReaderT r se

epToHandler :: Env -> EndpointM :~> Handler
epToHandler se = Nat (epToHandler' se)

type FullAPI =
    "api" :> PaperGrepApi
    :<|> "sdk" :> "js" :> "api.js" :> Get '[JsCode] T.Text
    :<|> "static" :> Raw

data JsCode deriving Typeable

instance Accept JsCode where
    contentType _ = "application" M.// "javascript"

instance MimeRender JsCode T.Text where
    mimeRender _ = JS.minify . BSL.fromStrict . T.encodeUtf8

jsApiBindings :: T.Text
jsApiBindings =
    let bdn =
            jsForAPI @PaperGrepApi Proxy $ jqueryWith
            defCommonGeneratorOptions
            { moduleName = "PGApi"
            , successCallback = "cont"
            , errorCallback = "errorHandler"
            }
    in "\"use strict\"; \n var PGApi= {};\n" <> bdn

server :: Env -> Server FullAPI
server se =
    enter (epToHandler se) backendApi
    :<|> pure jsApiBindings
    :<|> serveDirectory "_frontend"

fullAPI :: Proxy FullAPI
fullAPI = Proxy

makeApp :: Env -> Application
makeApp se = serve fullAPI (server se)

launch :: Int -> Store -> IO ()
launch port store =
    do let env =
               #store := store
               & rnil
       run port (makeApp env)

backendApi ::
    ( MonadReader (Rec env) m
    , Has "store" env Store
    , MonadIO m
    ) => ServerT PaperGrepApi m
backendApi =
    searchApi

searchApi ::
    ( MonadReader (Rec env) m
    , Has "store" env Store
    , MonadIO m
    )
    => Maybe T.Text -> m SearchResults
searchApi searchQuery =
    do s <- asksR #store
       case searchQuery of
         Nothing -> pure (#results := V.empty & rnil)
         Just sq ->
             do res <- liftIO (searchEntry s sq)
                pure (#results := V.map conv res & rnil)
    where
        conv re =
            #rank := re_rank re
            & #entry :=
                ( let e = re_entry re
                  in #key := e_key e
                     & #ty := toText (e_type e)
                     & #authors := e_authors e
                     & #title := optionToMaybe (e_title e)
                     & #year := optionToMaybe (e_year e)
                     & #journal := optionToMaybe (e_journal e)
                     & #url := optionToMaybe (e_url e)
                     & #ee := optionToMaybe (e_ee e)
                     & #pages := optionToMaybe (e_pages e)
                     & #volume := optionToMaybe (e_volume e)
                     & #editor := optionToMaybe (e_editor e)
                     & #series := optionToMaybe (e_series e)
                     & rnil
                )
            & rnil
