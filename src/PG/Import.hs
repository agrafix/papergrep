{-# LANGUAGE FlexibleContexts #-}
module PG.Import
    ( importToStore )
where

import PG.Store
import PG.Types

import Control.Monad.Trans
import Control.Monad.Trans.Resource
import Data.Conduit

importToStore :: (MonadIO m, MonadBaseControl IO m) => Store -> Source (ResourceT m) Entry -> m ()
importToStore store src =
    runResourceT $ src $$ sink
    where
      sink =
          do x <- await
             case x of
               Nothing -> pure ()
               Just e ->
                   do liftIO (putEntry store e)
                      sink
