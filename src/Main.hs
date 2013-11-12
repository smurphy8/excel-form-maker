{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes,
  TemplateHaskell, MultiParamTypeClasses, GADTs, FlexibleContexts,RankNTypes
, EmptyDataDecls #-}


import Yesod.Core (MonadIO,MonadBaseControl,LiftIO)
import Data.Text (Text, pack)


--import Control.Monad.Trans.Control


import Database.Persist
import Database.Persist.MongoDB
import Database.Persist.Quasi (lowerCaseSettings)

import Database.Persist.TH
import Language.Haskell.TH.Syntax
import Control.Applicative ((<$>), (<*>), liftA2, Applicative)
import Data.Excel.FormPopulate



--   connPool <- createMongoDBPipePool "localhost" 27017 Nothing 100 100 1000 bal 
main = runDB $ selectList [] [Asc OnpingTagHistoryId]


