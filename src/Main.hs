{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes,
  TemplateHaskell, MultiParamTypeClasses, GADTs, FlexibleContexts,RankNTypes
, EmptyDataDecls #-}

import Yesod.Core (MonadIO,MonadBaseControl)

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
main = createForm -- runDB $ selectList ([OnpingTagHistoryPid ==.(Just 19813) ]||.[OnpingTagHistoryPid ==. (Just 19814)]) [Desc OnpingTagHistoryTime, LimitTo 100]


