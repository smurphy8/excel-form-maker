{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes,
  TemplateHaskell, MultiParamTypeClasses, GADTs, FlexibleContexts,RankNTypes
, EmptyDataDecls #-}
import Yesod.Core (MonadIO,MonadBaseControl)
import Data.Text (Text, pack)
--import Control.Monad.Trans.Control
import Database.Persist
import Database.Persist.MongoDB
import Network (PortID (PortNumber))
import Database.Persist.TH
import Language.Haskell.TH.Syntax
import Control.Applicative ((<$>), (<*>), liftA2, Applicative)





share [mkPersist (mkPersistSettings (ConT ''MongoBackend)) { mpsGeneric = False }, mkMigrate "migrateAll"][persistLowerCase|
Questionnaire
  desc Text Maybe
  questions [Question]
  deriving Show Eq Read 
Question
  formulation Text
  deriving Show Eq Read 
|]

{-===========================================================================-}
{-                                 MAIN                                      -}
{-===========================================================================-}




--   connPool <- createMongoDBPipePool "localhost" 27017 Nothing 100 100 1000 
main = runDB $ selectList [] [Asc QuestionnaireId]



{-===========================================================================-}
{-                                 runDB                                     -}
{-===========================================================================-}

runDB :: forall (m :: * -> *) b.(MonadIO m ,MonadBaseControl IO m) =>
               Action m b -> m b

runDB a = withMongoDBConn "onping_production" "localhost" (PortNumber 27017) Nothing 2000 $ \pool -> do 
  (runMongoDBPool master a )  pool


