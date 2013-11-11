{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes,
  TemplateHaskell, MultiParamTypeClasses, GADTs, FlexibleContexts
, EmptyDataDecls #-}

import Data.Text (Text, pack)
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
main = withMongoDBConn "onping_production" "localhost" (PortNumber 27017) Nothing 2000 $ \pool -> do 
  (runMongoDBPool master (selectList [] [Asc QuestionnaireId]))  pool
-- import 
    -- dbconf <- withYamlEnvironment mongoConfFile Devxelopment
    --             Database.Persist.loadConfig >>=
    --           Database.Persist.applyEnv
    -- pool <- Database.Persist.createPoolConfig (dbconfp)

--    warp 3000 $ App pool dbconf
--    where mongoConfFile = "./mongoDB-nonscaffold.yml" 
