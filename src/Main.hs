{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes,
  TemplateHaskell, MultiParamTypeClasses, GADTs, FlexibleContexts
, EmptyDataDecls #-}

-- import Yesod

import Data.Text (Text, pack)
import Database.Persist (runPool, createPoolConfig, loadConfig,applyEnv)
import Database.Persist.MongoDB (MongoConf (..), Action, ConnectionPool (..), MongoBackend)
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


main = do
    -- dbconf <- withYamlEnvironment mongoConfFile Development
    --             Database.Persist.loadConfig >>=
    --           Database.Persist.applyEnv
    -- pool <- Database.Persist.createPoolConfig (dbconf)
    return () 
--    warp 3000 $ App pool dbconf
--    where mongoConfFile = "./mongoDB-nonscaffold.yml" 
