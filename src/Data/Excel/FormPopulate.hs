{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE EmptyDataDecls    #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
module Data.Excel.FormPopulate where
import Yesod
import qualified Database.Persist
import Yesod.Default.Config (DefaultEnv (..), withYamlEnvironment)
import Yesod.Core (MonadIO,MonadBaseControl)
import Database.Persist 
import Database.Persist.MongoDB
import Database.Persist.TH
import Data.Time
import Network (PortID (PortNumber))
import Database.Persist.Quasi (lowerCaseSettings)
import Data.Excel.FormPopulate.Internal
import Data.IntMap
import Data.Text
import Codec.Xlsx.Parser
import Codec.Xlsx.Writer
import Codec.Xlsx.Lens
import Codec.Xlsx
import Language.Haskell.TH.Syntax



dataList = [ FICV 0 16 11 (CellDouble 333.0), FICV 0 16 13 (CellDouble 332.0), FICV 0 16 13 (CellDouble 332.0), FICV 0 16 14 (CellDouble 332.0),
             FICV 0 17 11 (CellDouble 333.0), FICV 0 17 13 (CellDouble 332.0), FICV 0 17 13 (CellDouble 332.0), FICV 0 17 14 (CellDouble 332.0),
             FICV 0 18 11 (CellDouble 333.0), FICV 0 18 13 (CellDouble 332.0), FICV 0 18 13 (CellDouble 332.0), FICV 0 18 14 (CellDouble 332.0),
             FICV 0 19 11 (CellDouble 333.0), FICV 0 19 13 (CellDouble 332.0), FICV 0 19 13 (CellDouble 332.0), FICV 0 19 14 (CellDouble 332.0),
             FICV 0 20 11 (CellDouble 333.0), FICV 0 20 13 (CellDouble 332.0), FICV 0 20 13 (CellDouble 332.0), FICV 0 20 14 (CellDouble 332.0),
             FICV 0 21 11 (CellDouble 333.0), FICV 0 21 13 (CellDouble 332.0), FICV 0 21 13 (CellDouble 332.0), FICV 0 21 14 (CellDouble 332.0),
             FICV 0 22 11 (CellDouble 333.0), FICV 0 22 13 (CellDouble 332.0), FICV 0 22 13 (CellDouble 332.0), FICV 0 22 14 (CellDouble 332.0),
             FICV 0 23 11 (CellDouble 333.0), FICV 0 23 13 (CellDouble 332.0), FICV 0 23 13 (CellDouble 332.0), FICV 0 23 14 (CellDouble 332.0),
             FICV 0 24 11 (CellDouble 333.0), FICV 0 24 13 (CellDouble 332.0), FICV 0 24 13 (CellDouble 332.0), FICV 0 24 14 (CellDouble 332.0)]
           








createForm :: IO () 
createForm = do 
  x <- xlsx "BuildSheet.xlsx"
  ws <- getWorksheets x
  editWs <- return $ setMultiMappedSheetCellData ws dataList
  writeXlsx "ptest2.xlsx" x (Just editWs)


share [mkPersist (mkPersistSettings (ConT ''MongoBackend)) { mpsGeneric = False }, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "modelsMongo")








{-===========================================================================-}
{-                                 runDB                                     -}
{-===========================================================================-}

runDB :: forall (m :: * -> *) b.(MonadIO m ,MonadBaseControl IO m) =>
               Action m b -> m b

runDB a = withMongoDBConn "onping_production" "localhost" (PortNumber 27017) Nothing 2000 $ \pool -> do 
  (runMongoDBPool master a )  pool



















