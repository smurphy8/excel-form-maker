{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE EmptyDataDecls    #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
module Data.Excel.FormPopulate where
import Yesod hiding (runDB)

-- import Yesod.Default.Config (DefaultEnv (..), withYamlEnvironment)
-- import Yesod.Core (MonadIO,MonadBaseControl)
import Data.Maybe

import Database.Persist.MongoDB

import Data.Time

import Network (PortID (PortNumber))
import Database.Persist.Quasi (lowerCaseSettings)
import Control.Applicative


import Data.Text hiding (take,head,tail,zip,zipWith,concat)

import Codec.Xlsx.Parser
import Codec.Xlsx.Writer
import Codec.Xlsx.Lens
import Codec.Xlsx
import Language.Haskell.TH.Syntax


dataList :: [FullyIndexedCellValue]
dataList = [ FICV 0 16 11 (CellDouble 333.0), FICV 0 16 13 (CellDouble 332.0), FICV 0 16 13 (CellDouble 332.0), FICV 0 16 14 (CellDouble 332.0),
             FICV 0 17 11 (CellDouble 333.0), FICV 0 17 13 (CellDouble 332.0), FICV 0 17 13 (CellDouble 332.0), FICV 0 17 14 (CellDouble 332.0),
             FICV 0 18 11 (CellDouble 333.0), FICV 0 18 13 (CellDouble 332.0), FICV 0 18 13 (CellDouble 332.0), FICV 0 18 14 (CellDouble 332.0),
             FICV 0 19 11 (CellDouble 333.0), FICV 0 19 13 (CellDouble 332.0), FICV 0 19 13 (CellDouble 332.0), FICV 0 19 14 (CellDouble 332.0),
             FICV 0 20 11 (CellDouble 333.0), FICV 0 20 13 (CellDouble 332.0), FICV 0 20 13 (CellDouble 332.0), FICV 0 20 14 (CellDouble 332.0),
             FICV 0 21 11 (CellDouble 333.0), FICV 0 21 13 (CellDouble 332.0), FICV 0 21 13 (CellDouble 332.0), FICV 0 21 14 (CellDouble 332.0),
             FICV 0 22 11 (CellDouble 333.0), FICV 0 22 13 (CellDouble 332.0), FICV 0 22 13 (CellDouble 332.0), FICV 0 22 14 (CellDouble 332.0),
             FICV 0 23 11 (CellDouble 333.0), FICV 0 23 13 (CellDouble 332.0), FICV 0 23 13 (CellDouble 332.0), FICV 0 23 14 (CellDouble 332.0),
             FICV 0 24 11 (CellDouble 333.0), FICV 0 24 13 (CellDouble 332.0), FICV 0 24 13 (CellDouble 332.0), FICV 0 24 14 (CellDouble 332.0)]
           




share [mkPersist (mkPersistSettings (ConT ''MongoBackend)) { mpsGeneric = False }, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "modelsMongo")


{-===========================================================================-}
{-                                 runDB                                     -}
{-===========================================================================-}

runDB :: forall (m :: * -> *) b.(MonadIO m ,MonadBaseControl IO m) =>
               Action m b -> m b
runDB a = withMongoDBConn "onping_production" "10.84.207.130" (PortNumber 27017) Nothing 2000 $ \pool -> do 
  (runMongoDBPool master a )  pool



data TishReportData a = TRD {
      row::Int, 
      col :: Int, 
      sheet :: Int, 
      lookup :: Int,
      transform :: (a -> CellValue)
    }


-- |Temp Tish Report
tishWaterData :: [Int]
tishWaterData = [3176,3177,3163,3183,3184,3186,3187,3189,3190]


fresh :: Int
fresh = 19813

raw :: Int
raw = 19814 --3163

delta :: NominalDiffTime
delta = realToFrac (20::Integer)



testTime :: IO UTCTime
testTime = do 
   k <- getCurrentTime
   return $ UTCTime (fromGregorian  2013 10 14) (fromIntegral $ 4 * 3600)
   
oneDay :: NominalDiffTime 
oneDay = realToFrac $ 60*60*24


mkRowList bTime = do 
  mapM (\(i,newTime) -> mkTurbidityRow (i) (newTime) defaultStepList) (zipWith (\i b -> (i+8,addUTCTime ((r i)*oneDay) b)) [0 .. 30] (repeat bTime))
      where r = realToFrac

testMkRowList = do 
  z   <- testTime
  rowListList <- mkRowList z
  print rowListList

mkTurbidityRow  :: (MonadIO m, MonadBaseControl IO m) =>
     Int -> UTCTime -> [NominalDiffTime] -> m [FullyIndexedCellValue]
mkTurbidityRow rowNum baseTime stepList = do
  runDB $ do
    mraw <- selectFirst [OnpingTagHistoryTime >=. (Just baseTime),OnpingTagHistoryTime <. (Just (addUTCTime delta baseTime)), OnpingTagHistoryPid ==. (Just raw)][]
    freshMlist <-  mapM  (\s -> selectFirst (mkDataRowFilter baseTime s) [Asc OnpingTagHistoryTime] ) stepList
    let mRawFICV = (onpingTagToFICV 0 2 rowNum).entityVal <$> mraw       
        freshList = fromJust $ sequence freshMlist           
        turbidityIdx = (\(i,x) -> (onpingTagToFICV 0 i rowNum x)) <$> (zip [5, 6, 7, 8, 9, 10] (entityVal <$> freshList))
        
    return $ fromJust mRawFICV : turbidityIdx

onpingTagToFICV :: Int -> Int -> Int -> OnpingTagHistory -> FullyIndexedCellValue
onpingTagToFICV sheetNum colNum rowNum (OnpingTagHistory _ _ (Just v)) = FICV sheetNum colNum rowNum (CellDouble v)
onpingTagToFICV sheetNum colNum rowNum (OnpingTagHistory _ _ (Nothing)) = FICV sheetNum colNum rowNum (CellText "ERROR VALUE NOT FOUND")


testMkTurbidityRow :: IO () 
testMkTurbidityRow = do 
  z   <- testTime
  ans <- mkTurbidityRow 0 z defaultStepList

  print z
  print $  ans
  

mkDataRowFilter  :: UTCTime -> NominalDiffTime -> [Filter OnpingTagHistory]
mkDataRowFilter baseTime b  = let newBaseTime = addUTCTime b baseTime
                              in  getPointInDelta newBaseTime

mkTishFilter  :: UTCTime -> [NominalDiffTime] -> [Filter OnpingTagHistory]
mkTishFilter baseTime stepList= (Prelude.foldl
                         (\a b -> (mkDataRowFilter baseTime b) ||. a ) (mkDataRowFilter baseTime (head stepList)) (tail stepList ))



-- | get a point at a certain time but return a point close to it by the default delta
getPointInDelta :: UTCTime -> [Filter OnpingTagHistory]
getPointInDelta baseTime = [ OnpingTagHistoryTime >=. (Just baseTime) ,
                                      OnpingTagHistoryTime <. (Just (addUTCTime delta  baseTime)),
                                      OnpingTagHistoryPid ==. (Just fresh) ]



createForm :: IO () 
createForm = do 
  print "getting worksheet"
  x <- xlsx "BuildSheet.xlsx"  
  ws <- getWorksheets x
  z  <- testTime 
  print "making Rows"
  dList <- mkRowList z
  print "updating Spreadsheet"
  editWs <- return $ setMultiMappedSheetCellData ws (concat dList)
  writeXlsx "ptest2.xlsx" x (Just editWs)




defaultStepList :: [NominalDiffTime ]
defaultStepList = take tc $ fmap (realToFrac.(* stp)) [zero ..]
         where 
           tc :: Int 
           tc = 6
           stp :: Integer
           stp = 14400
           zero :: Integer
           zero = 0



--    


testRaw = do 
 z <- testTime
 k <-  runDB $ selectList ([OnpingTagHistoryPid ==.(Just 19813) ,OnpingTagHistoryTime >.(Just z)  ]||.[OnpingTagHistoryPid ==. (Just 19814), OnpingTagHistoryTime >. (Just z)]) [Desc OnpingTagHistoryTime , LimitTo 1000]
 print $ (onpingTagHistoryVal.entityVal) <$> k 



