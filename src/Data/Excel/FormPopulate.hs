{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE EmptyDataDecls    #-}
{-# LANGUAGE BangPatterns      #-}
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
import Debug.Trace
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


freshTurb :: Int
freshTurb = 3183

rawTurb :: Int
rawTurb = 3163

chlorine :: Int
chlorine = 3189

totalFlow :: Int 
totalFlow = 3950

delta :: NominalDiffTime
delta = realToFrac (30::Integer)



testTime :: IO UTCTime
testTime = do 
   k <- getCurrentTime
   return $ UTCTime (fromGregorian  2013 10 14) (fromIntegral $ 4 * 3600)
   
oneDay :: NominalDiffTime 
oneDay = realToFrac $ 60*60*24

mkRowList  :: (MonadIO m, MonadBaseControl IO m) => Int ->
     UTCTime -> m [[FullyIndexedCellValue]]

mkRowList bRow bTime = do 
  l1 <- fcn mkTurbidityRow
  l2 <- fcn mkChlorineRow
  return $ l1 ++ l2
      where r = realToFrac
            fcn f = mapM (\(i,newTime) -> f (i) (newTime) defaultStepList) (zipWith (\i b -> (i+bRow,addUTCTime ((r i)*oneDay) b)) [0 .. 30] (repeat bTime))


testMkRowList = do 
  z   <- testTime
  rowListList <- mkRowList 0 z
  print rowListList


-- | Turbidity Functions
mkTurbidityRow  :: (MonadIO m, MonadBaseControl IO m) =>
     Int -> UTCTime -> [NominalDiffTime] -> m [FullyIndexedCellValue]
mkTurbidityRow rowNum baseTime stepList = do
  runDB $ do
    mrawTurb <- selectFirst [OnpingTagHistoryTime >=. (Just baseTime),OnpingTagHistoryTime <. (Just (addUTCTime delta baseTime)), OnpingTagHistoryPid ==. (Just rawTurb)][]
    freshTurbMlist <-  mapM  (\s -> selectFirst (mkDataRowFilter freshTurb baseTime s) [Asc OnpingTagHistoryTime] ) stepList
    let mRawTurbFICV = (onpingTagToFICV 1 16 rowNum).entityVal <$> mrawTurb 
        freshTurbList = fromJust $ sequence freshTurbMlist
        turbidityIdx = (\(i, x) -> (onpingTagToFICV 1 i rowNum x)) <$> (zip [19..24] (entityVal <$> freshTurbList))        
    return $ fromJust mRawTurbFICV : turbidityIdx


mkChlorineRow  rowNum baseTime stepList = do
  runDB $ do
    chlorineMlist <- mapM (\s -> selectFirst (mkDataRowFilter chlorine baseTime s) [Asc OnpingTagHistoryTime])  stepList 
    let chlorineList = fromJust $ sequence chlorineMlist
        chlorineIdx = (\(i,x) -> (onpingTagToFICV 1 i rowNum x)) <$> (zip [25 .. 30] (entityVal <$> chlorineList))        
    return $ chlorineIdx


mkTotalFlowRow rowNum baseTime stepList = do 
  runDB $ do 
    (Just totalFlowList)<- selectFirst [OnpingTagHistoryTime >=. (Just baseTime),OnpingTagHistoryTime <. (Just (addUTCTime delta baseTime)), OnpingTagHistoryPid ==. (Just totalFlow)][]
    return $ [ (onpingTagToFICV 0 2  rowNum).entityVal $ totalFlowList]
        


-- mkTishFilter  :: UTCTime -> [NominalDiffTime] -> [Filter OnpingTagHistory]
-- mkTishFilter baseTime stepList= (Prelude.foldl
--                          (\a b -> (mkDataRowFilter baseTime b) ||. a ) (mkDataRowFilter baseTime (head stepList)) (tail stepList ))


onpingTagToFICV :: Int -> Int -> Int -> OnpingTagHistory -> FullyIndexedCellValue
onpingTagToFICV sheetNum colNum rowNum (OnpingTagHistory _ _ (Just v)) = FICV sheetNum colNum rowNum (CellDouble v)
onpingTagToFICV sheetNum colNum rowNum (OnpingTagHistory _ _ (Nothing)) = FICV sheetNum colNum rowNum (CellText "ERROR VALUE NOT FOUND")


testMkTurbidityRow :: IO () 
testMkTurbidityRow = do 
  z   <- testTime
  ans <- mkTurbidityRow 0 z defaultStepList
  print z
  print $  ans  


mkDataRowFilter  :: Int -> UTCTime -> NominalDiffTime -> [Filter OnpingTagHistory]
mkDataRowFilter i baseTime b  = let newBaseTime = addUTCTime b baseTime
                                in  getPointInDelta i newBaseTime


-- | get a point at a certain time but return a point close to it by the default delta
getPointInDelta :: Int -> UTCTime -> [Filter OnpingTagHistory]
getPointInDelta i baseTime = [ OnpingTagHistoryTime >=. (Just baseTime) ,
                                      OnpingTagHistoryTime <. (Just (addUTCTime delta  baseTime)),
                                      OnpingTagHistoryPid ==. (Just i) ]


createForm :: IO () 
createForm = do 
  print "getting worksheet"
  x <- xlsx "BuildSheet.xlsx"  
  ws <- getWorksheets x
  z  <- testTime 
  print "making Rows"
  dList <- mkRowList 12 z
  print "updating Spreadsheet"
  editWs <- return $ setMultiMappedSheetCellData ws (concat dList)
  print "writing Spreadsheet"
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



testRawTurb = do 
 z <- testTime
 k <-  runDB $ selectList ([OnpingTagHistoryPid ==.(Just 19813) ,OnpingTagHistoryTime >.(Just z)  ]||.[OnpingTagHistoryPid ==. (Just 19814), OnpingTagHistoryTime >. (Just z)]) [Desc OnpingTagHistoryTime , LimitTo 1000]
 print $ (onpingTagHistoryVal.entityVal) <$> k 



