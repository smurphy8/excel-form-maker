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

import Debug.Trace
import Data.List (foldl')
import Data.Text hiding (take,head,tail,zip,zipWith,concat,foldl')

import Data.Conduit
import Data.Conduit.List (consume)
import qualified Control.Monad as M
import qualified Data.Foldable as F
import qualified Data.Traversable as T

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

runDB a = withMongoDBConn "onping_production"  "10.84.207.130" (PortNumber 27017) Nothing 2000 $ \pool -> do 

  (runMongoDBPool slaveOk a )  pool



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

-- | z12 26
rawWaterPH :: Int
rawWaterPH = 3176

-- | aa12 ab12
finishWaterPH :: Int
finishWaterPH = 3190


-- | i12
backwashFlowTotal :: Int
backwashFlowTotal = 3955


-- | c12
filterOneRunStatus :: Int
filterOneRunStatus = 3128

-- | d12
filterTwoRunStatus :: Int
filterTwoRunStatus = 3144



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
   return $ UTCTime (fromGregorian  2013 11 00) (fromIntegral $ 0)
   
oneDay :: NominalDiffTime 
oneDay = realToFrac $ 60*60*24

mkRowList1  :: (MonadIO m, MonadBaseControl IO m) => Int -> 
     UTCTime -> m [[FullyIndexedCellValue]]
mkRowList1 bRow bTime = do 
  l1 <- fcn30 mkTurbidityRow
  l2 <- fcn30 mkChlorineRow
  return $ l1 ++ l2
      where r = realToFrac
            fcn30 f = mapM (\(i,newTime) -> f (i) (newTime) defaultStepList) (zipWith (\i b -> (i+bRow,addUTCTime ((r i)*oneDay) b)) [0 .. 30] (repeat bTime))

mkRowList2 bRow bTime = do
  ls  <- (fcn31 `T.mapM` [mkTotalFlowRow, mkRawWaterPhRow, mkFinishWaterPhRow1, mkFinishWaterPhRow2, mkBackwashFlowTotalRow])-- mkRunStatusAccumulator1Row, mkRunStatusAccumulator2Row ])
  acc1 <- mkRunStatusAccumulator1Row rowList
  return $ (F.concat ls ) ++ acc1
      where r = realToFrac
            fcn31 f = mapM (\(i,newTime) -> f (i) (newTime) defaultStepList) (zipWith (\i b -> (i+ bRow,addUTCTime ((r i)*oneDay) b)) [0 .. 30] (repeat bTime))
            rowList = (zipWith (\i b -> (i+ bRow,addUTCTime ((r i)*oneDay) b)) [0 .. 30] (repeat bTime)) -- extracted for accumulators
          
testMkRowList = do 
  z   <- testTime
  rowListList <- mkRowList1 0 z
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
    (mTotalFlow) <- selectFirst [OnpingTagHistoryTime >=. (Just baseTime),OnpingTagHistoryTime <. (Just (addUTCTime delta baseTime)), OnpingTagHistoryPid ==. (Just totalFlow)] []    
    return $ catMaybes [ ((onpingTagToFICV 0 2  rowNum).entityVal) <$> mTotalFlow]
        


-- -- | z12 26
-- rawWaterPH :: Int
-- rawWaterPH = 3176

mkRawWaterPhRow ::  (MonadIO m, MonadBaseControl IO m) =>
     Int -> UTCTime -> [NominalDiffTime] -> m [FullyIndexedCellValue]
mkRawWaterPhRow rowNum baseTime stepList = do 
  runDB $ do 
    mRawWaterPHList <- selectFirst [OnpingTagHistoryTime >=. (Just baseTime),OnpingTagHistoryTime <. (Just (addUTCTime delta baseTime)), OnpingTagHistoryPid ==. (Just rawWaterPH)] []
    return $ catMaybes  [ (onpingTagToFICV 0 26 rowNum).entityVal <$> mRawWaterPHList ]


-- -- | aa12 ab12
-- finishWaterPH :: Int
-- finishWaterPH = 3190

mkFinishWaterPhRow1 ::  (MonadIO m, MonadBaseControl IO m) =>
     Int -> UTCTime -> [NominalDiffTime] -> m [FullyIndexedCellValue]
mkFinishWaterPhRow1 rowNum baseTime stepList = do 
  runDB $ do 
    mFinishWaterPHList <- selectFirst [OnpingTagHistoryTime >=. (Just baseTime),OnpingTagHistoryTime <. (Just (addUTCTime delta baseTime)), OnpingTagHistoryPid ==. (Just finishWaterPH)][]
    return $ catMaybes [ (onpingTagToFICV 0 27 rowNum).entityVal <$> mFinishWaterPHList]

mkFinishWaterPhRow2 ::  (MonadIO m, MonadBaseControl IO m) =>
     Int -> UTCTime -> [NominalDiffTime] -> m [FullyIndexedCellValue]
mkFinishWaterPhRow2 rowNum baseTime stepList = do 
  runDB $ do
    let newBaseTime = addUTCTime  (realToFrac 4*3600) baseTime
    (mFinishWaterPHList)<- selectFirst [OnpingTagHistoryTime >=. (Just newBaseTime),OnpingTagHistoryTime <. (Just (addUTCTime delta newBaseTime)), OnpingTagHistoryPid ==. (Just finishWaterPH)][]
    return $ catMaybes [ (onpingTagToFICV 0 28 rowNum).entityVal <$> mFinishWaterPHList]


-- -- | i12
-- backwashFlowTotal :: Int
-- backwashFlowTotal = 3955

mkBackwashFlowTotalRow ::  (MonadIO m, MonadBaseControl IO m) =>
     Int -> UTCTime -> [NominalDiffTime] -> m [FullyIndexedCellValue]
mkBackwashFlowTotalRow rowNum baseTime stepList = do 
  runDB $ do
    (mBackwashFlowTotalList)<- selectFirst [OnpingTagHistoryTime >=. (Just baseTime),OnpingTagHistoryTime <. (Just (addUTCTime delta baseTime)), OnpingTagHistoryPid ==. (Just backwashFlowTotal)][]
    return $ catMaybes [ (onpingTagToFICV 0 9 rowNum).entityVal <$> mBackwashFlowTotalList ]





-- -- | c12
-- filterOneRunStatus :: Int
-- filterOneRunStatus = 3128

-- -- | d12
-- filterTwoRunStatus :: Int
-- filterTwoRunStatus = 3144


-- mkTishFilter  :: UTCTime -> [NominalDiffTime] -> [Filter OnpingTagHistory]
-- mkTishFilter baseTime stepList= (Prelude.foldl
--                          (\a b -> (mkDataRowFilter baseTime b) ||. a ) (mkDataRowFilter baseTime (head stepList)) (tail stepList ))



<<<<<<< HEAD
-- mkRunStatusAccumulator1Row rowNum baseTime stepList = do
--   statusAccum1List <- selectListIncremental 1000 [OnpingTagHistoryTime >=. (Just baseTime),OnpingTagHistoryTime <=. (Just (addUTCTime (realToFrac 24*3600) baseTime)), OnpingTagHistoryPid ==. (Just filterOneRunStatus)][]
--   let ttl = foldl' (\s v -> s + v) 0 $ catMaybes $ onpingTagHistoryVal.entityVal <$> statusAccum1List    
--   return $ [ (onpingTagToFICV 0 3 rowNum) $ OnpingTagHistory (Just filterOneRunStatus) (Just baseTime) (Just $ ttl)]

mkRunStatusAccumulator1Row rowandtime = do
  print "Accumulator 1 row"
  print rowandtime
  ttl  <- runDB $ do
    let countingFcn (r,t) = (\x -> (r,t, x)) <$> (Mdb.count (Mdb.select ["time" Mdb.=: ["$gt" Mdb.=: t , "$lt" Mdb.=: (addUTCTime (realToFrac 24*3600) t)] , "pid" Mdb.=: filterOneRunStatus ] "onping_tag_history") )
    T.traverse countingFcn rowandtime
  print ttl
  return $ (\(r,t,i) -> [ (onpingTagToFICV 0 3 r) $ OnpingTagHistory (Just filterOneRunStatus) (Just t) (Just $ (fromIntegral i))]) <$> ttl

=======
mkRunStatusAccumulator1Row rowNum baseTime stepList = do
  statusAccum1List <- selectListIncremental 1000 [OnpingTagHistoryTime >=. (Just baseTime),OnpingTagHistoryTime <=. (Just (addUTCTime (realToFrac 24*3600) baseTime)), OnpingTagHistoryPid ==. (Just filterOneRunStatus)][]
  let ttl = foldl' (\s v -> s + v) 0 $ catMaybes $ onpingTagHistoryVal.entityVal <$> statusAccum1List    
  return $ [ (onpingTagToFICV 0 3 rowNum) $ OnpingTagHistory (Just filterOneRunStatus) (Just baseTime) (Just $ ttl)]
>>>>>>> parent of f540d0f... Trying to use count



mkRunStatusAccumulator2Row rowNum baseTime stepList = do 
<<<<<<< HEAD
  print "Accumulator 2 row"
  ttl  <- runDB $ do
    Mdb.count (Mdb.select ["time" Mdb.=: ["$gt" Mdb.=: baseTime , "$lt" Mdb.=: (addUTCTime (realToFrac 24*3600) baseTime)] , "pid" Mdb.=: filterTwoRunStatus ] "onping_tag_history")      
  
  print ttl
  return $ [ (onpingTagToFICV 0 4 rowNum) $ OnpingTagHistory (Just filterTwoRunStatus) (Just baseTime) (Just $ (fromIntegral ttl))]


-- mkRunStatusAccumulator2Row rowNum baseTime stepList = do 
--   statusAccum1List <- selectListIncremental 1000 [OnpingTagHistoryTime >=. (Just baseTime),OnpingTagHistoryTime <=. (Just (addUTCTime (realToFrac 24*3600) baseTime)), OnpingTagHistoryPid ==. (Just filterTwoRunStatus)][]
--   let ttl = foldl' (\s v -> s + v) 0 $ catMaybes $ onpingTagHistoryVal.entityVal <$> statusAccum1List        
--   return $ [ (onpingTagToFICV 0 4 rowNum) $ OnpingTagHistory (Just filterTwoRunStatus) (Just baseTime) (Just $ ttl)]
=======
  statusAccum1List <- selectListIncremental 1000 [OnpingTagHistoryTime >=. (Just baseTime),OnpingTagHistoryTime <=. (Just (addUTCTime (realToFrac 24*3600) baseTime)), OnpingTagHistoryPid ==. (Just filterTwoRunStatus)][]
  let ttl = foldl' (\s v -> s + v) 0 $ catMaybes $ onpingTagHistoryVal.entityVal <$> statusAccum1List        
  return $ [ (onpingTagToFICV 0 4 rowNum) $ OnpingTagHistory (Just filterTwoRunStatus) (Just baseTime) (Just $ ttl)]
>>>>>>> parent of f540d0f... Trying to use count




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
  print "making Row Set 1"
  dList  <- mkRowList1 11 z
  print z  
  print "making Row Set 2"
  dList2 <- mkRowList2 12 z
  print "updating Spreadsheet"
  editWs <- return $ setMultiMappedSheetCellData ws (concat (dList ++ dList2))
  print "writing Spreadsheet"
  print dList2
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

 k <-  selectListIncremental 1000 ([OnpingTagHistoryPid ==.(Just 19813) ,OnpingTagHistoryTime >.(Just z)  ]||.[OnpingTagHistoryPid ==. (Just 19814), OnpingTagHistoryTime >. (Just z)]) [Desc OnpingTagHistoryTime , LimitTo 1000]
 print $ (onpingTagHistoryVal.entityVal) <$> k 


-- --------------------------------------------------


-- | Creates a limited data source that can be ran in place of a
-- call to runDB $ selectList qry args

dataSource ::(PersistEntityBackend a ~ MongoBackend ,PersistEntity a) =>  Int -> [Filter a] -> [SelectOpt a] -> Source IO [Entity a]
dataSource n qry opts = loop 1
 where loop i = do 
         rslt <- liftIO $ runDB $ selectList qry ([LimitTo n, OffsetBy (i*n)]  ++ opts)
         case rslt of 
           [] -> return ()
           l ->  do
             yield l
             loop (succ i)

testSelectListIncremental = do 
 r <- selectListIncremental 100 [OnpingTagHistoryPid ==. (Just 25315)] [LimitTo 1000]
 T.traverse print (take 1000 r)

selectListIncremental :: (PersistEntityBackend a ~ MongoBackend ,PersistEntity a ,MonadIO m , MonadBaseControl IO m, IO ~ m) => Int -> [Filter a] -> [SelectOpt a] -> m [Entity a]
selectListIncremental inc qry opts= do

  lol <- (dataSource inc qry opts) $$ consume
  return $ F.concat lol   
  
