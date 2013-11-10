{-# LANGUAGE OverloadedStrings #-}

module Data.Excel.FormPopulate where

import Data.Excel.FormPopulate.Internal
import Codec.Xlsx.Parser
import Codec.Xlsx.Writer
import Codec.Xlsx.Lens
import Codec.Xlsx









createForm :: IO () 
createForm = do 
  x <- xlsx "BuildSheet.xlsx"
  ws <- getWorksheets x
  editWs <- return $ setMultiMappedSheetCellData ws [FICV 0 1 3 (CellText"TestValue")]
  writeXlsx "ptest2.xlsx" x (Just editWs)
