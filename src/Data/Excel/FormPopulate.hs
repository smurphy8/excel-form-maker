module Data.Excel.FormPopulate where

import Data.Excel.FormPopulate.Internal
import Codec.Xlsx.Parser


createForm :: IO () 
createForm = do 
  xlsx "DEQ_Template.xlsx"
  print "Hello" 
