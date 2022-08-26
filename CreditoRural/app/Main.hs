module Main where

import Contrato
import Database
import VerifyContrato
import Workflow

main :: IO ()
main = do
  print c1
  print $ runContrato c1
  print "------------------------------"
  print c2
  print $ runContrato c2
  print "------------------------------"
  print c3
  print $ runContrato c3