module Main where

import Contrato
import Database
import VerifyContrato
import Workflow (LiberadoT)

teste :: Contrato -> Maybe LiberadoT
teste = runContrato

main :: IO ()
main = putStrLn ""