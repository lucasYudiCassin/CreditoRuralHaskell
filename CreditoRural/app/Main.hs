module Main where

import Contrato
import Database
import VerifyContrato
import Workflow

teste :: Contrato -> Maybe (Estado LiberadoID)
teste = runContrato

main :: IO ()
main = do
  print $ teste c1
  print $ teste c2

-- Polimorfismo (OK)
-- Tipos de dados algébricos (OK)
-- Tipos fantasmas
-- Tipos, Kinds, Sorts (OK)
-- GADTs (OK)
-- Singleton (OK)
-- Tipos existenciais
-- Type Family
-- Open Type Family (OK)
-- Tipos associados
-- Data family
