{-# LANGUAGE GADTs #-}

module Funding where

import Produto (Geral, Pronaf, Pronamp)

-- Tipo Funding vinculado a um produto
data Funding p where
  LCA :: Funding Pronamp
  PoupancaRural :: Funding Pronaf
  DAV :: Funding Geral

instance Show (Funding p) where
  show LCA = "Letra de Credito do Agronegocio"
  show PoupancaRural = "Poupanca Rural"
  show DAV = "Deposito a vista"