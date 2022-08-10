{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeOperators #-}

module Contrato where

import Data.Proxy (Proxy (Proxy))
import Documentos
import Produto
import User

data Contrato where
  MkContrato :: forall p. (Show p, Produto p) => Int -> User Cliente -> User GerVenda -> User GerBack -> Double -> Proxy p -> DocList [Matricula, CAR] -> Contrato

instance Show Contrato where
  show ctr@(MkContrato id c ve b vl p docList) =
    "Contrato (" ++ show id ++ "):\n"
      ++ show c
      ++ "\n"
      ++ show ve
      ++ "\n"
      ++ show b
      ++ "\n"
      ++ toString p
      ++ "\nValor: "
      ++ show vl
      ++ "\nResultado: "
      ++ show (resultadoContrato ctr)

resultadoContrato :: Contrato -> MBB
resultadoContrato (MkContrato _ _ _ _ v p docList) = calcularMBB p v
