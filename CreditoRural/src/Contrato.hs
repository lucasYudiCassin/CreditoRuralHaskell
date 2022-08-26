{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Contrato where

import Data.Proxy (Proxy (Proxy))
import Documentos (CAR, DocList, Matricula, getClieCAR, getClieMatricula)
import Funding (Funding)
import Produto (MBB, Produto, calcularMBB, toString)
import User (TipoUser (Cliente, GerBack, GerVenda), User)

-- Tipo Contrato contendo o ID do contrato, um cliente, gerente de venda, gerente do back, valor do contrato, Proxy do produto, lista de documentos contendo Matricula e CAR e o Fundind da Operação
data Contrato where
  MkContrato :: forall p. (Show p, Produto p) => Int -> User Cliente -> User GerVenda -> User GerBack -> Double -> Proxy p -> DocList [Matricula, CAR] -> Funding p -> Contrato

instance Show Contrato where
  show ctr@(MkContrato id c ve b vl p docList f) =
    "Contrato (" ++ show id ++ "):\n"
      ++ show c
      ++ "\n"
      ++ show ve
      ++ "\n"
      ++ show b
      ++ "\n"
      ++ toString p
      ++ "\nFunding: "
      ++ show f
      ++ "\nValor: "
      ++ show vl
      ++ "\nResultado: "
      ++ show (resultadoContrato ctr)
      ++ "\nMatricula: "
      ++ show (getClieMatricula docList)
      ++ "\nCAR: "
      ++ show (getClieCAR docList)

-- Função para capturar o resultado esperado do contrato
resultadoContrato :: Contrato -> MBB
resultadoContrato (MkContrato _ _ _ _ v p docList _) = calcularMBB p v