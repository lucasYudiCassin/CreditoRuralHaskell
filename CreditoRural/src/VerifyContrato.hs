{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module VerifyContrato (runContrato) where

import Contrato (Contrato (..))
import Database (getContratado)
import Documentos
  ( CAR,
    DocList (..),
    Matricula,
    clieDaMatricula,
    clieDoCAR,
  )
import Produto (Limite (..), getLimit)
import User (TipoUser (Cliente), User)
import Workflow
  ( Estado (AnalisandoDados, Emissao),
    ExEstado (..),
    IdEstado (S, Z),
    nextEstado,
  )

-- Função para passar de um Maybe Estado para Maybe ExEstado
helpExEstado :: Maybe (Estado id) -> Maybe ExEstado
helpExEstado Nothing = Nothing
helpExEstado (Just e) = Just $ MkExEstado e

-- Função para comparar o limite tomado pelo usuário com o valor do contrato. Apenas em um determinado estado
verifyLimit :: Contrato -> Estado (S Z) -> Maybe ExEstado
verifyLimit (MkContrato _ clie _ _ v p _ _) e
  | compareLimit (getContratado clie + v) (getLimit p) = helpExEstado $ nextEstado e
  | otherwise = Nothing

-- Função auxiliar na comparação de limite
compareLimit :: Double -> Limite -> Bool
compareLimit v (MkLimite d) = v <= d

-- Pegar a matricula de uma lista de documentos
getClieMatricula :: DocList [Matricula, CAR] -> User Cliente
getClieMatricula (DCons x xs) = clieDaMatricula x

-- Pegar o CAR de uma lista de documentos
getClieCAR :: DocList [Matricula, CAR] -> User Cliente
getClieCAR (DCons m (DCons c DEmpty)) = clieDoCAR c

-- Função para verificar os documentos do contrato. Apenas em um determinado estado
verifyDocs :: Contrato -> Estado (S (S Z)) -> Maybe ExEstado
verifyDocs (MkContrato _ clie _ _ _ _ docList _) e
  | (clie == getClieMatricula docList) && (clie == getClieCAR docList) = helpExEstado $ nextEstado e
  | otherwise = Nothing

-- Função para dado um estado passar para o próximo, apenas se passar na validação
verifyContrato :: Contrato -> Maybe ExEstado -> Maybe ExEstado
verifyContrato _ Nothing = Nothing
verifyContrato c (Just (MkExEstado e@AnalisandoDados)) = verifyLimit c e
verifyContrato c (Just (MkExEstado e@Emissao)) = verifyDocs c e
verifyContrato _ _ = Nothing

-- TODO: Garantir que termine no estado correto
runContrato :: Contrato -> Maybe ExEstado
runContrato c = verifyContrato c (verifyContrato c (Just $ MkExEstado AnalisandoDados))