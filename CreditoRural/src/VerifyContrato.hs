{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module VerifyContrato (runContrato) where

import Contrato (Contrato (..))
import Data.Kind (Constraint)
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

-- Função para comparar o limite tomado pelo usuário com o valor do contrato. Apenas em um determinado estado
verifyLimit :: Contrato -> Estado AnalisandoDadosID -> Maybe (ProximoEstado AnalisandoDadosID)
verifyLimit (MkContrato _ clie _ _ v p _ _) e
  | compareLimit (getContratado clie + v) (getLimit p) = nextEstado e
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
verifyDocs :: Contrato -> Estado EmissaoID -> Maybe (ProximoEstado EmissaoID)
verifyDocs (MkContrato _ clie _ _ _ _ docList _) e
  | (clie == getClieMatricula docList) && (clie == getClieCAR docList) = nextEstado e
  | otherwise = Nothing

-- Função para dado um estado passar para o próximo, apenas se passar na validação
verifyContrato :: Contrato -> Maybe (Estado id) -> Maybe (ProximoEstado id)
verifyContrato _ Nothing = Nothing
verifyContrato c (Just e@AnalisandoDados) = verifyLimit c e
verifyContrato c (Just e@Emissao) = verifyDocs c e
verifyContrato _ _ = Nothing

-- Função que passa o contrato pelo workflow, e garante chegar na etapa de liberação
runContrato :: Contrato -> Maybe (Estado LiberadoID)
runContrato c = verifyContrato c (verifyContrato c (Just AnalisandoDados))