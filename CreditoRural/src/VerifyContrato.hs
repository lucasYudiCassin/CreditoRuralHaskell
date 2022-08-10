{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module VerifyContrato where

import Contrato
import Database
import Documentos
import Produto
import User
import Workflow

helpExEstado :: Maybe (Estado id) -> Maybe ExEstado
helpExEstado Nothing = Nothing
helpExEstado (Just e) = Just $ MkExEstado e

compareLimit :: Double -> Limite -> Bool
compareLimit v (MkLimite d) = v <= d

verifyLimit :: Contrato -> Estado (S Z) -> Maybe ExEstado
verifyLimit (MkContrato _ clie _ _ v p _) e
  | compareLimit (getContratado clie + v) (getLimit p) = helpExEstado $ nextEstado e
  | otherwise = Nothing

getClieMatricula :: DocList [Matricula, CAR] -> User Cliente
getClieMatricula (DCons x xs) = clieDaMatricula x

getClieCAR :: DocList [Matricula, CAR] -> User Cliente
getClieCAR (DCons m (DCons c DEmpty)) = clieDoCAR c

verifyDocs :: Contrato -> Estado (S (S Z)) -> Maybe ExEstado
verifyDocs (MkContrato _ clie _ _ _ _ docList) e
  | (clie == getClieMatricula docList) && (clie == getClieCAR docList) = helpExEstado $ nextEstado e
  | otherwise = Nothing

-- verifyContrato :: Contrato -> Maybe (Estado id) -> Maybe ExEstado
-- verifyContrato _ Nothing = Nothing
-- verifyContrato c e@(Just AnalisandoDados) = verifyLimit c e
-- verifyContrato c e@(Just Emissao) = verifyDocs c e
-- verifyContrato _ _ = Nothing

verifyContrato :: Contrato -> Maybe ExEstado -> Maybe ExEstado
verifyContrato _ Nothing = Nothing
verifyContrato c (Just (MkExEstado e@AnalisandoDados)) = verifyLimit c e
verifyContrato c (Just (MkExEstado e@Emissao)) = verifyDocs c e
verifyContrato _ _ = Nothing

runContrato :: Contrato -> Maybe ExEstado
runContrato c = verifyContrato c (verifyContrato c (Just $ MkExEstado AnalisandoDados))