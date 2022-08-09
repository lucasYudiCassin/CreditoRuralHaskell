{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Workflow where

import Contrato
import Produto
import User

data IdEstado = Z | S IdEstado deriving (Show)

type Estado :: IdEstado -> *
data Estado id where
  AguardandoDados :: Estado (S Z)
  AnalisandoDados :: Estado (S (S Z))
  Emissao :: Estado (S (S (S Z)))
  Liberado :: Estado (S (S (S (S Z))))

deriving instance Show (Estado id)

nextEstado :: Estado id -> Maybe (Estado (S id))
nextEstado AguardandoDados = Just AnalisandoDados
nextEstado AnalisandoDados = Just Emissao
nextEstado Emissao = Just Liberado
nextEstado _ = Nothing

data ExEstado where
  MkExEstado :: Estado id -> ExEstado

deriving instance Show ExEstado

helpExEstado :: Maybe (Estado id) -> Maybe ExEstado
helpExEstado Nothing = Nothing
helpExEstado (Just e) = Just $ MkExEstado e

compareLimit :: Double -> Limite -> Bool
compareLimit v (MkLimite d) = v <= d

verifyContrato :: Contrato -> Maybe (Estado id) -> Maybe ExEstado
verifyContrato _ Nothing = Nothing
verifyContrato (MkContrato _ clie _ _ v p) (Just AnalisandoDados)
  | compareLimit (getContratado clie + v) (getLimit p) = helpExEstado $ nextEstado AnalisandoDados
  | otherwise = Nothing
verifyContrato c (Just x) = helpExEstado $ nextEstado x

teste :: Contrato -> Estado id -> Maybe ExEstado
teste c e = verifyContrato c (Just e)
