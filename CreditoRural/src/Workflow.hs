{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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

verifyContrato :: Contrato -> Maybe (Estado id) -> Maybe (Estado (S id))
verifyContrato _ Nothing = Nothing
verifyContrato (MkContrato _ clie _ _ v p) (Just AnalisandoDados)
  | getContratado clie + v <= getLimit p = nextEstado AnalisandoDados
  | otherwise = Nothing
verifyContrato c (Just x) = nextEstado x

teste :: Maybe (Estado ('S ('S ('S 'Z))))
teste = verifyContrato c1 (Just AnalisandoDados)