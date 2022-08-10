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
import Database
import Documentos
import Produto
import User

data IdEstado = Z | S IdEstado deriving (Show)

type Estado :: IdEstado -> *
data Estado id where
  AnalisandoDados :: Estado (S Z)
  Emissao :: Estado (S (S Z))
  Liberado :: Estado (S (S (S Z)))

deriving instance Show (Estado id)

nextEstado :: Estado id -> Maybe (Estado (S id))
nextEstado AnalisandoDados = Just Emissao
nextEstado Emissao = Just Liberado
nextEstado _ = Nothing

data ExEstado where
  MkExEstado :: Estado id -> ExEstado

deriving instance Show ExEstado

data Aprovado

data Reprovado

type Resultado :: Maybe ExEstado -> *
--type Resultado :: Maybe ExEstado -> *
type family Resultado a where
  Resultado (Just (MkExEstado Liberado)) = Aprovado
  Resultado _ = Reprovado

-- Resultado Nothing = Reprovado

-- type Teste :: User t -> *
-- type family Teste t where
--   Teste (User Cliente) = Aprovado
--   Teste GerVenda = Reprovado

-- passarPeloWF :: Contrato -> Resultado (Maybe (MkExEstado Liberado))
