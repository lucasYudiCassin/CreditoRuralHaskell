{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Workflow where

import Contrato ()
import Database ()
import Documentos ()
import Produto ()
import User ()

-- Tipo para criar um ID do estado
data IdEstado = Z | S IdEstado deriving (Show)

-- Tipo Estado com seu ID
type Estado :: IdEstado -> *
data Estado id where
  AnalisandoDados :: Estado (S Z)
  Emissao :: Estado (S (S Z))
  Liberado :: Estado (S (S (S Z)))

deriving instance Show (Estado id)

-- Função para definir o proximo estado
nextEstado :: Estado id -> Maybe (Estado (S id))
nextEstado AnalisandoDados = Just Emissao
nextEstado Emissao = Just Liberado
nextEstado _ = Nothing

-- Tipo para esconder o ID do estado
data ExEstado where
  MkExEstado :: Estado id -> ExEstado

deriving instance Show ExEstado

data Aprovado

data Reprovado

type Resultado :: Maybe ExEstado -> *
type family Resultado a where
  Resultado (Just (MkExEstado Liberado)) = Aprovado
  Resultado _ = Reprovado
