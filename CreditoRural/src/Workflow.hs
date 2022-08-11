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

type AnalisandoDadosT = Estado (S Z)

type EmissaoT = Estado (S (S Z))

type LiberadoT = Estado (S (S (S Z)))

-- Tipo Estado com seu ID
type Estado :: IdEstado -> *
data Estado id where
  AnalisandoDados :: AnalisandoDadosT
  Emissao :: EmissaoT
  Liberado :: LiberadoT

deriving instance Show (Estado id)

-- Função para definir o proximo estado
nextEstado :: Estado id -> Maybe (Estado (S id))
nextEstado AnalisandoDados = Just Emissao
nextEstado Emissao = Just Liberado
nextEstado _ = Nothing