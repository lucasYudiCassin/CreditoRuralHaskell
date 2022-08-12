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

type AnalisandoDadosID = Z

type EmissaoID = S (S Z)

type LiberadoID = S (S (S Z))

-- Tipo Estado com seu ID
type Estado :: IdEstado -> *
data Estado id where
  AnalisandoDados :: Estado AnalisandoDadosID
  Emissao :: Estado EmissaoID
  Liberado :: Estado LiberadoID

deriving instance Show (Estado id)

type ProximoEstado :: IdEstado -> *
type family ProximoEstado a

type instance ProximoEstado AnalisandoDadosID = Estado EmissaoID

type instance ProximoEstado EmissaoID = Estado LiberadoID

-- Função para definir o proximo estado
nextEstado :: Estado id -> Maybe (ProximoEstado id)
nextEstado AnalisandoDados = Just Emissao
nextEstado Emissao = Just Liberado
nextEstado _ = Nothing