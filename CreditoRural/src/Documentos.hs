{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Documentos where

import User (TipoUser (Cliente), User)

-- Tipo para a Matricula
data Matricula where
  MkMatricula :: User Cliente -> String -> Matricula

deriving instance Show Matricula

-- Tipo para o CAR
data CAR where
  MkCAR :: User Cliente -> String -> CAR

deriving instance Show CAR

-- Tipo para a lista de documentos heterogênia
data DocList x where
  DEmpty :: DocList '[]
  DCons :: Show x => x -> DocList xs -> DocList (x : xs)

deriving instance Show (DocList x)

-- Função auxiliar para pegar o cliente vinculado à matricula
clieDaMatricula :: Matricula -> User Cliente
clieDaMatricula (MkMatricula u _) = u

-- Função auxiliar para pegar o cliente vinculado ao CAR
clieDoCAR :: CAR -> User Cliente
clieDoCAR (MkCAR u _) = u

-- Pegar a matricula de uma lista de documentos
getClieMatricula :: DocList [Matricula, CAR] -> User Cliente
getClieMatricula (DCons x xs) = clieDaMatricula x

-- Pegar o CAR de uma lista de documentos
getClieCAR :: DocList [Matricula, CAR] -> User Cliente
getClieCAR (DCons m (DCons c DEmpty)) = clieDoCAR c