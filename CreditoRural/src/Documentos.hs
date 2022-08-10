{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Documentos where

import User

data Matricula where
  MkMatricula :: User Cliente -> String -> Matricula

deriving instance Show Matricula

data CAR where
  MkCAR :: User Cliente -> String -> CAR

deriving instance Show CAR

data DocList x where
  DEmpty :: DocList '[]
  DCons :: Show x => x -> DocList xs -> DocList (x : xs)

deriving instance Show (DocList x)

clieDaMatricula :: Matricula -> User Cliente
clieDaMatricula (MkMatricula u _) = u

clieDoCAR :: CAR -> User Cliente
clieDoCAR (MkCAR u _) = u