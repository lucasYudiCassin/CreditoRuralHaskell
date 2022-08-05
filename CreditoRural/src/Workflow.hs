{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Workflow where

import Data.Kind (Constraint)
import System.Posix (isNamedPipe)
import User

-- data Estado = AguardandoDados | AnalisandoDados | Emissao | Liberado | Cancelado

data IdEstado = Z | S IdEstado

-- type EstadoCompleto = IdEstado Estado

-- type AguardandoDados = (S Z) AguardandoDados

type Estado :: IdEstado -> *
data Estado id where
  AguardandoDados :: Estado (S Z)
  AnalisandoDados :: Estado (S (S Z))
  Emissao :: Estado (S (S (S Z)))
  Liberado :: Estado (S (S (S (S Z))))
  Cancelado :: Estado (S (S (S (S Z))))

type Next :: IdEstado -> IdEstado -> Constraint
class Next id1 id2

instance Next ('S 'Z) ('S ('S 'Z))

--instance Next (S (S Z)) (S (S (S Z)))

-- transicao :: (Next id1 id2) => Estado id1 -> Estado id2
-- transicao AguardandoDados = AnalisandoDados
