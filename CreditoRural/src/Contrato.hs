{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}

module Contrato where

import Data.Proxy (Proxy (Proxy))
import Produto
import User

type Contrato :: *
data Contrato where
  MkContrato :: forall p. (Show p, Produto p) => Int -> User Cliente -> User GerVenda -> User GerBack -> Double -> Proxy p -> Contrato

instance Show Contrato where
  show ctr@(MkContrato id c ve b vl p) =
    "Contrato (" ++ show id ++ "):\n"
      ++ show c
      ++ "\n"
      ++ show ve
      ++ "\n"
      ++ show b
      ++ "\n"
      ++ toString p
      ++ "\nValor: "
      ++ show vl
      ++ "\nResultado: "
      ++ show (resultadoContrato ctr)

u1 :: User 'Cliente
u1 = MkCliente "Lucas" 1

u2 :: User 'Cliente
u2 = MkCliente "Yudi" 2

u3 :: User 'Cliente
u3 = MkCliente "Cassin" 3

v1 :: User 'GerVenda
v1 = MkGerVenda "LucasV" 1

v2 :: User 'GerVenda
v2 = MkGerVenda "YudiV" 2

e1 :: User 'GerBack
e1 = MkGerBack "LucasB" 1

c1 :: Contrato
c1 = MkContrato 1 u1 v1 e1 100 (Proxy @Pronamp)

c2 :: Contrato
c2 = MkContrato 2 u1 v1 e1 100 (Proxy @Pronaf)

resultadoContrato :: Contrato -> MBB
resultadoContrato (MkContrato _ _ _ _ v p) = calcularMBB p v
