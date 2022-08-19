{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}

module Database where

import Contrato (Contrato (..))
import Data.Proxy (Proxy (Proxy))
import Documentos (CAR (..), DocList (..), Matricula (..))
import Funding (Funding (LCA, PoupancaRural))
import Produto (Pronaf, Pronamp)
import User (TipoUser (Cliente, GerBack, GerVenda), User (..))

-- Criando usuários
u1 :: User 'Cliente
u1 = MkCliente "Lucas" 1

u2 :: User 'Cliente
u2 = MkCliente "João" 2

u3 :: User 'Cliente
u3 = MkCliente "Alícia" 3

v1 :: User 'GerVenda
v1 = MkGerVenda "Allan" 1

v2 :: User 'GerVenda
v2 = MkGerVenda "Franca" 2

b1 :: User 'GerBack
b1 = MkGerBack "Roberto" 1

-- Criando matrículas
m1 :: Matricula
m1 = MkMatricula u1 "12345"

m2 :: Matricula
m2 = MkMatricula u2 "12345"

-- Criando CARs
car1 :: CAR
car1 = MkCAR u1 "12345"

car2 :: CAR
car2 = MkCAR u2 "12345"

-- Criando lista de documentos
dl1 :: DocList [Matricula, CAR]
dl1 = DCons m1 $ DCons car1 DEmpty

dl2 :: DocList [Matricula, CAR]
dl2 = DCons m2 $ DCons car2 DEmpty

dl3 :: DocList [CAR, Matricula]
dl3 = DCons car2 $ DCons m2 DEmpty

dl4 :: DocList '[CAR]
dl4 = DCons car2 DEmpty

-- Criando contratos
c1 :: Contrato
c1 = MkContrato 1 u1 v1 b1 100 (Proxy @Pronamp) dl1 LCA

c2 :: Contrato
c2 = MkContrato 2 u1 v1 b1 100 (Proxy @Pronaf) dl2 PoupancaRural

-- Pegar o valor contratado do cliente no mercado
getContratado :: User Cliente -> Double
getContratado (MkCliente _ 1) = 100
getContratado (MkCliente _ 2) = 100
getContratado (MkCliente _ 3) = 100
getContratado (MkCliente _ _) = 100