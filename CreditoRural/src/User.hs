{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module User where

data TipoUser = Cliente | GerVenda | GerBack | GerEmissao

type User :: TipoUser -> *
data User t where
  MkCliente :: String -> Int -> User Cliente
  MkGerVenda :: String -> Int -> User GerVenda
  MkGerBack :: String -> Int -> User GerBack
  MkGerEmissao :: String -> Int -> User GerEmissao

deriving instance Show (User t)

-- instance Show (User t) where
--     show (User Cliente) :: "Cliente"
--     show (User GerVenda) :: "Vendedor"
--     show _ :: "Outros"

-- Quando um gerente é designado para um contrato, somente ele pode atuar no contrato
-- Ter algum tipo de validador de usuário
-- Ter um limite que cada usuario pode contratar dado o quanto ele ja tomou