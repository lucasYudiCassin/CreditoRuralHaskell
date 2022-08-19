{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module User where

import Data.Kind ()

-- TipoUser para ser usado no usuário
data TipoUser = Cliente | GerVenda | GerBack

-- Tipo User que está vinculado a um único tipo de usuário
type User :: TipoUser -> *
data User t where
  MkCliente :: String -> Int -> User Cliente
  MkGerVenda :: String -> Int -> User GerVenda
  MkGerBack :: String -> Int -> User GerBack

getId :: User t -> Int
getId (MkCliente _ id) = id
getId (MkGerVenda _ id) = id
getId (MkGerBack _ id) = id

instance Eq (User t) where
  (MkCliente _ id1) == (MkCliente _ id2) = id1 == id2
  (MkGerVenda _ id1) == (MkGerVenda _ id2) = id1 == id2
  (MkGerBack _ id1) == (MkGerBack _ id2) = id1 == id2

instance Show (User t) where
  show (MkCliente n id) = "Nome Cliente (" ++ show id ++ "): " ++ n
  show (MkGerVenda n id) = "Nome Ger. Venda (" ++ show id ++ "): " ++ n
  show (MkGerBack n id) = "Nome Ger. Emissao (" ++ show id ++ "): " ++ n