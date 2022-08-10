{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module User where

data TipoUser = Cliente | GerVenda | GerBack

type User :: TipoUser -> *
data User t where
  MkCliente :: String -> Int -> User Cliente
  MkGerVenda :: String -> Int -> User GerVenda
  MkGerBack :: String -> Int -> User GerBack

instance Eq (User t) where
  (MkCliente _ id1) == (MkCliente _ id2) = id1 == id2
  (MkGerVenda _ id1) == (MkGerVenda _ id2) = id1 == id2
  (MkGerBack _ id1) == (MkGerBack _ id2) = id1 == id2

instance Show (User t) where
  show (MkCliente n id) = "Nome Cliente (" ++ show id ++ "): " ++ n
  show (MkGerVenda n id) = "Nome Ger. Venda (" ++ show id ++ "): " ++ n
  show (MkGerBack n id) = "Nome Ger. Emissao (" ++ show id ++ "): " ++ n

-- Quando um gerente é designado para um contrato, somente ele pode atuar no contrato
-- Ter algum tipo de validador de usuário
-- Ter um limite que cada usuario pode contratar dado o quanto ele ja tomou