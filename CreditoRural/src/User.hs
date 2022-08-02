{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module User
    ( mkCliente, User, TipoUser
    ) where



data TipoUser = Cliente | GerVenda | GerBack | GerEmissao

type User :: TipoUser -> *
data User t where
    MkCliente :: String -> Int -> User Cliente
    MkGerVenda :: String -> Int -> User GerVenda
deriving instance Show (User t)


mkCliente :: String -> Int -> User Cliente
mkCliente n id = MkCliente n id
-- instance Show (User t) where
--     show (User Cliente) :: "Cliente"
--     show (User GerVenda) :: "Vendedor"
--     show _ :: "Outros"