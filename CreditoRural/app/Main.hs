module Main where

import Contrato
import Database
import VerifyContrato
import Workflow (ExEstado)

-- import Produto
-- import User
-- import VerifyContrato
-- import Workflow

-- u1 = MkCliente "Lucas" 1

-- u2 = MkCliente "Yudi" 2

-- u3 = MkCliente "Cassin" 3

-- v1 = MkGerVenda "LucasV" 1

-- v2 = MkGerVenda "YudiV" 2

-- e1 = MkGerBack "LucasB" 1

-- c1 = MkContratoPronamp u1 v1 e1 100
teste :: Contrato -> Maybe ExEstado
teste = runContrato

main :: IO ()
main = putStrLn ""