{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Produto where

import Data.Proxy (Proxy (Proxy))

-- Tipos vazios para os produtos
data Pronamp

data Pronaf

data Geral

instance Show Pronamp where
  show _ = "Pronamp"

instance Show Pronaf where
  show _ = "Pronaf"

instance Show Geral where
  show _ = "Geral"

-- Classe produto e instâncias para os produtos
class Produto p where
  nome :: String
  limite :: Limite
  taxa :: Double

instance Produto Pronamp where
  nome = "Pronamp"
  limite = MkLimite 3000000
  taxa = 0.05

instance Produto Pronaf where
  nome = "Pronaf"
  limite = MkLimite 150000
  taxa = 0.05

instance Produto Geral where
  nome = "Geral"
  limite = MkLimite 8000000
  taxa = 0.05

-- Tipos MBB e Limite
newtype MBB = MkMBB Double

newtype Limite = MkLimite Double

instance Show Limite where
  show (MkLimite v) = show v

instance Show MBB where
  show (MkMBB v) = show v

calcularMBB :: forall p. (Produto p) => Proxy p -> Double -> MBB
calcularMBB _ v = MkMBB $ taxa @p * v

toString :: forall p. (Produto p) => Proxy p -> String
toString _ =
  "Produto: " ++ nome @p
    ++ "\nLimite: "
    ++ show (limite @p)
    ++ "\nTaxa: "
    ++ show (taxa @p)

getLimit :: forall p. (Produto p) => Proxy p -> Limite
getLimit _ = limite @p