{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Produto where

import Data.Proxy (Proxy (Proxy))

data Pronamp

data Pronaf

data Geral

class Produto p where
  nome :: String
  limite :: Double
  taxa :: Double

instance Produto Pronamp where
  nome = "Pronamp"
  limite = 100
  taxa = 0.05

instance Produto Pronaf where
  nome = "Pronaf"
  limite = 100
  taxa = 0.05

instance Produto Geral where
  nome = "Geral"
  limite = 100
  taxa = 0.05

calcularMBB :: forall p. (Produto p) => Proxy p -> Double -> Double
calcularMBB _ v = taxa @p * v