module Main where

import Functions

main :: IO ()
main = do
  entrada <- entrada_arquivo
  saida <- saida_arquivo
  k <- numero_grupos
  realizarAgrupamento entrada saida (fromIntegral k)