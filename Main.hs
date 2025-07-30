module Main where

import Functions

main :: IO ()
main = do
  -- parte 1: leitura dos nomes de entrada
  entrada <- entrada_arquivo  -- Validacao/base1.csv
  saida <- saida_arquivo
  k <- numero_grupos
  putStrLn $ "Arquivo de entrada: " ++ entrada
  putStrLn $ "Arquivo de saída: " ++ saida
  putStrLn $ "Número de grupos: " ++ show k
  
  -- parte 2: leitura do conteúdo do arquivo
  pontos <- ler_pontos entrada
  print pontos

  -- parte 3: realização do agrupamento de dados
  