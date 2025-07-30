module Functions where

import System.IO (hFlush, stdout)

-- parte 1
entrada_arquivo :: IO String
entrada_arquivo = do
  putStr "Forneca o nome do arquivo de entrada: "
  hFlush stdout
  arquivo_nome <- getLine
  return arquivo_nome

saida_arquivo :: IO String
saida_arquivo = do
  putStr "Forneca o nome do arquivo de saída: "
  hFlush stdout
  arquivo_nome <- getLine
  return arquivo_nome

numero_grupos :: IO Integer
numero_grupos = do
  putStr "Forneca o número de grupos (K): "
  hFlush stdout
  numero <- getLine
  return (read numero)

ler_pontos :: FilePath -> IO [String]
ler_pontos caminho = do
    conteudo <- readFile caminho
    return (lines conteudo)

type Ponto = (Int, [Double])

type Ligacao = (Int, Int, Double)

construir_lista_ligacoes :: [Ponto] -> [Ligacao]
construir_lista_ligacoes pontos = do
    return lista_ligacoes

