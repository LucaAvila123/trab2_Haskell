import System.IO (hFlush, stdout)

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

main :: IO ()
main = do
  entrada <- entrada_arquivo
  saida <- saida_arquivo
  k <- numero_grupos
  putStrLn $ "Arquivo de entrada: " ++ entrada
  putStrLn $ "Arquivo de saída: " ++ saida
  putStrLn $ "Número de grupos: " ++ show k
