import System.IO (hFlush, stdout)
main :: IO ()
main = do 
  putStr "Forneca o nome do arquivo de entrada: "
  hFlush stdout
  arquivo_nome <- getLine
  putStrLn arquivo_nome
