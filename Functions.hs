module Functions where

import System.IO (hFlush, stdout)
import Data.List (sortBy, intercalate) 

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

-- parte 2
type Ponto = (Int, [Double])

-- Função para converter uma string como "1,2,3" em [Double]
parseNumeros :: String -> [Double]
parseNumeros linha = map read (split ',' linha)
  where
    split :: Char -> String -> [String]
    split _ "" = []
    split delim str =
      let (parte, resto) = span (/= delim) str
      in parte : if null resto then [] else split delim (tail resto)

-- Função principal que lê o arquivo e converte em [Ponto]
ler_pontos :: FilePath -> IO [Ponto]
ler_pontos caminho = do
    conteudo <- readFile caminho
    let linhas = lines conteudo
    let pontos = zipWith (\idx linha -> (idx, parseNumeros linha)) [1..] linhas
    return pontos

-- parte 3
type Ligacao = (Int, Int, Double)

-- Calcula a distância euclidiana entre dois pontos (vetores de Double)
distancia :: [Double] -> [Double] -> Double
distancia xs ys = sqrt $ sum $ zipWith (\x y -> (x - y)^2) xs ys

-- Algoritmo principal que gera a lista de ligações
gerarLigacoes :: [Ponto] -> [Ligacao]
gerarLigacoes [] = []
gerarLigacoes (pontoAtual:resto) =
    let numPontos = length (pontoAtual:resto)
        visitados = replicate numPontos False
        -- Marca o ponto atual como visitado
        visitados' = take (fst pontoAtual - 1) visitados ++ [True] ++ drop (fst pontoAtual) visitados
        -- Gera as ligações
        ligacoes = auxiliar [pontoAtual] resto visitados' []
    in ligacoes
  where
    auxiliar :: [Ponto] -> [Ponto] -> [Bool] -> [Ligacao] -> [Ligacao]
    auxiliar _ [] _ ligs = reverse ligs  -- Todos os pontos conectados
    auxiliar (atual:corrente) naoEscolhidos visitados ligs =
        let (idxAtual, coordsAtual) = atual
            -- Encontra o ponto mais próximo não visitado
            (p1, p2, dist) = proximoPonto atual (atual:naoEscolhidos) visitados
            -- Marca o novo ponto como visitado
            visitados' = take (p2 - 1) visitados ++ [True] ++ drop p2 visitados
            -- Pega o novo ponto corrente
            novoCorrente = encontrarPorIndice p2 naoEscolhidos
            -- Remove o ponto escolhido dos não escolhidos
            naoEscolhidos' = filter (\(idx, _) -> idx /= p2) naoEscolhidos
        in auxiliar (novoCorrente:atual:corrente) naoEscolhidos' visitados' ((p1, p2, dist):ligs)

-- encontra o ponto mais próximo não visitado
proximoPonto :: Ponto -> [Ponto] -> [Bool] -> Ligacao
proximoPonto (idx1, coords1) pontos visitados =
    let naoVisitados = filter (\(idx, _) -> not (visitados !! (idx - 1))) pontos
        distancias = map (\(idx2, coords2) -> (idx1, idx2, distancia coords1 coords2)) naoVisitados
        (_, p2, dist) = minimoPorDistancia distancias
    in (idx1, p2, dist)

encontrarPorIndice :: Int -> [Ponto] -> Ponto
encontrarPorIndice _ [] = error "Ponto não encontrado"
encontrarPorIndice idx ((i, coords):xs)
    | idx == i = (i, coords)
    | otherwise = encontrarPorIndice idx xs

minimoPorDistancia :: [(Int, Int, Double)] -> (Int, Int, Double)
minimoPorDistancia [] = error "Lista vazia!"
minimoPorDistancia [x] = x
minimoPorDistancia (x:xs) =
    let menorResto = minimoPorDistancia xs
    in if terceiro x <= terceiro menorResto then x else menorResto
  where
    terceiro (_, _, d) = d  -- Pega o Double (distância)

-- parte 4

-- Função para ordenar ligações por distância (maiores primeiro)
ordenarLigacoes :: [Ligacao] -> [Ligacao]
ordenarLigacoes = sortBy compararLigacoes
  where
    compararLigacoes (a1, a2, ad) (b1, b2, bd)
      | ad > bd = LT
      | ad < bd = GT
      | a1 < b1 = LT
      | a1 > b1 = GT
      | a2 < b2 = LT
      | otherwise = GT

-- Função para dividir a lista de ligações em K grupos
dividirEmKGrupos :: Int -> [Ligacao] -> [[Int]]
dividirEmKGrupos k ligacoes = 
    let n = length ligacoes + 1  -- Número total de pontos (ligações + 1)
        gruposIniciais = [[i] | i <- [1..n]]  -- Cada ponto é seu próprio grupo
        todasLigacoes = ordenarLigacoes ligacoes
    in if k >= n 
       then gruposIniciais  -- Cada ponto é um grupo se K >= N
       else auxiliar gruposIniciais (reverse todasLigacoes) (n - k)
  where
    auxiliar grupos [] _ = grupos
    auxiliar grupos _ 0 = grupos
    auxiliar grupos ((p1, p2, _):resto) cortes =
        let grupo1 = encontrarGrupo p1 grupos
            grupo2 = encontrarGrupo p2 grupos
            novosGrupos = if grupo1 == grupo2 
                          then grupos 
                          else unirGrupos grupo1 grupo2 grupos
        in auxiliar novosGrupos resto (if grupo1 == grupo2 then cortes else cortes - 1)
    
    encontrarGrupo :: Int -> [[Int]] -> [Int]
    encontrarGrupo p grupos = case filter (elem p) grupos of
                                [] -> error "Ponto não encontrado em nenhum grupo"
                                (g:_) -> g
    
    unirGrupos :: [Int] -> [Int] -> [[Int]] -> [[Int]]
    unirGrupos g1 g2 grupos = 
        let outrosGrupos = filter (\g -> g /= g1 && g /= g2) grupos
        in (g1 ++ g2) : outrosGrupos

-- Função para formatar os grupos para saída
formatarGrupos :: [[Int]] -> String
formatarGrupos = unlines . map (intercalate ", " . map show)

-- Função para escrever os grupos no arquivo de saída
escreverGrupos :: FilePath -> [[Int]] -> IO ()
escreverGrupos caminho grupos = 
    writeFile caminho (formatarGrupos grupos)

-- Função principal que realiza todo o agrupamento
realizarAgrupamento :: String -> String -> Int -> IO ()
realizarAgrupamento entrada saida k = do
    pontos <- ler_pontos entrada
    let ligacoes = gerarLigacoes pontos
    let n = length pontos
    let grupos = dividirEmKGrupos (min k n) ligacoes  -- Garante que k não seja maior que n
    escreverGrupos saida grupos
    putStrLn "Agrupamentos:"
    putStr (formatarGrupos grupos)