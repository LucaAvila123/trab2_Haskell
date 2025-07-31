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

-- Encontra a ligação de maior distância em uma lista
maiorLigacao :: [Ligacao] -> Ligacao
maiorLigacao [] = error "Lista vazia!"
maiorLigacao [x] = x
maiorLigacao (x:xs) =
    let maiorResto = maiorLigacao xs
    in if terceiro x >= terceiro maiorResto then x else maiorResto
  where
    terceiro (_, _, d) = d

-- Verifica se um ponto está em um grupo
pertenceAoGrupo :: Int -> [Int] -> Bool
pertenceAoGrupo _ [] = False
pertenceAoGrupo x (y:ys) = x == y || pertenceAoGrupo x ys

-- Divide um grupo em dois com base na maior ligação interna
-- Versão corrigida: não remove pontos, apenas divide o grupo em dois subconjuntos
dividirGrupo :: [Int] -> [Ligacao] -> ([Int], [Int])
dividirGrupo grupo ligacoes =
    let ligacoesDoGrupo = filter (\(p1, p2, _) -> pertenceAoGrupo p1 grupo && pertenceAoGrupo p2 grupo) ligacoes
        (p1, p2, _) = maiorLigacao ligacoesDoGrupo
        -- Divide o grupo em dois, mantendo todos os pontos
        -- Grupo 1: pontos conectados a p1 (incluindo p1)
        -- Grupo 2: pontos conectados a p2 (incluindo p2)
        (grupo1, grupo2) = separarGrupos grupo p1 p2 [] []
    in (grupo1, grupo2)
  where
    -- Separa os pontos com base na conexão a p1 ou p2
    separarGrupos [] _ _ acc1 acc2 = (acc1, acc2)
    separarGrupos (x:xs) p1 p2 acc1 acc2
        | x == p1 = separarGrupos xs p1 p2 (x:acc1) acc2
        | x == p2 = separarGrupos xs p1 p2 acc1 (x:acc2)
        | otherwise =
            -- Verifica se x está conectado a p1 ou p2 nas ligações
            case (estaConectado x p1 ligacoes, estaConectado x p2 ligacoes) of
                (True, _) -> separarGrupos xs p1 p2 (x:acc1) acc2
                (_, True) -> separarGrupos xs p1 p2 acc1 (x:acc2)
                _ -> separarGrupos xs p1 p2 (x:acc1) (x:acc2)  -- Se não conectado, duplica (raro)

-- Verifica se dois pontos estão conectados (diretamente ou indiretamente)
estaConectado :: Int -> Int -> [Ligacao] -> Bool
estaConectado a b ligs =
    any (\(p1, p2, _) -> (p1 == a && p2 == b) || (p1 == b && p2 == a)) ligs
    
----------------------------------------------------------
-- Algoritmo principal para formar K grupos
----------------------------------------------------------

-- Função principal que divide os pontos em K grupos
dividirEmKgrupos :: Int -> [Ponto] -> [Ligacao] -> [[Int]]
dividirEmKgrupos k pontos ligacoes =
    let todosPontos = map fst pontos  -- Pega apenas os números dos pontos
        gruposIniciais = [todosPontos]  -- Começa com todos em um único grupo
    in formarGrupos k gruposIniciais ligacoes

-- Função auxiliar que realiza as divisões até ter K grupos
formarGrupos :: Int -> [[Int]] -> [Ligacao] -> [[Int]]
formarGrupos k grupos ligacoes
    | length grupos >= k = grupos  -- Condição de parada: já temos K grupos
    | otherwise =
        let -- Encontra o grupo com a maior ligação interna
            (grupoMaior, ligMaior) = encontrarGrupoComMaiorLigacao grupos ligacoes
            -- Divide esse grupo em dois
            (grupo1, grupo2) = dividirGrupo grupoMaior ligacoes
            -- Substitui o grupo antigo pelos dois novos
            novosGrupos = grupo1 : grupo2 : filter (/= grupoMaior) grupos
        in formarGrupos k novosGrupos ligacoes

-- Encontra o grupo que contém a maior ligação interna
encontrarGrupoComMaiorLigacao :: [[Int]] -> [Ligacao] -> ([Int], Ligacao)
encontrarGrupoComMaiorLigacao grupos ligacoes =
    let ligacoesPorGrupo = [ (grupo, filter (\(p1, p2, _) -> pertenceAoGrupo p1 grupo && pertenceAoGrupo p2 grupo) ligacoes) | grupo <- grupos ]
        -- Filtra grupos com ligações (evita grupos unitários)
        gruposComLigacoes = filter (\(_, lgs) -> not (null lgs)) ligacoesPorGrupo
        -- Encontra o grupo com a maior ligação
        (grupo, lgs) = if null gruposComLigacoes
                        then error "Não há ligações para dividir"
                        else foldl1 (\acc@(_, lgs1) (g, lgs2) -> if maiorLigacao lgs2 > maiorLigacao lgs1 then (g, lgs2) else acc) gruposComLigacoes
    in (grupo, maiorLigacao lgs)

----------------------------------------------------------
-- Função para imprimir os grupos (apenas números dos pontos)
----------------------------------------------------------

imprimirGrupos :: [[Int]] -> IO ()
imprimirGrupos grupos = do
    putStrLn "Grupos finais:"
    mapM_ (\grupo -> putStrLn $ "Grupo: " ++ show grupo) grupos