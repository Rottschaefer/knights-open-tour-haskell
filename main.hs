import Data.List (sortBy, delete)
import Data.Ord (comparing)

-- Lê casos de teste do arquivo exe.txt e resolve cada um
main :: IO ()
main = do
    content <- readFile "exe.txt"
    mapM_ putStrLn $ map resolverPasseio $ map processarLinha $ lines content

-- Converte linha "n m x y" em lista [n, m, x, y]
processarLinha :: String -> [Int]
processarLinha linha = map read $ words linha :: [Int]

-- Resolve um caso: verifica existência teórica e tenta backtracking
resolverPasseio :: [Int] -> String
resolverPasseio [n, m, x, y]
    | not (existeCaminho n m) = prefix ++ "impossible\n"
    | otherwise =
        let caminho_encontrado = backtrack n m x y (x, y) [] 0 
        in if null caminho_encontrado
           then prefix ++ "impossible\n"
           else prefix ++ (formatarCaminho $ reverse caminho_encontrado) ++ "\n"
    where
        prefix = "Tabuleiro " ++ show n ++ "x" ++ show m ++ " de (" ++ show x ++ ", " ++ show y ++ ")\n"


-- Backtracking recursivo: tenta construir caminho visitando todas as casas
-- Retorna lista de posições do caminho ou [] se falhar
backtrack :: Int -> Int -> Int -> Int -> (Int, Int) -> [(Int,Int)] -> Int -> [(Int,Int)]
backtrack n m x y posInicial visitadas iteracoes
    -- Sucesso: visitou todas as casas E não volta pro início
    | (iteracoes == (n*m-1) && naoVoltaProInicio (x, y) posInicial) =
        visitaPosicao visitadas (x, y)
    -- Falha: já visitou essa posição
    | (x, y) `elem` visitadas = []
    -- Recursão: tenta próximos movimentos
    | otherwise =
        let new_visitadas = visitaPosicao visitadas (x, y)
            moves = movimentosValidos n m new_visitadas (x,y)
            recursive_results = map (\(new_x, new_y) -> 
                                  backtrack n m new_x new_y posInicial new_visitadas (iteracoes + 1)) 
                                moves
            successful_paths = filter (not . null) recursive_results
        in headOrDefault [] successful_paths

-- Verifica se posição final NÃO alcança a inicial (caminho aberto)
naoVoltaProInicio :: (Int, Int) -> (Int, Int) -> Bool
naoVoltaProInicio posFinal posInicial =
    notElem posInicial (movimentosPossiveis posFinal)

-- Adiciona posição à lista de visitadas
visitaPosicao :: [(Int,Int)] -> (Int, Int) -> [(Int,Int)]
visitaPosicao visitadas pos = pos : visitadas

-- Gera 8 movimentos possíveis do cavalo em forma de "L"
movimentosPossiveis :: (Int, Int) -> [(Int, Int)]
movimentosPossiveis (x, y) =
    [ (x+2,y+1), (x+2,y-1), (x-2,y+1), (x-2,y-1),
      (x+1,y+2), (x+1,y-2), (x-1,y+2), (x-1,y-2) ]

-- Verifica se posição é válida: dentro do tabuleiro e não visitada
ehValido :: Int -> Int -> [(Int,Int)] -> (Int, Int) -> Bool
ehValido n m visitadas (x, y)
    | x < 0 || x >= n || y < 0 || y >= m = False
    | otherwise = not ((x, y) `elem` visitadas)

-- Heurística de Warnsdorff: conta movimentos válidos futuros
contarMovimentosFuturos :: Int -> Int -> [(Int,Int)] -> (Int, Int) -> Int
contarMovimentosFuturos n m visitadas pos =
    length . filter (ehValido n m visitadas) $ movimentosPossiveis pos

-- Retorna movimentos válidos ordenados (menos opções primeiro)
movimentosValidos :: Int -> Int -> [(Int,Int)] -> (Int, Int) -> [(Int, Int)]
movimentosValidos n m visitadas pos =
    sortBy (comparing (contarMovimentosFuturos n m visitadas)) . 
    filter (ehValido n m visitadas) $ movimentosPossiveis pos

-- Lin & Wei (2005, Discrete Applied Math.): para n≤m, há passeio ABERTO sse (n=3 ∧ (m=4 ∨ m≥7)) ∨ (n≥4 ∧ m≥5).
existeCaminho :: Int -> Int -> Bool
existeCaminho n m
    | n > m = existeCaminho m n
    | n == 3 && (m == 4 || m >= 7) = True
    | n >= 4 && m >= 5 = True
    | otherwise = False

-- Retorna primeiro elemento ou valor padrão se lista vazia
headOrDefault :: a -> [a] -> a
headOrDefault defaultVal [] = defaultVal
headOrDefault _ (x:_) = x

-- Formata caminho para impressão: [(0,0), (1,2)] -> "(0,0) (1,2)"
formatarCaminho :: [(Int, Int)] -> String
formatarCaminho xs = unwords $ map show xs