import Data.Array
import Data.Function ((&))
import Data.List (sortBy)
import Data.Ord (comparing)

main = do
    content <- readFile "exe.txt"
    mapM_ putStrLn $ map resolverPasseio $ map processarLinha $ lines content

--Recebe a linha lida do arquivo e retorna uma tupla com n, m e a posição (x,y)
processarLinha :: String -> [Int]
processarLinha linha = 
    map read $ words linha :: [Int]

    
resolverPasseio :: [Int] -> String
resolverPasseio [n, m, x, y]
    | not (existeCaminho n m) = prefix ++ "Não existe caminho\n"
    | backtrack n m x y (criarTabuleiro n m) 0            = prefix ++ "Existe caminho\n"
    | otherwise               = prefix ++ "Não existe caminho\n"
  where
    prefix = "Tabuleiro " ++ show n ++ "x" ++ show m ++ " de (" ++ show x ++ ", " ++ show y ++ ")\n"

backtrack :: Int  ->  Int  ->  Int  ->  Int -> Array (Int, Int) Bool -> Int ->  Bool 
backtrack n m x y tabuleiro iteracoes
    | iteracoes == (n*m - 1) = True
    | tabuleiro ! (x,y) = False -- Posição já visitada
    | otherwise = 
        let new_tabuleiro = visitaPosicao tabuleiro (x, y)
        in any (\(new_x, new_y) -> backtrack n m new_x new_y new_tabuleiro (iteracoes + 1)) $ movimentosValidos n m new_tabuleiro (x,y)

--Retorna um novo tabuleiro com a posição dada como True
visitaPosicao :: Array (Int, Int) Bool -> (Int, Int) -> Array (Int, Int) Bool
visitaPosicao tab pos = tab // [(pos, True)]

desmarcaPosicao :: Array (Int, Int) Bool -> (Int, Int) -> Array (Int, Int) Bool
desmarcaPosicao tab pos = tab // [(pos, False)]

-- gera os 8 movimentos possíveis a partir de uma posição
movimentosPossiveis :: (Int, Int) -> [(Int, Int)]
movimentosPossiveis (x, y) =
    [ (x+2,y+1), (x+2,y-1), (x-2,y+1), (x-2,y-1), (x+1,y+2), (x+1,y-2), (x-1,y+2), (x-1,y-2)]

-- verifica se posição está dentro do tabuleiro e não visitada
ehValido :: Int -> Int -> Array (Int, Int) Bool -> (Int, Int) -> Bool
ehValido n m tab (x, y)
    | x < 0 || x >= n || y < 0 || y >= m = False
    | otherwise = not (tab ! (x, y))

-- conta movimentos futuros (heurística simples)
contarMovimentosFuturos :: Int -> Int -> Array (Int, Int) Bool -> (Int, Int) -> Int
contarMovimentosFuturos n m tab pos =
    length . filter (ehValido n m tab) $ movimentosPossiveis pos
 

-- movimentos válidos ordenados por Warnsdorff (menor número de movimentos futuros primeiro)
movimentosValidos :: Int -> Int -> Array (Int, Int) Bool -> (Int, Int) -> [(Int, Int)]
movimentosValidos n m tab pos =
    sortBy (comparing (contarMovimentosFuturos n m tab)) . filter (ehValido n m tab) $ movimentosPossiveis pos

existeCaminho :: Int -> Int -> Bool
existeCaminho n m
    | n > m = existeCaminho m n           
    | n == 3 && (m == 4 || m >= 7) = True 
    | n >= 4 && m >= 5 = True             
    | otherwise = False  

criarTabuleiro :: Int -> Int -> Array (Int, Int) Bool
criarTabuleiro n m = 
    listArray ((0, 0), (n-1, m-1)) $ repeat False


