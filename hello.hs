import Data.Array

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
    | not $ existeCaminho n m = False
    | otherwise =
        let tabuleiro = criarTabuleiro n m
        in True

existeCaminho :: Int -> Int -> Bool
existeCaminho n m
    | n > m = existeCaminho m n           
    | n == 3 && (m == 4 || m >= 7) = True 
    | n >= 4 && m >= 5 = True             
    | otherwise = False  

criarTabuleiro :: Int -> Int -> Array (Int, Int) Bool
criarTabuleiro n m = 
    listArray ((0, 0), (n-1, m-1)) $ repeat False


