main = do
    content <- readFile "exe.txt"
    mapM_ putStrLn $ map resolverPasseio $ map processarLinha $ lines content

--Recebe a linha lida do arquivo e retorna uma tupla com n, m e a posição (x,y)
processarLinha :: String -> [Int]
processarLinha linha = 
    map read $ words linha :: [Int]

    
resolverPasseio :: [Int] -> String
resolverPasseio [n, m, x, y] =
    "Tabuleiro " ++ show n ++ "x" ++ show m ++ " de (" ++ show x ++ ", " ++ show y ++ ")\n"