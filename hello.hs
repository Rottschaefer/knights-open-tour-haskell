-- Imports necessários
import Data.Array           -- Para trabalhar com arrays imutáveis (acesso O(1))
import Data.List (sortBy)   -- Para ordenar listas
import Data.Ord (comparing) -- Para comparar elementos ao ordenar

{- MAIN: Ponto de entrada do programa
   Lê o arquivo exe.txt, processa cada linha e imprime o resultado -}
main :: IO ()
main = do
    content <- readFile "exe.txt"  -- Lê o arquivo (lazy)
    mapM_ putStrLn $ map resolverPasseio $ map processarLinha $ lines content
    -- Pipeline funcional:
    -- lines content         -> separa em linhas
    -- map processarLinha    -> converte cada linha em [Int]
    -- map resolverPasseio   -> resolve cada caso
    -- mapM_ putStrLn        -> imprime cada resultado

{- PROCESSAMENTO DE ENTRADA
   Converte uma linha "n m x y" em lista [n, m, x, y]
   Exemplo: "8 8 0 0" -> [8, 8, 0, 0] -}
processarLinha :: String -> [Int]
processarLinha linha = 
    map read $ words linha :: [Int]
    -- words linha    -> separa por espaços: ["8", "8", "0", "0"]
    -- map read       -> converte para Int: [8, 8, 0, 0]

    
{- RESOLVER PASSEIO DO CAVALO
   Verifica se existe um caminho aberto no tabuleiro nxm partindo de (x,y)
   Retorna string com resultado formatado -}
resolverPasseio :: [Int] -> String
resolverPasseio [n, m, x, y]
    | not (existeCaminho n m) = prefix ++ "impossible\n"
        -- Verifica condição teórica de existência (do artigo)
    | backtrack n m x y (x, y) (criarTabuleiro n m) 0 = prefix ++ "Existe caminho\n"
        -- Tenta encontrar caminho via backtracking
    | otherwise = prefix ++ "impossible\n"
        -- Falhou em encontrar caminho
  where
    prefix = "Tabuleiro " ++ show n ++ "x" ++ show m ++ " de (" ++ show x ++ ", " ++ show y ++ ")\n"

{- BACKTRACKING - Coração do algoritmo
   Parâmetros:
   - n, m: dimensões do tabuleiro
   - x, y: posição atual
   - posInicial: posição de partida (para verificar caminho aberto no final)
   - tabuleiro: array com posições visitadas (True = visitada)
   - iteracoes: contador de casas visitadas
   
   Retorna True se encontrou caminho aberto, False caso contrário -}
backtrack :: Int -> Int ->  Int -> Int ->  (Int,  Int) -> Array (Int, Int) Bool -> Int ->  Bool 
backtrack n m x y posInicial tabuleiro iteracoes
    | (iteracoes == (n*m-1) && naoVoltaProInicio (x, y) posInicial) = True
        -- Visitou todas as casas E é caminho aberto
        -- (última posição NÃO pode alcançar a primeira)
    | tabuleiro ! (x,y) = False
        -- Posição já visitada: caminho inválido
    | otherwise = 
        let new_tabuleiro = visitaPosicao tabuleiro (x, y)
            -- Marca posição atual como visitada (cria novo array)
        in any (\(new_x, new_y) -> backtrack n m new_x new_y posInicial new_tabuleiro (iteracoes + 1)) $ 
           movimentosValidos n m new_tabuleiro (x,y)
            -- any: para na primeira recursão que retornar True
            -- testa cada movimento válido recursivamente

-- Verifica se NÃO volta para o início (caminho aberto)
{- VERIFICAÇÃO DE CAMINHO ABERTO
   Verifica se a posição final NÃO consegue alcançar a posição inicial
   em um único movimento de cavalo. Se não conseguir, é caminho aberto. -}
naoVoltaProInicio :: (Int, Int) -> (Int, Int) -> Bool
naoVoltaProInicio posFinal posInicial = 
    notElem posInicial (movimentosPossiveis posFinal)
    -- Gera todos os 8 movimentos possíveis da posição final
    -- e verifica se a posição inicial NÃO está entre eles

{- MANIPULAÇÃO DO TABULEIRO
   Arrays em Haskell são imutáveis, então "modificar" na verdade
   cria um novo array com a posição alterada (operador //) -}
   
-- Marca uma posição como visitada (True)
visitaPosicao :: Array (Int, Int) Bool -> (Int, Int) -> Array (Int, Int) Bool
visitaPosicao tab pos = tab // [(pos, True)]

-- Desmarca uma posição (False) - não usado atualmente
desmarcaPosicao :: Array (Int, Int) Bool -> (Int, Int) -> Array (Int, Int) Bool
desmarcaPosicao tab pos = tab // [(pos, False)]

{- MOVIMENTOS DO CAVALO
   Gera todas as 8 posições que um cavalo pode alcançar
   a partir da posição (x,y) seguindo as regras do xadrez:
   movimento em "L" (2 casas em uma direção + 1 casa perpendicular) -}
movimentosPossiveis :: (Int, Int) -> [(Int, Int)]
movimentosPossiveis (x, y) =
    [ (x+2,y+1), (x+2,y-1),  -- 2 para direita
      (x-2,y+1), (x-2,y-1),  -- 2 para esquerda
      (x+1,y+2), (x+1,y-2),  -- 2 para cima/baixo
      (x-1,y+2), (x-1,y-2) ] -- 2 para cima/baixo (inverso)

{- VALIDAÇÃO DE POSIÇÃO
   Verifica se uma posição é válida para o próximo movimento:
   1. Está dentro dos limites do tabuleiro
   2. Ainda não foi visitada -}
ehValido :: Int -> Int -> Array (Int, Int) Bool -> (Int, Int) -> Bool
ehValido n m tab (x, y)
    | x < 0 || x >= n || y < 0 || y >= m = False  -- Fora do tabuleiro
    | otherwise = not (tab ! (x, y))               -- Não visitada (False no array)

{- HEURÍSTICA DE WARNSDORFF
   Conta quantos movimentos válidos existem a partir de uma posição.
   Usado para ordenar: tentamos primeiro posições com MENOS opções futuras
   (reduz ramificação do backtracking) -}
contarMovimentosFuturos :: Int -> Int -> Array (Int, Int) Bool -> (Int, Int) -> Int
contarMovimentosFuturos n m tab pos =
    length . filter (ehValido n m tab) $ movimentosPossiveis pos
    -- Pipeline: gera movimentos -> filtra válidos -> conta quantos
 
{- MOVIMENTOS VÁLIDOS COM HEURÍSTICA
   Retorna lista de movimentos válidos ordenados pela heurística de Warnsdorff:
   posições com menos movimentos futuros vêm primeiro.
   Isso melhora drasticamente a performance do backtracking. -}
movimentosValidos :: Int -> Int -> Array (Int, Int) Bool -> (Int, Int) -> [(Int, Int)]
movimentosValidos n m tab pos =
    sortBy (comparing (contarMovimentosFuturos n m tab)) . filter (ehValido n m tab) $ movimentosPossiveis pos
    -- Pipeline:
    -- movimentosPossiveis pos  -> gera 8 movimentos
    -- filter (ehValido ...)     -> remove inválidos (fora ou visitados)
    -- sortBy (comparing ...)    -> ordena por número de movimentos futuros (crescente)

{- CONDIÇÃO TEÓRICA DE EXISTÊNCIA
   Baseado no artigo acadêmico sobre Knight's Tour.
   Verifica se TEORICAMENTE existe solução para um tabuleiro n×m.
   
   Condições (assumindo n ≤ m):
   1. n = 3 e (m = 4 ou m ≥ 7), OU
   2. n ≥ 4 e m ≥ 5
   
   Esta é uma verificação rápida antes de tentar backtracking. -}
existeCaminho :: Int -> Int -> Bool
existeCaminho n m
    | n > m = existeCaminho m n            -- Garante n ≤ m (inverte se necessário)
    | n == 3 && (m == 4 || m >= 7) = True  -- Caso especial: 3×4, 3×7, 3×8...
    | n >= 4 && m >= 5 = True              -- Caso geral: mínimo 4×5
    | otherwise = False                    -- Impossível (ex: 2×3, 3×3)

{- CRIAÇÃO DO TABULEIRO
   Cria um array 2D de booleanos inicializado com False.
   - Índices: de (0,0) até (n-1, m-1)
   - Valores: todos False (nenhuma posição visitada)
   - Acesso: O(1) com operador ! 
   
   listArray pega uma lista infinita (repeat False) e usa apenas
   os primeiros n×m elementos. -}
criarTabuleiro :: Int -> Int -> Array (Int, Int) Bool
criarTabuleiro n m = 
    listArray ((0, 0), (n-1, m-1)) $ repeat False


