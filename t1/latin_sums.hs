import Data.Maybe (listToMaybe, maybeToList)

-- tipos de celulas
data Celula = Vazio
            | Resposta Int
            | Soma Int

instance Eq Celula where
    Vazio == Vazio = True
    (Soma a) == (Soma b) = a == b
    (Resposta a) == (Resposta b) = a == b
    _ == _ = False

type Tabuleiro = [[Celula]]
tabuleiro :: Tabuleiro

{--
-- tabuleiro 227
tabuleiro = [[Vazio, Soma 5, Soma 11, Vazio, Soma 7, Soma 3, Vazio, Vazio],
             [Soma 1, Soma 8, Vazio, Vazio, Soma 13, Soma 7, Vazio, Vazio],
             [Soma 0, Soma 9, Vazio, Vazio, Soma 17, Vazio, Soma 11, Vazio],
             [Soma 4, Soma 10, Vazio, Vazio, Vazio, Vazio, Soma 10, Soma 1],
             [Soma 11, Vazio, Vazio, Soma 13, Vazio, Vazio, Soma 6, Soma 0],
             [Vazio, Vazio, Soma 9, Soma 6, Vazio, Vazio, Soma 10, Soma 7],
             [Vazio, Vazio, Soma 6, Soma 3, Soma 4, Soma 11, Vazio, Vazio],
             [Vazio, Vazio, Soma 3, Soma 1, Vazio, Soma 8, Vazio, Soma 11]
            ]

numerosPossiveis :: [Int]
numerosPossiveis = [1, 2, 3, 4]
--}

-- tabuleiro 59
tabuleiro = [[Vazio, Soma 11, Soma 9, Vazio, Vazio, Soma 13, Vazio, Vazio],
             [Vazio, Vazio, Vazio, Soma 15, Soma 16, Vazio, Vazio, Soma 13],
             [Soma 15, Vazio, Soma 14, Vazio, Vazio, Soma 18, Vazio, Vazio],
             [Vazio, Soma 16, Vazio, Soma 16, Vazio, Vazio, Soma 18, Vazio],
             [Soma 11, Vazio, Vazio, Vazio, Soma 16, Vazio, Vazio, Soma 11],
             [Vazio, Vazio, Vazio, Soma 15, Soma 12, Vazio, Soma 18, Vazio],
             [Soma 11, Vazio, Vazio, Vazio, Vazio, Soma 12, Vazio, Soma 11],
             [Vazio, Soma 11, Soma 16, Vazio, Vazio, Vazio, Soma 7, Vazio]
            ]

-- valores possiveis para respostas
numerosPossiveis :: [Int]
numerosPossiveis = [1, 2, 3, 4, 5]

vizinhosOffset :: [(Int, Int)]
vizinhosOffset = [(-1, -1), (-1, 0), (-1, 1),
                   (0, -1),           (0, 1),
                   (1, -1),  (1, 0),  (1, 1)]

-- quais celulas pretas sao vizinhas de uma celula branca
somasAoRedor :: Tabuleiro -> Int -> Int -> [(Int, Int)]
somasAoRedor tab i j =
    [(ni, nj) | (di, dj) <- vizinhosOffset,
     let ni = i + di,
     let nj = j + dj,
     ni >= 0 && ni < length tab,
     nj >= 0 && nj < length (head tab),
     ehSoma (tab !! ni !! nj)]

    where
        ehSoma (Soma _) = True
        ehSoma _ = False

-- verifica se o numero ja esta na linha ou coluna
validadeRepeticao :: Tabuleiro -> Int -> Int -> Int -> Bool
validadeRepeticao tab i j val =
    not (existeNaLinha || existeNaColuna)

    where
        linhaSemAtual = [tab !! i !! k | k <- [0..(length tab - 1)], k /= j]
        colunaSemAtual = [tab !! k !! j | k <- [0..(length tab - 1)], k /= i]

        contemValor lista = any (\celula -> case celula of 
                                            Resposta v -> v == val
                                            _ -> False) lista

        existeNaLinha = contemValor linhaSemAtual
        existeNaColuna = contemValor colunaSemAtual

-- verifica se a soma das ajcencias de celulas pretas nao ultrapassa
validadeCelulaSoma :: Tabuleiro -> Int -> Int -> Bool
validadeCelulaSoma tab i j =
    all verificaSoma (somasAoRedor tab i j)

    where
        verificaSoma (ni, nj) =
            let
                celulaSoma = valorCelula (tab !! ni !! nj)
                vizinhos = [tab !! si !! sj | (di, dj) <- vizinhosOffset,
                            let si = ni + di,
                            let sj = nj + dj,
                            si >= 0 && si < length tab,
                            sj >= 0 && sj < length (head tab),
                            not (ehSoma (tab !! si !! sj))]

                somaTotal = sum [v | Resposta v <- vizinhos]
                temVazio = any (== Vazio) vizinhos
            in
                if temVazio
                then somaTotal < celulaSoma
                else somaTotal == celulaSoma

        ehSoma (Soma _) = True
        ehSoma _ = False

        valorCelula (Soma v) = v
        valorCelula _ = 0

-- achar primeiro vazio do tab                      
primeiroVazio :: Tabuleiro -> Maybe (Int, Int)
primeiroVazio tab = listToMaybe [(i, j) | i <- [0..(length tab - 1)],
                                 j <- [0..(length (head tab) - 1)],
                                 tab !! i !! j == Vazio]

-- atualiza vazio pelo novo numero
atualizar :: Int -> Int -> Int -> Tabuleiro -> Tabuleiro
atualizar i j val tab =
    [[if (x == i && y == j) then Resposta val else (tab !! x !! y) 
      | y <- [0..(length (head tab) - 1)]]
      | x <- [0..(length tab - 1)]]

-- backtracking
resolver :: Tabuleiro -> Maybe Tabuleiro
resolver tab =
    case primeiroVazio tab of
        Nothing -> Just tab
        Just (i, j) ->
            let
                tentativas = [novoTabuleiro | val <-numerosPossiveis,
                              let candidato = atualizar i j val tab,
                              validadeRepeticao candidato i j val,
                              validadeCelulaSoma candidato i j,
                              novoTabuleiro <- maybeToList (resolver candidato)]
            in
                listToMaybe tentativas

-- impressão
imprimir :: Tabuleiro -> IO()
imprimir [] = return ()
imprimir (linha:resto) = do
    imprimirLinha linha
    imprimir resto

imprimirLinha :: [Celula] -> IO()
imprimirLinha [] = putStrLn ""
imprimirLinha (celula:resto) = do
    let conteudo = formatar celula
    putStr (conteudo ++ "        ")
    imprimirLinha resto

    where
        formatar (Soma v) =  show v 
        formatar (Resposta v) = show v
        formatar Vazio = "V"

main :: IO()
main = do
    case resolver tabuleiro of
        Just tabResolvido -> do
            imprimir tabResolvido
        Nothing -> do
            putStrLn "solução nao encontrada"