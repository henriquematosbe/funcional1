module Popularidade
  ( tabela,
    pesquisa,
    go,
    main,
  )
where

import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)
import Text.Printf (printf)

-- | Converte o conteúdo de um arquivo de nomes em uma tabela de dados.
-- Cada linha tem o formato "Nome quantidade"; o ranking começa em 1.
tabela :: String -> [(Int, String, Int)]
tabela texto = numera 1 (lines texto)
  where
    numera :: Int -> [String] -> [(Int, String, Int)]
    numera _ [] = []
    numera n (linha : resto) =
      case words linha of
        [nome, quantidade] -> (n, nome, read quantidade) : numera (n + 1) resto
        _ -> error "Formato de registro invalido"

-- | Pesquisa um nome em uma tabela e imprime o resultado.
pesquisa :: String -> [(Int, String, Int)] -> String -> IO ()
pesquisa tipo [] nome =
  printf "%s não encontrado entre %s\n" nome tipo
pesquisa tipo ((posicao, n, registros) : resto) nome
  | n == nome =
      printf
        "%s classificado na posição %d entre %s com %d registros\n"
        nome
        posicao
        tipo
        registros
  | otherwise = pesquisa tipo resto nome

-- | Ação recursiva que implementa o laço de interação com o usuário.
go :: [(Int, String, Int)] -> [(Int, String, Int)] -> IO ()
go tabGarotos tabGarotas = do
  putStr "Nome: "
  nome <- getLine
  if null nome
    then return ()
    else do
      pesquisa "garotas" tabGarotas nome
      pesquisa "garotos" tabGarotos nome
      go tabGarotos tabGarotas

-- | Ação principal que orquestra o programa.
main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  textoGarotos <- readFile "boynames.txt"
  textoGarotas <- readFile "girlnames.txt"
  go (tabela textoGarotos) (tabela textoGarotas)
