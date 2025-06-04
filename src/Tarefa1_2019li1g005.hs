-- | Este módulo define funções comuns da Tarefa 2 do trabalho prático.
module Tarefa1_2019li1g005 where

import LI11920
import System.Random

-- * Testes

-- | Testes unitários da Tarefa 1.
--
-- Cada teste é um triplo (/número de 'Pista's/,/comprimento de cada 'Pista' do 'Mapa'/,/semente de aleatoriedades/).
testesT1 :: [(Int,Int,Int)]
testesT1 = [(2,10,511),(6,15,2000),(5,800,9),(1,1,500)]

-- * Funções pré-definidas da Tarefa 1.

geraAleatorios :: Int -> Int -> [Int]
geraAleatorios n seed = take n (randomRs (0,9) (mkStdGen seed))

-- * Função principal da Tarefa 1.

-- | Função principal que chama todas as outras de forma a criar o mapa aleatório
gera :: Int -> Int -> Int -> Mapa
gera npistas comprimento seed = geraMapa npistas (geraAleatorios (2 * npistas * (comprimento - 1)) seed ) comprimento


-- ** Funçôes secundária

-- | Função que vai chamando a funçao geraPista com os pedaços correspondentes da lista de numeros aletorios gerados na função gera
geraMapa :: Int -> [Int] -> Int -> Mapa
geraMapa pistas l comprimento | pistas <= 0 = []  
                              | otherwise = let (l1,l2) = splitAt ((comprimento - 1) * 2) l
                                            in (geraPista 0 Terra (0:6:l1):geraMapa (pistas - 1) l2 comprimento ) 


-- | Função que dá o tipo de terreno pretendido
terreno :: Int -> Piso -> Piso
terreno x piso | x <= 1 = Terra
               | x <= 3 = Relva
               | x == 4 = Lama 
               | x == 5 = Boost 
               | otherwise = piso

-- | Função que transforma a lista de pares correspondentes a uma pista numa pista    
geraPista :: Int -> Piso -> [Int] -> Pista
geraPista _ _ [] = []
geraPista h piso (x:y:t)   | y <= 1 = (Rampa terr h (h + y + 1)):(geraPista (h + y + 1) terr t)
                           | y <= 5 = if h == 0 then (Recta terr h):geraPista h terr t else if h1 < 0 then (Rampa terr h 0:geraPista 0 terr t) else (Rampa terr h h1:geraPista h1 terr t)
                           | y <= 9 = (Recta terr h):geraPista h terr t
                           where terr = terreno x piso 
                                 h1 = h-(y-1)

