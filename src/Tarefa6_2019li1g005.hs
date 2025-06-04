-- | Este módulo define funções comuns da Tarefa 6 do trabalho prático.
module Tarefa6_2019li1g005 where

import Tarefa2_2019li1g005
import Tarefa4_2019li1g005
import LI11920


-- * Funções principais da Tarefa 6.

-- | Define um ro'bot' capaz de jogar autonomamente o jogo.
bot :: Int          -- ^ O identificador do 'Jogador' associado ao ro'bot'.
    -> Estado       -- ^ O 'Estado' para o qual o ro'bot' deve tomar uma decisão.
    -> Maybe Jogada -- ^ Uma possível 'Jogada' a efetuar pelo ro'bot'.
bot x e = analize ((jogadoresEstado e) !! x) (mapaEstado e)
 
fr = 50

analize :: Jogador -> Mapa -> Maybe Jogada
analize j mp | estaMorto ej = Nothing 
             | estaAr ej = undefined
             | estaChao ej = if moveTo distP pista peca 0 < intPista then (Just Movimenta C) else if moveTo distP pista peca 0 > intPista then (Just Movimenta B) else Nothing
              where distP = (distanciaJogador j - (floor (distanciaJogador j)))
              	    ej = estadoJogador j
                    intPist = pistaJogador j
                    pista = mp !! intPista
                    peca =  pista !! (floor (distanciaJogador j))
                    pecaA = (mp !! ((floor (distanciaJogador j))-1) !! (intPista -1))
angleH :: EstadoJogador -> Peca -> Maybe Jogada 
angleH ej peca = undefined

moveTo :: Int -> [Peca] -> Peca -> Int -> Int
moveTo [] x y = y
moveTo (h:t) x y | melhorP x h = moveTo t x y+1
                 | otherwise = moveTo t h y+1
 
melhorP :: Int -> Peca -> Peca -> Bool
melhorP x p p1 | pecaAtrito p <= pecaAtrito p1 && ((alturas p1 x)-(alturas p x)<0.2) = True
               | otherwise = False

eBoost :: Peca -> Bool
eBoost (Recta Boost _) = True
eBoost (Rampa Boost _ _) = True
eBoost _ = False

distP :: Int -> Estado -> Int 
distP x e = distanciaJogador ((jogadoresEstado e) !! x)