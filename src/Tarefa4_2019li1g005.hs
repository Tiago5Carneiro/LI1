-- | Este módulo define funções comuns da Tarefa 4 do trabalho prático.
module Tarefa4_2019li1g005 where

import Tarefa2_2019li1g005
import LI11920
import Tarefa0_2019li1g005
import Textes

-- * Testes
-- | Testes unitários da Tarefa 4.
--
-- Cada teste é um par (/tempo/,/'Mapa'/,/'Jogador'/).
testesT4 :: [(Double,Mapa,Jogador)]
testesT4 = testeT4

-- * Funções principais da Tarefa 4.

-- | Avança o estado de um 'Jogador' um 'passo' em frente, durante um determinado período de tempo.
passo :: Double -- ^ O tempo decorrido.
     -> Mapa    -- ^ O mapa utilizado.
     -> Jogador -- ^ O estado anterior do 'Jogador'.
     -> Jogador -- ^ O estado do 'Jogador' após um 'passo'.
passo t m j = move t m (acelera t m j)

-- | Altera a velocidade de um 'Jogador', durante um determinado período de tempo.
acelera :: Double -- ^ O tempo decorrido.
     -> Mapa    -- ^ O mapa utilizado.
     -> Jogador -- ^ O estado anterior do 'Jogador'.
     -> Jogador -- ^ O estado do 'Jogador' após acelerar.
acelera t m j | (estaChao estPlayer) = let accelMota = if v < 2 then 1.0 else 0.0
                                           (x,y) = (pistaJogador j,floor (distanciaJogador j))
                                           atrito = pecaAtrito ((m !! x) !! y)
                                           in if (aceleraJogador estPlayer) then j{velocidadeJogador = max 0 (v + (accelMota - atrito*v)*t)} 
                                                                            else j{velocidadeJogador = max 0 (v - atrito*v*t)}
              | estaAr estPlayer = let resistAr = 0.125
                                       accelGrav = 1.0
                                       g = gravidadeJogador estPlayer
                                       in j{velocidadeJogador = v - (v * resistAr * t),estadoJogador = (estPlayer{gravidadeJogador = g + accelGrav*t})}
              | otherwise = j
              where estPlayer = estadoJogador j
                    v = velocidadeJogador j
      
-- | Altera a posição de 'Jogador', durante um determinado período de tempo.
move :: Double -- ^ O tempo decorrido.
     -> Mapa    -- ^ O mapa utilizado.
     -> Jogador -- ^ O estado anterior do 'Jogador'.
     -> Jogador -- ^ O estado do 'Jogador' após se movimentar.
move t m j | estaMorto estPlayer = let tm = timeoutJogador estPlayer 
                                   in if tm >= t then j{estadoJogador = estPlayer{timeoutJogador = tm - t}} else  j{estadoJogador = Chao False,velocidadeJogador = 0}
           | estaChao estPlayer = whenFloor peca peca1 t j 
           | estaAr estPlayer = whenAr peca t j
           where estPlayer = estadoJogador j
                 (x,y) = (floor (distanciaJogador j),pistaJogador j)
                 pista = (m!!y)
                 peca = pista!!x
                 peca1 = pista!!(x+1) 

whenFloor :: Peca -> Peca -> Double -> Jogador -> Jogador
whenFloor peca peca1 t j| (d + distr) > lim = if incl > incl1 then (j{distanciaJogador = lim,estadoJogador = Ar (alt peca) (incl*180/pi) 0})
                                                              else j{distanciaJogador = lim}
                        | otherwise = j{distanciaJogador = (d+distr)}
                        where d = distanciaJogador j
                              incl = inclin peca
                              incl1 = inclin peca1
                              dist = t * (velocidadeJogador j)  
                              distr = dist * (cos incl)
                              lim = (fromIntegral(floor d)) + 1

whenAr :: Peca -> Double -> Jogador -> Jogador
whenAr peca t j |intersetam reta1 reta2 = let incl = (inclin peca)*(pi)/180 
                                              ponto = intersecao reta1 reta2
                                              x = posx ponto
                                          in if  abs (incl - (inclinacaoJogador estPlayer)) <= 45 
                                             then j{distanciaJogador = x,estadoJogador = Chao False}
                                             else j{distanciaJogador = x,estadoJogador = Morto 1.0}
                | (posx dist)+dp > (d+1) = j{distanciaJogador = (d+1),estadoJogador=estPlayer{alturaJogador = ((dp+1)/((posx dist)+dp))*(posy dist)+ altu}} 
                | otherwise = j{distanciaJogador = (posx dist) + dp,estadoJogador=estPlayer{alturaJogador = (posy dist) + altu}}
                where estPlayer = estadoJogador j
                      player = Cartesiano (distanciaJogador j) (alturaJogador estPlayer)
                      vel = Polar (velocidadeJogador j) (inclinacaoJogador estPlayer)
                      grav = Polar (gravidadeJogador estPlayer) (-90)  
                      velr = somaVetores vel grav
                      dist = multiplicaVetor t velr
                      reta1 = (somaVetores dist player,player)
                      dp = distanciaJogador j
                      d = fromIntegral(floor dp)
                      reta2 = ((Cartesiano (d-0.1) (alti peca)),(Cartesiano (d+1) (alt peca)))
                      altu = alturaJogador estPlayer

pecaAtrito :: Peca -> Double
pecaAtrito x = aux (aux1 x)
             where aux Terra = 0.25
                   aux Lama = 1.50
                   aux Boost = (-0.50)
                   aux Relva = 0.75
                   aux Cola = 3.00
                   aux1 (Rampa p _ _) = p
                   aux1 (Recta p _) = p

alt :: Peca -> Double
alt (Rampa _ _ x) = fromIntegral x
alt (Recta _ x) = fromIntegral x

alti :: Peca -> Double
alti (Rampa _ x _) = fromIntegral x
alti (Recta _ x) = fromIntegral x