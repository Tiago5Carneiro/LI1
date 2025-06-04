module Tarefa2_2019li1g005v1 where

import Tarefa0_2019li1g005v1
import LI11920v1

-- * Testes

-- | Testes unitários da Tarefa 2.
--
-- Cada teste é um triplo (/identificador do 'Jogador'/,/'Jogada' a efetuar/,/'Estado' anterior/).

-- * Funções principais da Tarefa 2.

-- | Efetua uma jogada.
jogada :: Int -- ^ O identificador do 'Jogador' que efetua a jogada.
       -> Jogada -- ^ A 'Jogada' a efetuar.
       -> Estado -- ^ O 'Estado' anterior.
       -> Estado -- ^ O 'Estado' resultante após o jogador efetuar a jogada.
jogada id j e | estaMorto estadoPlayer = e
              | j == Acelera =  e {jogadoresEstado = (atualizaIndiceLista id (player {estadoJogador = aceleraT2 estadoPlayer}) players)}
              | j == Desacelera = e {jogadoresEstado = (atualizaIndiceLista id (player {estadoJogador = desacelera estadoPlayer}) players)} --Estado {mapaEstado,jogadoresEstado}
              | j == Dispara = let (mapa1 , player1) = (dispara mapa player)
                                   jogadores = (atualizaIndiceLista id player1 (players))
                               in Estado mapa1 jogadores
              | otherwise = e {jogadoresEstado = (atualizaIndiceLista id (movimentos j mapa player) (players))}
              where players = jogadoresEstado e
                    player = encontraIndiceLista id (players)
                    estadoPlayer = estadoJogador player
                    mapa = mapaEstado e

-- ** Funções secundárias da Tarefa 2 

-- | Função que faz o jogador disparar , utilizando a função troca terrêno para alterar a peça anterior à atual do jogador 
dispara :: Mapa -> Jogador -> (Mapa,Jogador)
dispara mapa player | cola > 0 && (distplayer - 1) > 0 && estaChao estado = let posicao = (pista,floor(distplayer)- 1)
                                                                                nPeca = trocaTerreno (encontraPosicaoMatriz posicao mapa) Cola
                                                                                qCola = (cola-1)
                                                                            in (atualizaPosicaoMatriz posicao nPeca mapa, Jogador pista distplayer velocidade qCola estado)
                    | otherwise = (mapa,player)
                    where cola = colaJogador player
                          distplayer = distanciaJogador player
                          estado = estadoJogador player
                          velocidade  = velocidadeJogador player
                          pista = pistaJogador player

-- | Função que acelera o jogador
aceleraT2 :: EstadoJogador -> EstadoJogador
aceleraT2 (Chao (False)) = Chao (True)
aceleraT2 a = a

-- | Função que desacelera o jogador
desacelera :: EstadoJogador -> EstadoJogador
desacelera (Chao (True)) = Chao (False)
desacelera a = a

-- | Função que deteta o tipo de movimento e executa a função necessária para tal movimento
movimentos :: Jogada -> Mapa -> Jogador -> Jogador 
movimentos (Movimenta C) map player | ePosicaoMatrizValida (y-1,floor x) map && estaChao estado =  sobe map player
                                    | otherwise = player
                                    where (x,y) = (distanciaJogador player,pistaJogador player)
                                          estado = estadoJogador player
movimentos (Movimenta B) map player | ePosicaoMatrizValida (y+1,floor x) map && estaChao estado = desce map player
                                    | otherwise = player
                                    where (x,y) = (distanciaJogador player,pistaJogador player)
                                          estado = estadoJogador player
movimentos (Movimenta D) map player | estaAr (estadoJogador player) = inclinaD player 
                                    | otherwise = player
movimentos (Movimenta E) map player | estaAr (estadoJogador player) = inclinaE player
                                    | otherwise = player


-- *** Funções terciárias da Tarefa 3

-- | Troca a pista de um jogador (se possivél) pela que está a cima de si
sobe :: Mapa -> Jogador -> Jogador
sobe m player | diffh < (-0.2) = player{pistaJogador = y - 1,estadoJogador = Ar {alturaJogador = h1, inclinacaoJogador = 0 + ((180*inclin peca1)/pi), gravidadeJogador = 0, inclinacaoJogadorini = 0 + ((180*inclin peca1)/pi)}} 
              | diffh < 0.2 = player {pistaJogador = y - 1}  
              | otherwise = player {estadoJogador = Morto {timeoutJogador=1},velocidadeJogador = 0}
              where (x,y) = (distanciaJogador player,pistaJogador player)
                    peca1 = encontraPosicaoMatriz (y,floor x) m
                    peca2 = encontraPosicaoMatriz (y-1,floor x) m
                    h1 = alturas peca1 (x - (fromIntegral(floor x)))
                    h2 = alturas peca2 (x - (fromIntegral(floor x)))
                    diffh = h2 - h1

-- | Troca a pista do jogador (caso possivél) para a que está a baixo de si
desce :: Mapa -> Jogador -> Jogador
desce m player | diffh < (-0.2) = player{pistaJogador = y + 1,estadoJogador = Ar {alturaJogador = h1, inclinacaoJogador = 0 + ((180*inclin peca1)/pi), gravidadeJogador = 0, inclinacaoJogadorini = 0 + ((180*inclin peca1)/pi)}} 
               | diffh < 0.2 = player {pistaJogador = y + 1}  
               | otherwise = player {estadoJogador = Morto {timeoutJogador=1},velocidadeJogador = 0}
               where (x,y) = (distanciaJogador player,pistaJogador player)
                     peca1 = encontraPosicaoMatriz (y,floor x) m
                     peca2 = encontraPosicaoMatriz (y+1,floor x) m
                     h1 = alturas peca1 (x - (fromIntegral(floor x)))
                     h2 = alturas peca2 (x - (fromIntegral(floor x)))
                     diffh = h2 - h1

-- | Função que muda a inclinação quando é executada a jogada Movimenta E
inclinaE :: Jogador -> Jogador 
inclinaE player | (ang + 2) >= 90 = player {estadoJogador = estado {inclinacaoJogador = 90}}
                | otherwise = player {estadoJogador = estado {inclinacaoJogador = ang + 2}}
                where estado = estadoJogador player
                      ang = inclinacaoJogador estado


-- | Função que muda a inclinação quando é executada a jogada Movimenta D
inclinaD :: Jogador -> Jogador 
inclinaD player | (ang - 2) <= (-90) = player {estadoJogador = estado {inclinacaoJogador = (-90)}}
                | otherwise = player {estadoJogador = estado {inclinacaoJogador = ang - 2}}
                where estado = estadoJogador player
                      ang = inclinacaoJogador estado

-- *** Funções auxiliares

-- | Calcula a inclinação numa determinada peça
inclin :: Peca -> Double
inclin (Recta _ _) = 0
inclin (Rampa _ y z) = atan (fromIntegral(z-y))


-- | Função que troca o piso atual por um piso desejado
trocaTerreno :: Peca -> Piso -> Peca
trocaTerreno (Recta _ x) p = (Recta p x)
trocaTerreno (Rampa _ x y) p = (Rampa p x y)

-- | Calcula a altura
diffAlt :: Int -> Double -> Double
diffAlt h1 dist = (fromIntegral h1) * dist 
 
-- | Altura atual considerando a altura inicial
alturas :: Peca -> Double -> Double
alturas (Rampa _ h h1) x = (diffAlt (h1-h) x) + (fromIntegral h)
alturas (Recta _ h) x = (fromIntegral h)
