-- | Este módulo define funções genéricas sobre vetores e matrizes, que serão úteis na resolução do trabalho prático.
module Tarefa0_2019li1g005 where
import LI11920

--teste

-- * Funções não-recursivas.

-- | Um ponto a duas dimensões dado num referencial cartesiado (distâncias aos eixos vertical e horizontal)
--
-- <<http://li1.lsd.di.uminho.pt/images/cartesiano.png cartesisano>>
-- , ou num referencial polar (distância à origem e ângulo do respectivo vector com o eixo horizontal).
--
-- <<http://li1.lsd.di.uminho.pt/images/polar.png polar>>
data Ponto = Cartesiano Double Double | Polar Double Angulo deriving (Show)
-- | Um ângulo em graus.
type Angulo = Double
-- ** Funções sobre vetores
-- | Um 'Vetor' na representação escalar é um 'Ponto' em relação à origem.
type Vetor = Ponto
-- ^ <<http://li1.lsd.di.uminho.pt/images/vetor.png vetor>>

-- *** Funções gerais sobre 'Vetor'es.

-- | X de um ponto
posx :: Ponto -> Double
posx (Cartesiano x y ) = x
posx (Polar x y) = cos ((y*pi)/180) * x



-- | Y de um ponto
posy :: Ponto -> Double
posy (Cartesiano x y ) = y
posy (Polar x y) = sin ((y*pi)/180) * x


-- | Função que transforma Polar para Cartesiano
polCart :: Ponto -> Ponto
polCart x = (Cartesiano (posx x) (posy x))


-- | Soma dois 'Vetor'es.
somaVetores :: Vetor -> Vetor -> Vetor
somaVetores (Cartesiano x  y) (Cartesiano x1  y1) = Cartesiano (x + x1) (y + y1)
somaVetores x y = somaVetores (polCart x) (polCart y)


-- | Subtrai dois 'Vetor'es.
subtraiVetores :: Vetor -> Vetor -> Vetor
subtraiVetores (Cartesiano x  y) (Cartesiano x1  y1) = Cartesiano (x - x1) (y - y1)
subtraiVetores x y = subtraiVetores (polCart x) (polCart y)


-- | Multiplica um escalar por um 'Vetor'.
multiplicaVetor :: Double -> Vetor -> Vetor
multiplicaVetor x (Cartesiano x1 y1) = Cartesiano (x1*x) (y1*x)
multiplicaVetor x y = multiplicaVetor x (polCart y)


-- ** Funções sobre rectas.

-- | Um segmento de reta é definido por dois pontos.
type Reta = (Ponto,Ponto)


-- | Aplica a função polCart aos dois pontos de uma determinada reta
retaCart :: Reta -> Reta
retaCart (x,y) = (polCart x, polCart y)

-- | Testar se dois segmentos de reta se intersetam.
--
-- __NB:__ Aplique as equações matemáticas bem conhecidas, como explicado por exemplo em <http://www.cs.swan.ac.uk/~cssimon/line_intersection.html>.
intersetam :: Reta -> Reta -> Bool
intersetam ((Cartesiano x1 y1),(Cartesiano x2 y2)) ((Cartesiano x3 y3),(Cartesiano x4 y4))=
    let a = ((y3-y4)*(x1-x3)+(x4-x3)*(y1-y3))/((x4-x3)*(y1-y2)-(x1-x2)*(y4-y3))
        b = ((y1-y2)*(x1-x3)+(x2-x1)*(y1-y3))/((x4-x3)*(y1-y2)-(x1-x2)*(y4-y3))
    in a > 0 && a < 1 && b > 0 && b < 1
intersetam x y = intersetam (retaCart x) (retaCart y)  

-- | Calcular o ponto de intersecao entre dois segmentos de reta.
--
-- __NB:__ Aplique as equações matemáticas bem conhecidas, como explicado por exemplo em <http://www.cs.swan.ac.uk/~cssimon/line_intersection.html>.
intersecao :: Reta -> Reta -> Ponto
intersecao ((Cartesiano x1 y1),(Cartesiano x2 y2)) ((Cartesiano x3 y3),(Cartesiano x4 y4))= (Cartesiano x y)
        where a = ((y3-y4)*(x1-x3)+(x4-x3)*(y1-y3))/((x4-x3)*(y1-y2)-(x1-x2)*(y4-y3))
              x = x1 + a*(x2 - x1)
              y = y1 + a*(y2 - y1)
intersecao x y = intersecao (retaCart x) (retaCart y)


-- ** Funções sobre listas

-- *** Funções gerais sobre listas.
--
-- Funções não disponíveis no 'Prelude', mas com grande utilidade.

-- | Verifica se o indice pertence à lista.
--
-- __Sugestão:__ use a função 'length' que calcula tamanhos de listas
eIndiceListaValido :: Int -> [a] -> Bool
eIndiceListaValido x ys = x >= 0 && (length ys) > x
--eIndiceListaValido x [] = False 

-- | A dimensão de um mapa dada como um par (/número de linhas/,/número de colunhas/).
type DimensaoMatriz = (Int,Int)

-- | Uma posição numa matriz dada como um par (/linha/,/colunha/).
-- As coordenadas são dois números naturais e começam com (0,0) no canto superior esquerdo, com as linhas incrementando para baixo e as colunas incrementando para a direita:
--
-- <<http://li1.lsd.di.uminho.pt/images/posicaomatriz.png posicaomatriz>>
type PosicaoMatriz = (Int,Int)

-- | Uma matriz é um conjunto de elementos a duas dimensões.
--
-- Em notação matemática, é geralmente representada por:
--
-- <<https://upload.wikimedia.org/wikipedia/commons/d/d8/Matriz_organizacao.png matriz>>
type Matriz a = [[a]]

-- | Calcula a dimensão de uma matriz.
--
-- __NB:__ Note que não existem matrizes de dimensão /m * 0/ ou /0 * n/, e que qualquer matriz vazia deve ter dimensão /0 * 0/.
--
-- __Sugestão:__ relembre a função 'length', referida anteriormente.
dimensaoMatriz :: Matriz a -> DimensaoMatriz
dimensaoMatriz [] = (0,0)
dimensaoMatriz (h : t) = if length h == 0 then (0,0) else (length (h:t),length h)

-- | Verifica se a posição pertence à matriz.
ePosicaoMatrizValida :: PosicaoMatriz -> Matriz a -> Bool 
ePosicaoMatrizValida (x,y) a = x >= 0 && y >= 0 && x <= (length a) - 1 && y <= (length (a !! 0)) - 1 
 
-- * Funções recursivas.

-- ** Funções sobre ângulos

-- | Normaliza um ângulo na gama [0..360).
--  Um ângulo pode ser usado para representar a rotação
--  que um objecto efectua. Normalizar um ângulo na gama [0..360)
--  consiste, intuitivamente, em extrair a orientação do
--  objecto que resulta da aplicação de uma rotação. Por exemplo, é verdade que:
--
-- prop> normalizaAngulo 360 = 0
-- prop> normalizaAngulo 390 = 30
-- prop> normalizaAngulo 720 = 0
-- prop> normalizaAngulo (-30) = 330
normalizaAngulo :: Angulo -> Angulo
normalizaAngulo x = if x >= 360 then normalizaAngulo(x - 360) else if x < 0 then normalizaAngulo(x + 360) else x 

-- ** Funções sobre listas.

-- | Devolve o elemento num dado índice de uma lista.
--
-- __Sugestão:__ Não use a função (!!) :: [a] -> Int -> a :-)
encontraIndiceLista :: Int -> [a] -> a
encontraIndiceLista 0 (h:t) = h
encontraIndiceLista x (h:t) = encontraIndiceLista (x-1) t

-- | Modifica um elemento num dado índice.
--
-- __NB:__ Devolve a própria lista se o elemento não existir.
atualizaIndiceLista :: Int -> a -> [a] -> [a]
atualizaIndiceLista _ _ [] = []
atualizaIndiceLista 0 y (h:t) = y:t
atualizaIndiceLista x y (h:t) = h:(atualizaIndiceLista (x - 1) y t) 

-- ** Funções sobre matrizes.

-- | Devolve o elemento numa dada 'Posicao' de uma 'Matriz'.
encontraPosicaoMatriz :: PosicaoMatriz -> Matriz a -> a
encontraPosicaoMatriz (x,y) m = encontraIndiceLista y (encontraIndiceLista x m)

-- | Modifica um elemento numa dada 'Posicao'
--
-- __NB:__ Devolve a própria 'Matriz' se o elemento não existir.
atualizaPosicaoMatriz :: PosicaoMatriz -> a -> Matriz a -> Matriz a
atualizaPosicaoMatriz (x,y) a  m = atualizaIndiceLista x (atualizaIndiceLista y a (encontraIndiceLista x m)) m

-- | Função que verifica se o jogador está no chão
estaChao :: EstadoJogador -> Bool
estaChao (Chao _ ) = True
estaChao _ = False

-- | Função que verifica se um jogador está no ar
estaAr :: EstadoJogador -> Bool
estaAr (Ar _ _ _) = True
estaAr _ = False

-- | Verifica se jogador está morto
estaMorto :: EstadoJogador -> Bool
estaMorto (Morto _) = True
estaMorto _ = False 
