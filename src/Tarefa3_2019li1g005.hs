-- | Este módulo define funções comuns da Tarefa 3 do trabalho prático.
module Tarefa3_2019li1g005 where
import Textes
import LI11920
import Tarefa0_2019li1g005
import Data.List
-- * Testes

-- | Testes unitários da Tarefa 3.
--
-- Cada teste é um 'Mapa'.
testesT3 :: [Mapa]
testesT3 = testeT3
type Padrao = (Int,Instrucao)

-- * Funções principais da Tarefa 3.

-- | Desconstrói um 'Mapa' numa sequência de 'Instrucoes'.
--
-- __NB:__ Uma solução correcta deve retornar uma sequência de 'Instrucoes' tal que, para qualquer mapa válido 'm', executar as instruções '(desconstroi m)' produza o mesmo mapa 'm'.
--
-- __NB:__ Uma boa solução deve representar o 'Mapa' dado no mínimo número de 'Instrucoes', de acordo com a função 'tamanhoInstrucoes'.
desconstroi :: Mapa -> Instrucoes
desconstroi (h:t) = mapa5
                  where np    = length mapa
                        mapa  = instMap 0 (h:t) 
                        mapa1 = instH mapa
                        mapa2 = vert2 (transpose (instH mapa1))
                        mapa3 = vert2 (transpose mapa)
                        mapa4 = vToH np mapa3 []
                        mapa5 = tmIsnts (infDeteta mapa2) (infDeteta mapa4)

-- ** Funções que transformam o mapa inicial para instruções sem fazer quaisquer padrões

-- | Transforma mapa para Instrucoes
instMap :: Int -> Mapa -> [Instrucoes]
instMap x [] = []
instMap x (h:t) = (instP x (tail h)):(instMap (x+1) t)
-- | Transforma pista para Instrucoes
instP :: Int -> Pista -> Instrucoes 
instP _ [] = []
instP i (h:t) = (pecaToInst i h) : (instP i t)
-- | Trasnforma peca para instrucao
pecaToInst ::Int -> Peca -> Instrucao
pecaToInst i (Recta x y) = (Anda [i] x)
pecaToInst i (Rampa x y z) | y-z < 0 = (Sobe [i] x (z-y))
                           |otherwise = (Desce [i] x (y-z))
-- ** Funções secundárias 

-- | Função que deteta qual é o conjunto de instruções com menor número dados dois conjuntos 
tmIsnts :: Instrucoes -> Instrucoes -> Instrucoes
tmIsnts a b | tamanhoInstrucoes a < tamanhoInstrucoes b = a
            | otherwise = b

-- | Função que junta as instruções após os padrões verticais terem sido feitos
vert2 :: [Instrucoes] -> Instrucoes
vert2 [] = []
vert2 (x:xs) = auxV1 x ++ vert2 xs

-- | Função que faz os padrões horizontais caso estes sejam os primeiros a serem feitos
instH :: [Instrucoes] -- ^ Lista de instruções que vem da função instMao
      -> [Instrucoes] -- ^ Lista de instruções com cada pista já com os seus padrões horizontais feitos
instH [] = []
instH (x:xs) = r2(a):c
             where a = infDeteta x
                   c = instH xs

-- | Função principal de deteção de padrões horizontais
deteta :: Instrucoes -> Instrucoes -> Instrucoes
deteta l [] = l
deteta l (x:xs) | b == [] = deteta (l ++ [x]) xs
                | otherwise = (a ++ b ++ (deteta [] c))
                where (a,b,c) = verifica l (x:xs) 
 
-- | Verifica se o primeiro grupo de instruções tem um padrão na segunda
verifica :: Instrucoes -> Instrucoes -> (Instrucoes,Instrucoes,Instrucoes)
verifica [] l = ([],[],[])
verifica (x:xs) (y:ys) | x == y = if auxVerifica (x:xs) (y:ys) then ([],[Repete (n+1) d],(drop ((length (x:xs))*n) (y:ys))) else (x:a,b,c)
                       | otherwise = (x:a,b,c)
                       where (a,b,c) = verifica xs (y:ys)
                             n = repetePadrao (x:xs) (y:ys)
                             d = deteta [] (x:xs)

-- | Função que faz horizontais após verticais já tiverem sido feitos anteriormente
vToH :: Int -- ^ Número de pistas que o mapa contêm
     -> Instrucoes -- ^ Instruções nas quais iremos verificar se existem padrões verticais 
     -> Instrucoes -- ^ Intruções que já verificamos que tinham ou não padrões verticais vão
     -> Instrucoes -- ^ Instruções já com os padrões verticais definidos
vToH x [] y = (deteta [] (concat(auxvToH y (map (\x -> []) [1..x]))))
vToH x (h:t) l | length (pistaInst h) == 1 = vToH x t (l ++ [h])
               | otherwise = (concat l1) ++ h:vToH x t []
               where l1 = auxvToH l (map (\x -> []) [1..x])

-- | Função que executa a função que cria padrões horizontais até não se poder encontrar mais padrões horizontais
infDeteta :: Instrucoes -- ^ Instruções que queremos verificar se ainda têm padrões verticais 
          -> Instrucoes -- ^ Instruções mais compactas horizontalmente
infDeteta l | (deteta [] l) ==  (deteta []  (deteta [] l)) = (deteta [] l)
            | otherwise = infDeteta (deteta []  (deteta [] l))

-- *** Funções terciárias 

-- | Dá pista da instrucao
pistaInst :: Instrucao -> [Int]
pistaInst (Anda x _) = x
pistaInst (Sobe x _ _) = x
pistaInst (Desce x _ _) = x
pistaInst (Teleporta x _) = x
pistaInst (Repete _ (x:_)) = pistaInst x 

pistaInst1 x = head (pistaInst x) 

-- | Verifica se ambas as instruções são iguais
auxVerifica :: Instrucoes -> Instrucoes -> Bool
auxVerifica a b = a == take (length a) b

-- | Vê quanta vezes o padrao se repete no casos em que é necessario o Repete
repetePadrao :: Instrucoes -> Instrucoes -> Int
repetePadrao a b | a == (take (length a) b) = 1 + repetePadrao a (drop (length a) b)
                 | otherwise = 0 

-- | Função que dada uma lista de pistas e uma Instrução faz com que a instrução seja executada em todas essas pistas
same :: [Int] -> Instrucao -> Instrucao
same a (Anda _ b) = (Anda a b) 
same a (Sobe _ b c) = (Sobe a b c)
same a (Desce _ b c) = (Desce a b c)
same a (Repete b c) = (Repete b (map (same a) c))

-- | Função que verifica se duas funções são iguais independentemente das pistas destas mesmas Instruções
iguais :: Instrucao -> Instrucao -> Bool
iguais (Anda _ x) (Anda _ y) = y == x
iguais (Desce _ x x1) (Desce _ y y1) = y == x && y1 == x1
iguais (Sobe _ x x1) (Sobe _ y y1) = y == x && y1 == x1
iguais (Repete x x1) (Repete y y1) = x == y && aux_iguais x1 y1
                                   where aux_iguais (x:xs) (y:ys) = (iguais x y) && (aux_iguais xs ys)
                                         aux_iguais a b = a == [] && b == []
iguais a b = False

-- | Função que retira a existencia de Repete 2 de apenas uma instrução de forma a abrir a possibilidade da existência de mais padrões horizontais 
r2 :: Instrucoes -> Instrucoes
r2 [] = []
r2 ((Repete 2 x):t) | length x == 1 = x ++ x ++ (r2 t)
r2 (h:t) = h : r2 t

-- *** Funções auxiliares

-- | Função que cria padrões verticais
auxV1 :: Instrucoes -> Instrucoes 
auxV1 [] = []
auxV1 (h:t) | a == [] = h : auxV1 t
            | otherwise = (same ((pistaInst1 h):a) h): b 
            where (a,b) = auxV2 h t 

-- | Verifica de se duas intruções são iguais e se forem junta-as 
auxV2 :: Instrucao -> Instrucoes -> ([Int],Instrucoes)
auxV2 _ [] = ([],[])
auxV2 x (h:t) | iguais x h = ((pistaInst1 h): f,s)
              | otherwise = (f,h: s)
              where (f,s) = auxV2 x t

-- | Função auxiliar à função vToH
auxvToH :: Instrucoes -> [Instrucoes] -> [Instrucoes]
auxvToH [] l = l
auxvToH (h:t) l = auxvToH t (atualizaIndiceLista np ((l !! np) ++ [h]) l) 
              where np = pistaInst1 h 
