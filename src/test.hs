module Main where


import Graphics.Gloss
import Tarefa0_2019li1g005
import Tarefa1_2019li1g005
import Tarefa2_2019li1g005
import Tarefa4_2019li1g005
import Graphics.Gloss.Interface.Pure.Game
import LI11920

type EstadoG = (Float,Float,[Key],Estado)
type Img = [Picture]

a = gera 4 20 666

desenha :: Img -> (Float,Float) -> Pista -> [Picture]
desenha img _ [] = []
desenha img (x,y) ((Rampa p x1 y1):ps) = let y2 = fromIntegral(y1-x1)
                                             x2 = fromIntegral (x1-1)
                                             x3 = fromIntegral (y1 -1)
                                         in if y2 > 0 then (Translate x (x2*75 + y) (Pictures((desenhaRampa img p y2):fill img p x1))): desenha img (x+100,y) ps
                                                      else (Translate x (x3*75 + y) (scale (-1) 1 (Pictures((desenhaRampa img p (abs y2)):fill img p y1)))): desenha img (x+100,y) ps  
desenha img (x,y) ((Recta p x1):ps) = let x2 = fromIntegral((x1-1)*75)
                                      in  (Translate x (x2 + y) (Pictures((desenhaPiso 0 img p):fill img p x1)) ):desenha img (x+100,y) ps 

desenhaPiso :: Int -> Img -> Piso -> Picture
desenhaPiso x img Cola = img !! (0 + x)
desenhaPiso x img Terra = img !! (4 + x)
desenhaPiso x img Lama = img !! (3 + x)
desenhaPiso x img Relva = img !! (2 + x)
desenhaPiso x img Boost = img !! (1 + x)

desenhaRampa :: Img -> Piso -> Int -> Picture
desenhaRampa img p 1 = desenhaPiso 5 img p
desenhaRampa img p x = Translate (-(((fromIntegral x)-1)*50)/(fromIntegral x)) (75) (scale (1/(fromIntegral x)) 1 (Pictures(desenha img (0,0) (createRampas p x))))

createRampas :: Piso -> Int -> Pista
createRampas _ 0 = []
createRampas p x = (createRampas p (x-1)) ++ [(Rampa p (x-1) x)] 

fill :: Img -> Piso -> Int -> [Picture]
fill img p 0 = [] 
fill img p 1 = [Translate 0 (-96) (Scale 1 0.75 (desenhaPiso 10 img p))]
fill img p x = Translate 0 ((-75)*(fromIntegral x)) (desenhaPiso 10 img p):fill img p (x-1) 

desenhaPlayer :: Img -> [Jogador] -> [[Picture]] -> [[Picture]]
desenhaPlayer _ [] l = l
desenhaPlayer img (x:xs) l = desenhaPlayer img xs (atualizaIndiceLista x1 (pic:(l!!x1)) l)
                           where x1 = pistaJogador x 
                                 pic =Translate 0 (fromIntegral(x1*100)) (img !! 0) 

desenhaPlayerPrin :: Img -> Jogador -> Float -> [[Picture]] -> ([[Picture]],Float)
desenhaPlayerPrin img j x l = (atualizaIndiceLista x1 [pic] l,x2)
                            where x1 = pistaJogador j
                                  x2 = (x - (realToFrac(distanciaJogador j)*100))
                                  pic = Translate 0 (fromIntegral(x1*100)) (img !! 0)



estadoInicial :: EstadoG
estadoInicial = (0,0,[],(Estado a (aux ((length a)-1))))
              where aux (-1) = []
                    aux x = aux (x-1) ++ [(Jogador x 0 0 5 (Chao False))]
                      

desenhaEstado ::[Picture] -> EstadoG -> Picture 
desenhaEstado img (x,y,z,e) =  Pictures (concat (zipWith (++) mapa (reverse player)))
  where
    poligno :: Picture
    poligno = Translate (-650) (-100) (Polygon [(0,0),(10,0),(10,10),(0,10),(0,0)])
    m = mapaEstado e
    mapa = aux img (x1,y,m)
    (l,x1) = desenhaPlayerPrin [poligno] p x (replicate (length m) []) 
    player = desenhaPlayer [poligno] (ps) l  
    aux img (x,y,m:ms) = (aux img (x,y+100,ms)) ++ [(desenha img ((x-500),(y)) m)]
    aux img (x,y,[]) = []
    (p:ps) = jogadoresEstado e


reageEvento :: Event -> EstadoG -> EstadoG
reageEvento (EventKey (Char 'm') Up _ _) (x,y,l,e) = let e1 = jogada 0 (Desacelera) e in (x,y,filter (\a -> a /=  (Char 'm')) l,e1) 
reageEvento (EventKey k Down _ _) (x,y,l,e) = (x,y,k:l,e)
reageEvento (EventKey k Up _ _) (x,y,l,e) = (x,y,filter (\a -> a /=  k) l,e) -- ignora qualquer outro evento
reageEvento _ s = s

reageTempo :: Float -> EstadoG -> EstadoG
reageTempo t (x,y,z,e) = let (x1,y1,_,e1) = reageTempo1 t (x,y,z,e) 
                       in (x1,y1,z,e1) 

reageTempo1 :: Float -> EstadoG -> EstadoG
reageTempo1 t (x,y,(SpecialKey(KeyUp):ks),e) = let e1 = jogada 0 (Movimenta C) e in reageTempo1 t (x,y,ks,e1) --reageTempo1 t (x,y-10,ks,m,e)
reageTempo1 t (x,y,(SpecialKey(KeyDown):ks),e) = let e1 = jogada 0 (Movimenta B) e in reageTempo1 t (x,y,ks,e1) --reageTempo1 t (x,y+10,ks,m,e)
reageTempo1 t (x,y,(SpecialKey(KeyRight):ks),e) = let e1 = jogada 0 (Movimenta D) e in reageTempo1 t (x,y,ks,e1) --reageTempo1 t (x-10,y,ks,m,e)
reageTempo1 t (x,y,(SpecialKey(KeyLeft):ks),e) = let e1 = jogada 0 (Movimenta B) e in reageTempo1 t (x,y,ks,e1) --reageTempo1 t (x+10,y,ks,m,e)
reageTempo1 t (x,y,(Char 'm':ks),e) = let e1 = jogada 0 (Acelera) e in reageTempo1 t (x,y,ks,e1)
reageTempo1 t (x,y,(Char 'n':ks),e) = let e1 = jogada 0 (Dispara) e in reageTempo1 t (x,y,ks,e1)
reageTempo1 t (x,y,(_:ks),e) = reageTempo1 t (x,y,ks,e) --reageTempo1 t (x,y,ks,m,e)
reageTempo1 t (x,y,[],e) = (x,y,[],e1)--(x,y,[],e)
                         where j = jogadoresEstado e
                               m = mapaEstado e
                               j1 = map (passo (realToFrac t) m) j
                               e1 = e{jogadoresEstado = j1}

fr :: Int
fr = 50

dim_peca :: (Int,Int)
dim_peca = (100,100)

dm :: Display
dm = InWindow "Novo Jogo" (1200, 1200) (0, 0)

main :: IO ()
main = do a <- loadBMP "Img/Recta Cola.bmp"
          b <- loadBMP "Img/Recta Boost.bmp"
          c <- loadBMP "Img/Recta Relva.bmp"
          d <- loadBMP "Img/Recta Lama.bmp"
          e <- loadBMP "Img/Recta Terra.bmp"
          f <- loadBMP "Img/Rampa Cola.bmp"
          g <- loadBMP "Img/Rampa Boost.bmp"
          h <- loadBMP "Img/Rampa Relva.bmp"
          i <- loadBMP "Img/Rampa Lama.bmp"
          j <- loadBMP "Img/Rampa Terra.bmp" 
          k <- loadBMP "Img/Filler Cola.bmp"
          l <- loadBMP "Img/Filler Boost.bmp"
          m <- loadBMP "Img/Filler Relva.bmp"
          n <- loadBMP "Img/Filler Lama.bmp"
          o <- loadBMP "Img/Filler Terra.bmp"
          play dm         -- janela onde irá correr o jogo
           (greyN 0.5)     -- côr do fundo da janela
           fr              -- frame rate
           estadoInicial   -- estado inicial
           (desenhaEstado [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o])   -- desenha o estado do jogo
           reageEvento     -- reage a um evento
           reageTempo      -- reage ao passar do tempo 