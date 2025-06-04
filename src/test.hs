module Main where


import Graphics.Gloss
import Tarefa0_2019li1g005v1
import Tarefa1_2019li1g005v1
import Tarefa2_2019li1g005v1
import Tarefa4_2019li1g005v1
import Graphics.Gloss.Interface.Pure.Game
import LI11920v1

type EstadoG = (Float,Float,[Key],Estado,Int,Mode)
type Img = [Picture]
data Mode = Game | EndGame | StartMenu | EndlessGame | GameForTwo

a = gera 4 20 666

desenha :: Img -> (Float,Float) -> (Float,Float) -> Pista -> [Picture]
desenha img _ _ [] = []
desenha img (lim,lim1) (x,y) ((Rampa p x1 y1):ps) | (x <= lim) && (x > lim1) = 
                                         let y2 = fromIntegral(y1-x1)
                                             x2 = fromIntegral (x1-1)
                                             x3 = fromIntegral (y1 -1)
                                         in if y2 > 0 then (Translate x (x2*75 + y) (Pictures((desenhaRampa img p y2):fill img p x1))): desenha img (lim,lim1) (x+100,y) ps
                                                      else (Translate x (x3*75 + y) (scale (-1) 1 (Pictures((desenhaRampa img p (abs y2)):fill img p y1)))): desenha img (lim,lim1) (x+100,y) ps  
                                       | otherwise = desenha img (lim,lim1) (x+100,y) ps
desenha img (lim,lim1) (x,y) ((Recta p x1):ps) | (x <= lim) && (x > (lim1)) =
                                      let x2 = fromIntegral((x1-1)*75)
                                      in  (Translate x (x2 + y) (Pictures((desenhaPiso 0 img p):fill img p x1)) ):desenha img (lim,lim1) (x+100,y) ps
                                    | otherwise = desenha img (lim,lim1) (x+100,y) ps 

desenhaPiso :: Int -> Img -> Piso -> Picture
desenhaPiso x img Cola = img !! (0 + x)
desenhaPiso x img Terra = img !! (4 + x)
desenhaPiso x img Lama = img !! (3 + x)
desenhaPiso x img Relva = img !! (2 + x)
desenhaPiso x img Boost = img !! (1 + x)

desenhaRampa :: Img -> Piso -> Int -> Picture
desenhaRampa img p 1 = desenhaPiso 5 img p
desenhaRampa img p x = Translate (-(((fromIntegral x)-1)*50)/(fromIntegral x)) (75) (scale (1/(fromIntegral x)) 1 (Pictures(desenha img (1050.0,(-1050.0)) (0,0) (createRampas p x))))

createRampas :: Piso -> Int -> Pista
createRampas _ 0 = []
createRampas p x = (createRampas p (x-1)) ++ [(Rampa p (x-1) x)] 

fill :: Img -> Piso -> Int -> [Picture]
fill img p 0 = [] 
fill img p 1 = [Translate 0 (-96.5) (Scale 1 0.75 (desenhaPiso 10 img p))]
fill img p x = Translate 0 ((-75)*(fromIntegral x)) (desenhaPiso 10 img p):fill img p (x-1) 

desenhaPlayer :: (Float,Float) ->  Img -> [Jogador] -> [[Picture]] -> Float -> [Double] -> [Double] -> [[Picture]]
desenhaPlayer _ _ [] l _ _ _= l
desenhaPlayer (x0,y) img (x:xs) l z (a:as) (i:is) | x2 >= 900 = desenhaPlayer (x0,y) img xs l z as is 
                                                  | otherwise = desenhaPlayer (x0,y) img xs (atualizaIndiceLista x1 (pic:(l!!x1)) l) z as is
                                                  where x1 = pistaJogador x 
                                                        d = distanciaJogador x
                                                        estplayer = estadoJogador x
                                                        y1 = a * 75
                                                        x2 = (((realToFrac d)-z)*100+x0)
                                                        pic = Translate x2 (fromIntegral(x1*100) + (realToFrac y1)-100+y) (Rotate (realToFrac i) (img !! 0)) 

desenhaPlayerPrin :: Img -> Jogador -> (Float,Float) -> [[Picture]] -> Double -> Double -> ([[Picture]],Float,Float)
desenhaPlayerPrin img j (x,y) l a i = (atualizaIndiceLista x1 [pic] l,x2,y2)
                                    where x1 = pistaJogador j
                                          x2 = (x - (realToFrac(distanciaJogador j)*100))
                                          estplayer = estadoJogador j
                                          y0 = fromIntegral(x1*100) + a * 75
                                          (y1,y2) = if y0 > 450 then (450,450 + y - (realToFrac y0)) else (y0,y + 0)   
                                          pic = Translate x ((realToFrac y1)-100) (Rotate (realToFrac i) (img !! 0))

posicoes :: [Jogador] -> Picture
posicoes x = Color white (Pictures (aux p 1))
           where p = posicao (zipWith (\x y -> (x,y)) [0..(length x)] x) []
                 aux ((x,x1):ps) y = (Translate (-140) ((-250)-(y*(50))) (scale 0.4 0.4 (text ((show y)++"-Jogador "++show x++ " -- "++show(colaJogador x1))))):aux ps (y+1)
                 aux [] _ = []                                            



estadoInicial :: Mode ->  EstadoG
estadoInicial Game = (-700,0,[],(Estado a (aux ((length a)-1))),1,Game)
                   where aux (-1) = []
                         aux x = aux (x-1) ++ [(Jogador x 0 0 5 (Chao False))]
estadoInicial GameForTwo = (0,0,[],(Estado a (aux ((length a)-1))),0,GameForTwo)
                         where aux (-1) = []
                               aux x = aux (x-1) ++ [(Jogador x 0 0 5 (Chao False))]
estadoInicial StartMenu = (0,0,[],Estado [] [],0, StartMenu)
estadoInicial EndGame = (0,0,[],Estado [] [],0, EndGame)
estadoInicial EndlessGame = (0,0,[],Estado [] [],0, EndlessGame)

desenhaMapa :: [Picture] -> (Float,Float) -> EstadoG -> Picture
desenhaMapa img (lim,lim1) (x,y,z,e,jp,_) = Pictures (concat (zipWith (++) mapa (reverse player)))
  where
    poligno :: Picture
    poligno = Pictures [(Polygon [((-10),(-2)),(0,(-2)),(0,2),((-10),2),((-10),(-2))]),Color red(Polygon [((10),(-2)),(0,(-2)),(0,2),((10),2),((10),(-2))])]
    m = mapaEstado e
    mapa = aux img (x1,y1,m)
    (l,x1,y1) = desenhaPlayerPrin [poligno] p (x,y) (replicate (length m) []) a i
    player = desenhaPlayer (x,y1) [poligno] (ps) l d as is
    aux img (x,y,m:ms) = (aux img (x,y+100,ms)) ++ [(desenha img (lim,lim1) ((x+50),(y)) m)]
    aux img (x,y,[]) = []
    playeres = jogadoresEstado e
    (p,ps) = remove jp playeres
    d =  realToFrac (distanciaJogador p)
    (a,as) = remove jp (altura playeres m) 
    (i,is) = remove jp (inclinacao playeres m)
                      

desenhaEstado ::[Picture] -> EstadoG -> Picture 
desenhaEstado img (x,y,z,e,jp,Game) = Pictures [desenhaMapa img (1050,(-1050)) (x,y,z,e,jp,Game),posicoes (jogadoresEstado e)]
desenhaEstado img (x,y,z,e,jp,EndGame) = Pictures [(Polygon [((-10),(-2)),(0,(-2)),(0,2),((-10),2),((-10),(-2))]),Color red(Polygon [((10),(-2)),(0,(-2)),(0,2),((10),2),((10),(-2))])]
desenhaEstado img (x,y,z,e,jp,StartMenu) = Pictures [desenhaButton (buttons!!0),desenhaButton (buttons!!1)]
desenhaEstado img (x,y,z,e,jp,GameForTwo) = Pictures [Translate (150) 0 (desenhaMapa img (900,(-1050)) (x,y,z,e,jp+1,GameForTwo)),Translate (-900) 0 (desenhaMapa img (900,(-1050)) (x,y,z,e,jp,GameForTwo)),
                                                      Polygon[(50,540),(-50,540),(-50,-540),(50,-540),(50,540)],posicoes (jogadoresEstado e)]

altura :: [Jogador] -> Mapa -> [Double]
altura [] _ = []
altura (x:xs) m = if (estaAr estplayer) then (alturaJogador estplayer):(altura xs m) else (alturas peca dp):(altura xs m)
                where estplayer = estadoJogador x
                      x1 = pistaJogador x
                      d = distanciaJogador x
                      df = fromIntegral(floor (distanciaJogador x))
                      dp = d-df
                      peca = if ePosicaoMatrizValida (x1,floor d) m then (m!!x1)!!(floor d) else (Recta Terra 0)

inclinacao :: [Jogador] -> Mapa -> [Double]
inclinacao [] _ = []
inclinacao (x:xs) m = i:inclinacao xs m
                    where x1 = pistaJogador x
                          d = fromIntegral (floor (distanciaJogador x))
                          peca = if ePosicaoMatrizValida (x1,d) m then (m!!x1)!!d else (Recta Terra 0)
                          estplayer = estadoJogador x
                          i = if (estaAr estplayer) then (inclinacaoJogador estplayer)*(-1) else (inclin peca)*(-180 / pi) 

desenhaButton :: Button -> Picture
desenhaButton (x,y,x1,y1) = Polygon [(x,y),(x,y1),(x1,y1),(x1,y),(x,y)]

remove :: Int -> [a] -> (a,[a])
remove x l = let (y,p:ps) = splitAt x l 
             in (p,y++ps)



reageEvento :: Event -> EstadoG -> EstadoG
reageEvento (EventKey (MouseButton(LeftButton)) Down _ pos) (_,_,_,_,_,StartMenu) = cheackClick pos 
reageEvento (EventKey (SpecialKey(KeyUp)) Down _ _)  (_,_,_,_,_,EndGame) = estadoInicial StartMenu
reageEvento (EventKey (Char 'm') Up _ _)  (x,y,l,e,jp,m) = (x,y,filter (\a -> a /=  (Char 'm')) l,jogada jp Desacelera e,jp,m)
reageEvento (EventKey (SpecialKey(KeyUp)) Down _ _)  (x,y,l,e,jp,m) = (x,y,l,jogada jp (Movimenta B) e,jp,m) 
reageEvento (EventKey (SpecialKey(KeyDown)) Down _ _)  (x,y,l,e,jp,m) = (x,y,l,jogada jp (Movimenta C) e,jp,m)
reageEvento (EventKey (Char 'n') Down _ _) (x,y,l,e,jp,m) = (x,y,l,jogada jp (Dispara) e,jp,m)
reageEvento (EventKey (Char 'q') Up _ _)  (x,y,l,e,jp,GameForTwo) = (x,y,filter (\a -> a /=  (Char 'q')) l,jogada (jp+1) Desacelera e,jp,GameForTwo)
reageEvento (EventKey (Char 'w') Down _ _) (x,y,l,e,jp,GameForTwo) = (x,y,l,jogada (jp+1) (Movimenta B) e,jp,GameForTwo) 
reageEvento (EventKey (Char 's') Down _ _) (x,y,l,e,jp,GameForTwo) = (x,y,l,jogada (jp+1) (Movimenta C) e,jp,GameForTwo) 
reageEvento (EventKey (Char 'e') Down _ _) (x,y,l,e,jp,GameForTwo) = (x,y,l,jogada (jp+1) (Dispara) e,jp,GameForTwo)
reageEvento (EventKey k Down _ _)  (x,y,l,e,jp,m) = (x,y,k:l,e,jp,m)
reageEvento (EventKey k Up _ _)  (x,y,l,e,jp,m) = (x,y,filter (\a -> a /=  k) l,e,jp,m) 
reageEvento _ s = s

reageTempo :: Float -> EstadoG -> EstadoG
reageTempo t (x,y,z,e,jp,m) = let (x1,y1,_,e1,_,m1) = reageTempo1 t (x,y,z,e,jp,m) 
                       in (x1,y1,z,e1,jp,m1) 

reageTempo1 :: Float -> EstadoG -> EstadoG
reageTempo1 t (x,y,(SpecialKey(KeyRight):ks),e,jp,m) = let e1 = jogada jp (Movimenta D) e in reageTempo1 t (x,y,ks,e1,jp,m) --reageTempo1 t (x-10,y,ks,m,e)
reageTempo1 t (x,y,(SpecialKey(KeyLeft):ks),e,jp,m) = let e1 = jogada jp (Movimenta E) e in reageTempo1 t (x,y,ks,e1,jp,m) --reageTempo1 t (x+10,y,ks,m,e)
reageTempo1 t (x,y,(Char 'm':ks),e,jp,m) = let e1 = jogada jp (Acelera) e in reageTempo1 t (x,y,ks,e1,jp,m)
reageTempo1 t (x,y,(Char 'd':ks),e,jp,GameForTwo) = let e1 = jogada (jp+1) (Movimenta D) e in reageTempo1 t (x,y,ks,e1,jp,GameForTwo) --reageTempo1 t (x-10,y,ks,m,e)
reageTempo1 t (x,y,(Char 'a':ks),e,jp,GameForTwo) = let e1 = jogada (jp+1) (Movimenta E) e in reageTempo1 t (x,y,ks,e1,jp,GameForTwo) --reageTempo1 t (x+10,y,ks,m,e)
reageTempo1 t (x,y,(Char 'q':ks),e,jp,GameForTwo) = let e1 = jogada (jp+1) (Acelera) e in reageTempo1 t (x,y,ks,e1,jp,GameForTwo)
reageTempo1 t (x,y,(_:ks),e,jp,m) = reageTempo1 t (x,y,ks,e,jp,m) --reageTempo1 t (x,y,ks,m,e)
reageTempo1 t (x,y,[],e,jp,m) = (x,y,[],e1,jp,m1)--(x,y,[],e)
                              where j = jogadoresEstado e
                                    mapa = mapaEstado e
                                    j1 = map (passo (realToFrac t) mapa) j
                                    e1 = e{jogadoresEstado = j1}
                                    m1 = updateMode m e1 jp 

cheackClick :: (Float,Float) -> EstadoG
cheackClick p | cheackButton p (buttons!!0) = estadoInicial Game
              | cheackButton p (buttons!!1) = estadoInicial GameForTwo
              | otherwise = estadoInicial StartMenu

cheackButton :: (Float,Float) -> Button -> Bool
cheackButton (x,y) (x1,y1,x2,y2) = (x >= x1) && (x <= x2) && (y >= y2) && (y <= y1)

updateMode :: Mode -> Estado -> Int -> Mode
updateMode Game x jp = let a = acabou (length ((mapaEstado x)!!0)) ((jogadoresEstado x)!!jp)
                       in if a then EndGame else Game
updateMode EndGame _ _ = EndGame
updateMode StartMenu _ _ = StartMenu
updateMode EndlessGame _ _ = EndlessGame
updateMode GameForTwo x jp = let a = acabou (length ((mapaEstado x)!!0)) ((jogadoresEstado x)!!jp)
                                 b = acabou (length ((mapaEstado x)!!0)) ((jogadoresEstado x)!!(jp+1))
                             in if a && b then EndGame else GameForTwo

acabou :: Int -> Jogador -> Bool
acabou x j = (distanciaJogador j) >= ((fromIntegral x)- 0.01)

posicao :: [(Int,Jogador)] -> [(Int,Jogador)] -> [(Int,Jogador)]
posicao [] l = l
posicao (j:js) l = posicao js (insert j l)
                 where insert (x,j) ((x1,j1):ls) = if (distanciaJogador j) > (distanciaJogador j1) then ((x,j):(x1,j1):ls) else (x1,j1):(insert (x,j) ls)
                       insert j [] = [j]  


fr :: Int
fr = 120

dm :: Display
dm = FullScreen--InWindow "Novo Jogo" (1200, 1200) (0, 0) 

type Button = (Float,Float,Float,Float)

buttons :: [Button]
buttons = [(-100,400,100,350),(-100,300,100,250)]

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
           (estadoInicial StartMenu)   -- estado inicial
           (desenhaEstado [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o])   -- desenha o estado do jogo
           reageEvento     -- reage a um evento
           reageTempo      -- reage ao passar do tempo 