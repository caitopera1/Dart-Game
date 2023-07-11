{-# LANGUAGE OverloadedStrings #-}

import CodeWorld
import System.Random
import Control.Exception as E
import System.IO.Error
import qualified Data.Text as T

------------------------------------------------- FUNÇÕES MAIN E IO ---------------------------------------------------------------------------------------------------------------------------------------------------

rndmLst :: Int -> IO [Int]
rndmLst n = sequence (Prelude.replicate n (randomRIO (-8, 9::Int)))

main = do
       xs <- rndmLst 10   -- Numero de Baloes                                                                                                
       activityOf      worldInitial {listaRandomica = xs}
                       update 
                       visualization
      
------------------------------------------------- DATAS ---------------------------------------------------------------------------------------------------------------------------------------------------      
      
data DirecaoDardo = Up | Down | Stop 
                    deriving Show
                  
                  
data World =
        Game { pontoDardo :: Point,
               velYdardo :: Double,
               direcaoDardo :: DirecaoDardo,
               ativacaoDardo :: Bool,
               posicoesBaloes :: [Point],
               timer :: Double,
               listaRandomica :: [Int],
               numeroBaloes :: Int,
               score :: Int,
               reinicio :: Bool
             } 
               deriving Show

------------------------------------------------- MUNDO INCIAL ---------------------------------------------------------------------------------------------------------------------------------------------------

worldInitial =
           Game { pontoDardo = (-10, 0),
                  velYdardo = velocidadeVerticalInicial,
                  direcaoDardo = Stop,
                  ativacaoDardo = False,
                  posicoesBaloes = [(0, -14)],
                  timer = periodo,
                  listaRandomica = [],
                  numeroBaloes = 0,
                  score = 0,
                  reinicio = False
                 }
-------------------------------------------------  VISUAL ------------------------------------------------------------------------------------------------------------------


visualization :: World -> Picture
visualization game@Game { pontoDardo = (posXdardo, posYdardo),
                          posicoesBaloes = ps,
                          score = s,
                          reinicio = r
                         } = 
                       
  telaDeReinicioVisual r &
  translated posXdardo posYdardo dard & 
  baloes ps & scoreVisual (s) &
  translated 0 0 cenario
    where
      nuvens = nuvem & translated (1.4) 0 nuvem

------------------------------------------------- FUNÇÕES VISUAIS ---------------------------------------------------------------------------------------------------------------------------------------------------

baloes :: [Point] -> Picture
baloes [] = blank
baloes ((xb, yb):ps) = translated xb yb balao & baloes ps


scoreVisual :: Int -> Picture
scoreVisual s = translated 0 (-9) (pontuacao & fundo)
  where
   pontuacao  = (styledLettering Bold Monospace ("SCORE : "<> T.pack (show s)  <>" "))
   fundo      = translated (-0.25) 0 (colored (darker 0.2 green) (solidRectangle 6.7 1.5))
   

telaDeReinicioVisual :: Bool -> Picture
telaDeReinicioVisual r
  | r == True = translated 0 0 (mensagemReinicio)
  | otherwise = blank
  where
    mensagemReinicio = letras1 & translated 0 (-2) letras2 & fundo
     where
      letras1 = (styledLettering Bold Monospace ("FIM DE JOGO"))
      letras2 = (styledLettering Bold Monospace ("PARA REINICIAR PRESSIONE 'R'"))
      fundo   = colored blue (translated 0 0 (solidRectangle 22 22)) 

------------------------------------------------- CONSTANTES ---------------------------------------------------------------------------------------------------------------------------------------------------

g = -5.5
velocidadeHorizontal = 15
velocidadeVerticalInicial = 4
velocidadePosicionamentoDardo = 15
velocidadeBalao = 4
periodo = 2
worldLimit = 10

---------------------------------------------- FUNÇÃO UPDATE ----------------------------------------------------------------------------------------------------------




update :: Event -> World -> World
update (KeyPress ("Down"))   game    =  game { direcaoDardo = Down }                  -- USE 'UP' E 'DOWN' PARA POSICIONAR O DARDO
update (KeyRelease ("Down")) game    =  game { direcaoDardo = Stop }
update (KeyPress ("Up"))     game    =  game { direcaoDardo = Up   }
update (KeyRelease ("Up"))   game    =  game { direcaoDardo = Stop }                   
update (KeyPress (" "))      game    =  game { ativacaoDardo = True}                   -- PRESSIONE ' ' PARA LANÇAR O DARDO
update (KeyPress ("R"))      game    =  game { pontoDardo = (-10,0),                   -- PRESSIONE 'R' PARA REINICIAR O GAME
                                               velYdardo = velocidadeVerticalInicial,
                                               ativacaoDardo = False,
                                               posicoesBaloes = [(0,-14)],
                                               numeroBaloes = 0,
                                               score = 0,
                                               reinicio = False
                                             }
update (TimePassing t) w = 
  reiniciaUpdate . dardUpdate t . balaoUpdate t $ w
update _ w = w 


---------------------------------------------- FUNÇÕES DARDO ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

lancamentoDardo :: Double -> World -> World
lancamentoDardo t game@Game { pontoDardo = (posXdardo, posYdardo),
                               velYdardo = velVertical
                            }  =
             
   game { pontoDardo = (novoX, novoY),
          velYdardo =  velVertical + g * t
        }
    where
      novoX = posXdardo + velocidadeHorizontal * t
      novoY = posYdardo + velVertical * t + 1/2 * g * t^2
      novaVelVertical = velVertical + g * t

dardUpdate :: Double -> World -> World
dardUpdate t game@Game { pontoDardo = (posXdardo, posYdardo), 
                         ativacaoDardo = a,
                         posicoesBaloes = ps,
                         score = s,
                         direcaoDardo = d
                       }
                       
  | posYdardo >= 10 && 
    converterDirecaoDardo d == 1     = game { pontoDardo = (posXdardo, paraDardoLimite)}
  | posYdardo <= -10 && 
    converterDirecaoDardo d == -1    = game { pontoDardo = (posXdardo, paraDardoLimite)}
 
  | posXdardo >= 10.5 ||
    posYdardo >= 10.5 ||   
    posYdardo <= -10.5               = game { pontoDardo = (-10, 0),
                                              velYdardo = velocidadeVerticalInicial,
                                              ativacaoDardo = False
                                            }

  | a  == True                       = lancaDardo
                                                        
  | otherwise                        = game { pontoDardo = (posXdardo, deslizaDardo)  }
    where 
      deslizaDardo = posYdardo + velocidadePosicionamentoDardo * direcaoConvertido * t
      sobrando = Prelude.filter (balaoSobrevive posXdardo posYdardo ) ps
      morre = Prelude.filter (colisao posXdardo posYdardo ) ps
      aumentaScore = s + (length morre)
      lancaDardo = lancamentoDardo t game { posicoesBaloes = sobrando, score = aumentaScore }
      direcaoConvertido = converterDirecaoDardo d
      paraDardoLimite = posYdardo + velocidadePosicionamentoDardo * t 
                        * converterDirecaoDardo Stop
      
converterDirecaoDardo :: DirecaoDardo -> Double
converterDirecaoDardo Up     = 1
converterDirecaoDardo Down   = -1
converterDirecaoDardo Stop   = 0

----------------------------------------- FUNÇÕES DE ESTADOS BOOLEANOS ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

haBaloesNaTela :: [Point] -> Bool
haBaloesNaTela ps = [yB | (_, yB) <- ps, yB < 12.5] /= []

balaoSobrevive :: Double -> Double -> Point -> Bool
balaoSobrevive posXdardo posYdardo (xb, yb) = 
     not (colisao posXdardo posYdardo (xb, yb))
     
     
---------------------------------------------- FUNÇÕES BALÕES ------------------------------------------------------------------------------------------------------------------------------------------                                          
                                           
                                           
balaoUpdate :: Double -> World -> World
balaoUpdate t  game@Game { posicoesBaloes = ps,
                           timer = tm,
                           listaRandomica = nxs,
                           numeroBaloes = nb
                         }
 
  | ntm <= 0             = game { posicoesBaloes = spawnDeBalao,
                                  timer = periodo,
                                  numeroBaloes = nb + 1
                                }
                                                    
  | otherwise            = game { posicoesBaloes = nps,
                                  timer = ntm
                                }
   where
     ntm = tm - t
     nps = atualizaBaloes t ps
     spawnDeBalao   = ([fromIntegral x | x <- nxs] !! nb, -14) : nps
     
     
atualizaBaloes :: Double -> [Point] -> [Point]
atualizaBaloes t ps = [ (xb, yb + velocidadeBalao * t) | (xb, yb) <- ps ]


--------------------------------------------------- COLISÃO ------------------------------------------------------------------------------------------------------------------------------------------

colisao :: Double -> Double -> Point -> Bool
colisao posXdardo yDardo (xB, yB) =
    pontaDardo >= xB - 0.7 && pontaDardo <= xB + 0.7 && yDardo >= yB - 1 && yDardo <= yB + 1          
        where
           pontaDardo = posXdardo + 1.2  -- APENAS A PONTA DO DARDO É AFIADA
        
---------------------------------------------- FUNÇÃO REINICIAR ------------------------------------------------------------------------------------------------------------------------------------------


reiniciaUpdate :: World -> World
reiniciaUpdate game@Game { posicoesBaloes = ps,
                           listaRandomica = nxs,
                           score = s,
                           numeroBaloes = nb
                         }
  | ps == [] && length nxs /= length ps + s   = game
  | ps == [] && length nxs == length ps + s   = game  { timer = periodo,
                                                       numeroBaloes = 0,
                                                       reinicio = True
                                                      }
  | snd(head ps) >= 12                        = game  { timer = periodo,
                                                       numeroBaloes = 0,
                                                       reinicio = True
                                                      }
  | not (haBaloesNaTela ps) && 
  length nxs == length ps + s                 = game { timer = periodo,
                                                       numeroBaloes = 0,
                                                       reinicio = True
                                                      }     
   
  | length nxs == length ps + s               = game { timer = periodo,
                                                       numeroBaloes = 0,
                                                       reinicio = False
                                                     }
                                             
  | otherwise                                 = game



---------------------------------------------- PICTURES ---------------------------------------------------------------------------------------------------------------


nuvem :: Picture
nuvem = colored white      
            (solidRectangle 2 0.1 &
            translated 0 0.1 (solidRectangle 2.5 0.1) &
            translated 0 0.2 (solidRectangle 2.5 0.1) &
            translated 0 0.3 (solidRectangle 2.8 0.1) &
            translated 0 0.4 (solidRectangle 2.8 0.1) &
            translated (-0.1) 0.5 (solidRectangle 2.4 0.2) &
            translated (-0.5) 0.6 (solidRectangle 1.2 0.1) &
            translated (-0.5) 0.7 (solidRectangle 1 0.1) &
            translated (-0.5) 0.8 (solidRectangle 0.8 0.1) &
            translated (-0.5) 0.9 (solidRectangle 0.4 0.1) &
            translated (-0.5) 1 (solidRectangle 0.2 0.1) &
            translated (0.5) 0.6 (solidRectangle 1.2 0.1) &
            translated (0.5) 0.7 (solidRectangle 1 0.1) &
            translated (0.5) 0.8 (solidRectangle 0.8 0.1) &
            translated (0.5) 0.9 (solidRectangle 0.4 0.1))

cadeiaDeNuvens :: Double -> Picture            
cadeiaDeNuvens n = pictures [translated (1.4 * x) 0 nuvem | x <- [0..n]] &
           pictures [translated (1.4 * x) (-0.8) nuvem | x <- [0..n]]
                                            
cenario =  translated 8 7 sol & nuvens & ceu
  where
    ceu = colored (lighter 0.2 blue) (solidRectangle 40 40)
    sol = colored (brighter 100 (lighter 0.1 (mixed [orange, yellow]))) 
          (solidCircle 0.6) 
                                 &
          colored (brighter 100 orange) (solidCircle 0.8)
    nuvens = translated (-9) 9 (cadeiaDeNuvens 20)
    
balao = cauda & cabeca & barbante
  where
    cabeca = colored red (solidClosedCurve [(0,-1), (-0.7,0.3), (0,1), (0.7,0.3), (0,-1)])
    cauda = colored red (translated 0 (-0.85) (solidRectangle 0.3 0.3))
    barbante = curve [(0,-1), (0.1, -1.1), (0, -1.2), (-0.1, -1.3), (0,-1.4), 
                     (0.1, -1.5), (0, -1.6), (-0.1, -1.7), (0, -1.8), (0.1, -1.9),
                     (0, -2)]
                     
dard :: Picture                     
dard = cabeca & corpo & cauda
  where
    cabeca = colored gray (solidPolygon [(0.5,0.3), (0.5, -0.3), (1.2,0)])
    corpo = colored brown (solidRectangle 1.75 0.25)
    cauda = colored (mixed [brown, yellow, gray]) (solidPolygon
        [(-1.4, 0), (-1.4, 0.25), (-1, 0.25), (-0.87, 0.125), (-0.87, 0)] & 
        solidPolygon 
        [(-1.4, 0), (-1.4, -0.25), (-1, -0.25), (-0.87, -0.125), (-0.87, 0)])





