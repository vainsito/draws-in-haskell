module Dibujos.Antropia where

import Graphics.Gloss (line, Picture(Blank))

import qualified Graphics.Gloss.Data.Point.Arithmetic as V

import Dibujo (Dibujo, figura, juntar)
import FloatingPic (Conf(..), Output, half)


type Antropico = Bool

dibBasica :: Dibujo Antropico
dibBasica = figura True

dibVacio :: Dibujo Antropico
dibVacio = figura False

-- Recursion de lineas que forman patrones visuales "Antropicos"
-- Representa el caos de la mente humana y el continuo cambio
antrop :: Int -> Antropico -> Dibujo Antropico
antrop 0 _ = dibVacio
antrop 1 _ = dibBasica
antrop n p = juntar 1 2 s (juntar 1 1 s s)
    where s = antrop (n-1) p

interpBas :: Output Antropico
interpBas False _ _ _ = Blank
interpBas True x y w = line $ map (x V.+) [(0,0), w V.+ half y, y, (0,0)]

antropiaConf :: Conf
antropiaConf = Conf {
    name = "Antropia",
    pic = antrop 6 True,
    bas = interpBas
}