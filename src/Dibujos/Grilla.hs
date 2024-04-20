-- En este archivo quiero dibujar una grilla de 8 bloques,
-- En cada bloque debe haber una figura de un Numero correspondiente a la posicion en la grilla (0-7)

module Dibujos.Grilla where

import Interp (interp)
import Dibujo (Dibujo, figura, juntar, apilar)
import FloatingPic(Conf(..), Output, half, zero)
import qualified Graphics.Gloss.Data.Point.Arithmetic as V
import Graphics.Gloss ( Picture (Blank), text, translate, scale)

-- Defino Tipos
type Basica = (Int, Int)

row :: [Dibujo a] -> Dibujo a
row [] = error "row: no puede ser vacío"
row [d] = d
row (d:ds) = juntar 1 (fromIntegral $ length ds) d (row ds)

column :: [Dibujo a] -> Dibujo a
column [] = error "column: no puede ser vacío"
column [d] = d
column (d:ds) = apilar 1 (fromIntegral $ length ds) d (column ds)

grilla :: [[Dibujo a]] -> Dibujo a
grilla = column . map row


-- Construimos una grilla de tamanio arbitrario

-- En esta funcion voy a armar la figura (x, y) de la grilla, donde x e y son las posiciones de la grilla
-- 
dibujoItem :: Int -> Int -> Dibujo Basica
dibujoItem x y = figura (x, y)

dibujoGrilla :: Dibujo Basica 
dibujoGrilla = grilla [[dibujoItem x y | y <- [0..7]] | x <- [0..7]]

-- En esta funcion voy a trasladar la figura a la posicion (x, y) de la grilla, haciendo
-- Operaciones con los vectores

interpBas:: Output Basica
interpBas gridItem (x, y) (w,_) (_,h) = translate xPos yPos texto
    where
        scaleTam = 0.15
        xPos = x + w/4
        yPos = y + h/4
        texto = scale scaleTam scaleTam $ text $ show gridItem


grillaConf :: Conf
grillaConf = Conf {
    name = "Grilla",
    pic = dibujoGrilla,
    bas = interpBas
}