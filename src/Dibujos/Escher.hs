module Dibujos.Escher where

import Graphics.Gloss (Picture(Blank), blue, red, yellow, color, line, polygon, pictures)

import Dibujo (Dibujo, figura, rotar, encimar4, espejar, (^^^), cuarteto, r270, r180, rot45)
import FloatingPic(Conf(..), Output, half, zero)
import GrillaFun(grilla)

import qualified Graphics.Gloss.Data.Point.Arithmetic as V


-- Supongamos que eligen.
type Escher = Bool

-- Definimos los tipos de dibujos basicos que vamos a usar.
dibBase :: Dibujo Escher
dibBase = figura True

dibVacio :: Dibujo Escher
dibVacio = figura False

-- El dibujo u.
dibujoU :: Dibujo Escher -> Dibujo Escher
dibujoU p = encimar4 (espejar (rot45 p)) -- Arma el cuadradito con las figuras rotadas y espejadas.

-- El dibujo t.
--- Es una superposicion con la mitad de la figura de u, superpuesta en una figura P
dibujoT :: Dibujo Escher -> Dibujo Escher
dibujoT p = p ^^^ (fig1 ^^^ fig2)
    where
        fig1 = espejar (rot45 p)
        fig2 = r270 (fig1)

-- Esquina con nivel de detalle en base a la figura p.
esquina :: Int -> Dibujo Escher -> Dibujo Escher
esquina 1 p = cuarteto dibVacio dibVacio dibVacio (dibujoU p)
esquina n p = cuarteto corner side (rotar side) (dibujoU p)
    where
        corner = esquina (n-1) p
        side = lado (n-1) p

-- Lado con nivel de detalle.
lado :: Int -> Dibujo Escher -> Dibujo Escher
lado 1 p = cuarteto dibVacio dibVacio (rotar t) t  where t = dibujoT p
lado n p = cuarteto side side (rotar t) t
    where 
        t = dibujoT p
        side = lado(n-1) p

-- Por suerte no tenemos que poner el tipo!
-- En teoria podemos usar la grilla que se define en Grilla.hs, porque noneto ubica las figuras en cada "cuadrante"
-- Entonces utilizo la funcion grilla, ubicando cada figura en su lugar correspondiente.
noneto p q r s t u v w x = grilla [[p, q, r], 
                                    [s, t, u], 
                                    [v, w, x]]

-- El dibujo de Escher:
escher :: Int -> Escher -> Dibujo Escher
escher n p = noneto (esquina n (figura p))
                    (lado n (figura p))
                    (r270 (esquina n (figura p)))
                    (rotar (lado n (figura p)))
                    (dibujoU (figura p))
                    (r270 (lado n (figura p)))
                    (rotar (esquina n (figura p)))
                    (r180 (lado n (figura p)))
                    (r180 (esquina n (figura p)))

-- Ahora debo definir la funcion de interpretacion para el dibujo de Escher.
-- La idea es que si es False, entonces no dibuje nada, y si es True, dibuje una figura.
-- Uso picture para armar las figuras usando triangulos y polygonos y lineas
interpBasEscher :: Output Escher
interpBasEscher False _ _ _ = Blank
interpBasEscher True x y w = color blue $ pictures [ line $ triangulo x y w]
    where 
        triangulo a b c =  map (a V.+) [zero, b, c, zero]
        -- Estas coords son para el triangulo, usamos halfs de y y w para que quede centrado

escherConf :: Conf
escherConf = Conf {
    name = "Escher",
    pic = escher 3 True,
    bas = interpBasEscher
}