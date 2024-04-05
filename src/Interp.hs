module Interp
  ( interp,
    initial,
  )
where

import Dibujo
import FloatingPic
import Graphics.Gloss (Display (InWindow), color, display, makeColorI, pictures, translate, white, Picture)
import qualified Graphics.Gloss.Data.Point.Arithmetic as V

-- Dada una computación que construye una configuración, mostramos por
-- pantalla la figura de la misma de acuerdo a la interpretación para
-- las figuras básicas. Permitimos una computación para poder leer
-- archivos, tomar argumentos, etc.
initial :: Conf -> Float -> IO ()
initial (Conf n dib intBas) size = display win white $ withGrid fig size
  where
    win = InWindow n (ceiling size, ceiling size) (0, 0)
    fig = interp intBas dib (0, 0) (size, 0) (0, size)
    desp = -(size / 2)
    withGrid p x = translate desp desp $ pictures [p, color grey $ grid (ceiling $ size / 10) (0, 0) x 10]
    grey = makeColorI 100 100 100 100

--- Figuras
rectangle :: FloatingPic
rectangle x y w = line [x, x V.+ y, x V.+ y V.+ w, x V.+ w, x]

triangle :: FloatingPic
triangle x y w = line $ map (x V.+) [(0,0), y V.+ half w, w, (0,0)]

crux :: FloatingPic
crux x y w = pictures [line [x, x V.+ y V.+ w], line [x V.+ y, x V.+ w]]

efe :: FloatingPic
efe x y w = line . map (x V.+) $ [
        zero,uX, p13, p33, p33 V.+ uY , p13 V.+ uY,
        uX V.+ 4 V.* uY ,uX V.+ 5 V.* uY, x4 V.+ y5,
        x4 V.+ 6 V.* uY, 6 V.* uY, zero
    ]
    where
        p33 = 3 V.* (uX V.+ uY)
        p13 = uX V.+ 3 V.* uY
        x4 = 4 V.* uX
        y5 = 5 V.* uY
        uX = (1/6) V.* y
        uY = (1/6) V.* w

--- V.+ y V.- serian la suma y resta de vectores
--- V.* multiplicacion
--- V.negate es negar el vector

half :: Vector -> Vector
half v = (0.5 V.* v)

-- Interpretación de (^^^)
ov :: Picture -> Picture -> Picture
ov = undefined

r45 :: FloatingPic -> FloatingPic
r45 f d w h = f (d V.+ half(w V.+ h))   (half(w V.+ h)) (half(h V.- w))

rot :: FloatingPic -> FloatingPic
rot f d w h = f  (d V.+ w) (h) (V.negate w)

esp :: FloatingPic -> FloatingPic
esp f d w h = f (d V.+ w) (V.negate w) (h)

sup :: FloatingPic -> FloatingPic -> FloatingPic
sup f1 f2 d w h = pictures [f1 d w h, f2 d w h]

jun :: Float -> Float -> FloatingPic -> FloatingPic -> FloatingPic
jun m n f1 f2 d w h = pictures [f1 (d) (w') (h), f2 (d V.+ w') (r' V.* w) (h)]
  where
    r' = n / (m + n)
    r = m / (m + n)
    w' = r V.* w

api :: Float -> Float -> FloatingPic -> FloatingPic -> FloatingPic
api x y f1 f2 d w h = pictures [f1 (d V.+ h') (w) (r V.* h), f2 () () ()]
  where
    r' = n / (m + n)
    r = m / (m + n)
    h' = r' V.* h

interpRotar = rot (interp f a)

interp :: Output a -> Output (Dibujo a)
interp f a = foldDib f rot es r45 api jun ov a
