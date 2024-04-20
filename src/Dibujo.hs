{-# LANGUAGE LambdaCase #-}
module Dibujo (
    Dibujo,
    figura, rotar, espejar, rot45, apilar, juntar, encimar,
    r180, r270,
    (.-.), (///), (^^^),
    cuarteto, encimar4, ciclar,
    foldDib, mapDib, change
) where



{- Gramática de las figuras: 
<Fig> ::= Figura <Bas> 
    | Rotar <Fig> 
    | Espejar <Fig> 
    | Rot45 <Fig>
    | Apilar <Float> <Float> <Fig> <Fig> 
    | Juntar <Float> <Float> <Fig> <Fig> 
    | Encimar <Fig> <Fig>
-}

{- definimos la estructura de datos Dibujos con sus correspondientes operadores (Traduciendo lo de arriba) -}
data Dibujo a 
              = Figura a
              | Rotar (Dibujo a)
              | Espejar (Dibujo a)
              | Rot45 (Dibujo a)
              | Apilar Float Float (Dibujo a) (Dibujo a)
              | Juntar Float Float (Dibujo a) (Dibujo a)
              | Encimar (Dibujo a) (Dibujo a)
              deriving (Eq, Show)

-- combinadores
infixr 6 ^^^

infixr 7 .-.

infixr 8 ///

-- lo que hace comp es aplicar n veces la función f a la figura x
comp :: (a -> a) -> Int -> a -> a
comp f n x = if n <= 0 then x else f (comp f (n-1) x)

-- Funciones constructoras
figura :: a -> Dibujo a
figura = Figura

encimar :: Dibujo a -> Dibujo a -> Dibujo a
encimar = Encimar

apilar :: Float -> Float -> Dibujo a -> Dibujo a -> Dibujo a
apilar = Apilar

juntar  :: Float -> Float -> Dibujo a -> Dibujo a -> Dibujo a
juntar = Juntar

rot45 :: Dibujo a -> Dibujo a
rot45 = Rot45

rotar :: Dibujo a -> Dibujo a
rotar = Rotar

espejar :: Dibujo a -> Dibujo a
espejar = Espejar

(.-.) :: Dibujo a -> Dibujo a -> Dibujo a 
(.-.) a b = Apilar 1 1 a b 

-- Pone una figura al lado de la otra, ambas ocupan el mismo espacio.
(///) :: Dibujo a -> Dibujo a -> Dibujo a 
(///) a b = Juntar 1 1 a b 

-- Superpone una figura con otra.
(^^^) :: Dibujo a -> Dibujo a -> Dibujo a 
(^^^) a b = Encimar a b 

-- rotaciones
r90 :: Dibujo a -> Dibujo a
r90 dibujo = rotar dibujo 

r180 :: Dibujo a -> Dibujo a  
r180 dibujo = comp r90 2 dibujo

r270 :: Dibujo a -> Dibujo a
r270 dibujo = comp rotar 3 dibujo

-- Dadas cuatro figuras las ubica en los cuatro cuadrantes.
cuarteto :: Dibujo a -> Dibujo a -> Dibujo a -> Dibujo a -> Dibujo a
cuarteto p q r s = (.-.) ((///) p q) ((///) r s) 

-- Una figura repetida con las cuatro rotaciones, superpuestas.
encimar4 :: Dibujo a -> Dibujo a 
encimar4 a = (^^^) a ((^^^) ((^^^) (rotar a) (r180 a)) (r270 a))

-- un cuarteto donde se repite la imagen, rotada (¡No confundir con encimar4!)
ciclar :: Dibujo a -> Dibujo a
ciclar p = cuarteto p (rotar p) (r180 p) (r270 p)

-- map para nuestro lenguaje
mapDib :: (a -> b) -> Dibujo a -> Dibujo b
mapDib f (Figura a) = Figura (f a)
mapDib f (Rotar a) = Rotar (mapDib f a)
mapDib f (Espejar a) = Espejar (mapDib f a)
mapDib f (Rot45 a) = Rot45 (mapDib f a)
mapDib f (Apilar x y a b) = Apilar x y (mapDib f a) (mapDib f b)
mapDib f (Juntar x y a b) = Juntar x y (mapDib f a) (mapDib f b)
mapDib f (Encimar a b) = Encimar (mapDib f a) (mapDib f b)

-- verificar que las operaciones satisfagan
-- 1. map figura = id
-- 2. map (g . f) = mapDib g . mapDib f

-- Cambiar todas las básicas de acuerdo a la función.
change :: (a -> Dibujo b) -> Dibujo a -> Dibujo b
change f (Figura a) = f a {- Ni idea de si esta bien -}
change f (Rotar a) = Rotar (change f a)
change f (Espejar a) = Espejar (change f a)
change f (Rot45 a) = Rot45 (change f a)
change f (Apilar x y a b) = Apilar x y (change f a) (change f b)
change f (Juntar x y a b) = Juntar x y (change f a) (change f b)
change f (Encimar a b) = Encimar (change f a) (change f b)

-- Principio de recursión para Dibujos.
foldDib ::
  (a -> b) ->
  (b -> b) ->
  (b -> b) ->
  (b -> b) ->
  (Float -> Float -> b -> b -> b) ->
  (Float -> Float -> b -> b -> b) ->
  (b -> b -> b) ->
  Dibujo a ->
  b
foldDib f _ _ _ _ _ _ (Figura a) = f a
foldDib f g h i j k l (Rotar a) = g (foldDib f g h i j k l a)
foldDib f g h i j k l (Espejar a) = h (foldDib f g h i j k l a)
foldDib f g h i j k l (Rot45 a) = i (foldDib f g h i j k l a)
foldDib f g h i j k l (Apilar x y a b) = j x y (foldDib f g h i j k l a) (foldDib f g h i j k l b)
foldDib f g h i j k l (Juntar x y a b) = k x y (foldDib f g h i j k l a) (foldDib f g h i j k l b)
foldDib f g h i j k l (Encimar a b) = l (foldDib f g h i j k l a) (foldDib f g h i j k l b)
