module Pred (
  Pred,
  cambiar, anyDib, allDib, orP, andP, falla
) where

type Pred a = a -> Bool

-- Dado un predicado sobre básicas, cambiar todas las que satisfacen
-- el predicado por la figura básica indicada por el segundo argumento.
cambiar :: Pred a -> (a -> Dibujo a) -> Dibujo a -> Dibujo a
-- Uso mapdib para aplicarle el predicado a cada figura y si cumple lo cambio por la figura que me pasan
cambiar predicado figura = mapDib (\x -> if predicado x then figura x else x)

-- Alguna básica satisface el predicado.
anyDib :: Pred a -> Dibujo a -> Bool
-- Uso foldDib ya que tengo que recorrer todo el dibujo y ver si alguno cumple el predicado
anyDib predicado = foldDib predicado id id id (\_ _ a b -> a || b) (\_ _ a b -> a || b) (||) (\_ _ a -> a)

-- Todas las básicas satisfacen el predicado.
allDib :: Pred a -> Dibujo a -> Bool
-- Debo recorrer todo el dibujo y ver si todas las figuras cumplen el predicado por eso uso foldDib
allDib predicado = foldDib predicado id id id (\_ _ a b -> a && b) (\_ _ a b -> a && b) (&&) (\_ _ a -> a)
-- Los dos predicados se cumplen para el elemento recibido.
andP :: Pred a -> Pred a -> Pred a
andP pred1 pred2 elemento = pred1 elemento && pred2 elemento 

-- Algún predicado se cumple para el elemento recibido.
orP :: Pred a -> Pred a -> Pred a
orP pred1 pred2 elemento = pred1 elemento || pred2 elemento

-- Esta linea de abajo sirve para el TestPred.hs, cuando no esta implementado Pred, falla = True
falla = False