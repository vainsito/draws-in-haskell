---
title: Laboratorio de Funcional
author: Ezequiel Marin, Alexis Ortiz
---
La consigna del laboratorio está en https://tinyurl.com/funcional-2024-famaf

# 1. Tareas
Pueden usar esta checklist para indicar el avance.

## Verificación de que pueden hacer las cosas.
- [x] Haskell instalado y testeos provistos funcionando. (En Install.md están las instrucciones para instalar.)

## 1.1. Lenguaje
- [x] Módulo `Dibujo.hs` con el tipo `Dibujo` y combinadores. Puntos 1 a 3 de la consigna.
- [x] Definición de funciones (esquemas) para la manipulación de dibujos.
- [x] Módulo `Pred.hs`. Punto extra si definen predicados para transformaciones innecesarias (por ejemplo, espejar dos veces es la identidad).

## 1.2. Interpretación geométrica
- [x] Módulo `Interp.hs`.

## 1.3. Expresión artística (Utilizar el lenguaje)
- [x] El dibujo de `Dibujos/Feo.hs` se ve lindo.
- [x] Módulo `Dibujos/Grilla.hs`.
- [x] Módulo `Dibujos/Escher.hs`.
- [x] Listado de dibujos en `Main.hs`.

## 1.4 Tests
- [x] Tests para `Dibujo.hs`.
- [x] Tests para `Pred.hs`.

# 2. Experiencia
### ¿Qué nos pareció la experiencia de programar en Haskell?
Si bien al principio nos costo un poco acostumbrarnos a la sintaxis y a la forma de programar en Haskell, una vez que le agarramos la mano nos resulto muy comodo y sencillo.
Fue muy interesante este desafio ya que nos permitio entender y asimilar el concepto de paradigma funcional, con la que no habiamos trabajado con anterioridad.

### ¿Qué nos pareció la idea del laboratorio?
La idea del laboratorio nos parecio muy interesante y creativa.
Fue interesante aprender a representar dibujos de manera abstracta y luego interpretarlos de manera grafica.
La idea de separar las funcionalidades en modulos nos parecio muy acertada ya que nos permitio trabajar de manera mas organizada y eficiente.

Destacamos la idea de la interpretacion grafica luego de haber definido los dibujos de manera abstracta, ya que nos permitio ver de manera visual el resultado de nuestro trabajo.

### ¿Qué dificultades tuvimos?
Tuvimos bastantes dificultades, sobre todo adaptandonos a la sintaxis de Haskell y a la forma de programar en este lenguaje.
Lo que mas nos complico fue entender como debiamos interpretar vectorialmente los dibujos, y como debiamos definir las funciones para manipularlos.

Tambien al principio nos costo mucho la configuracion del entorno de trabajo, ya que cabal nos generaba muchos problemas y no podiamos instalar las dependencias necesarias.

### ¿Cómo fue nuestra dinámica de trabajo?
Nuestra dinamica de trabajo fue muy buena, ya que pudimos organizarnos y dividir las tareas de manera equitativa.
Generalmente trabajabamos juntos, utilizando la herramienta de `Live Share` de Visual Studio Code, lo que nos permitio trabajar en tiempo real y compartir el codigo de manera sencilla y tambien para comunicarnos utilizabamos `Discord`.

Cuando realizabamos cambios, lo documentabamos en un documento de `Notion` para que el otro integrante pueda entender los cambios realizados.

# 3. Preguntas
Al responder tranformar cada pregunta en una subsección para que sea más fácil de leer.

- [3.1 ¿Por qué están separadas las funcionalidades en los módulos indicados?](#31-por-qué-están-separadas-las-funcionalidades-en-los-módulos-indicados)
- [3.2 ¿Por qué las figuras básicas no están incluidas en la definición del lenguaje, y en vez de eso, es un parámetro del tipo?](#32-por-qué-las-figuras-básicas-no-están-incluidas-en-la-definición-del-lenguaje-y-en-vez-de-eso-es-un-parámetro-del-tipo)
- [3.3 ¿Qué ventaja tiene utilizar una función de `fold` sobre hacer pattern-matching directo?](#33-por-qué-ventaja-tiene-utilizar-una-función-de-fold-sobre-hacer-pattern-matching-directo)

## 3.1 ¿Por qué están separadas las funcionalidades en los módulos indicados?

- **Claridad y organización**:
 Cada módulo tiene su tarea y  propósito, esto facilita la comprensión del código. 
- **Mantenimiento más sencillo**:
Ya que los modulos son independientes entre si, cuando se realizan cambios o se corrigen errores es mas facil indentificar la parte especifica del codigo que necesita ser corregida.
- **Mejor colaboración**:
Con esta separacion de responsabilidades de cada modulo pudimos separar las tareas de cada integrante.

## 3.2 ¿Por qué las figuras básicas no están incluidas en la definición del lenguaje, y en vez de eso, es un parámetro del tipo?
No sabemos a priori qué figuras básicas tendremos, y por eso el tipo Dibujo debe ser polimórfico. Una figura basica podria ser cualquier letra del abecedario, un elipse, o un octagono, cada uno con una implementacion diferente.

## 3.3 ¿Qué ventaja tiene utilizar una función de `fold` sobre hacer pattern-matching directo?
La ventaja de `fold` es que se puede crear un codigo mucho mas sencillo y comprensible. Por ejemplo, la utilizamos en `interp.hs` en la funcion `interp` de esta manera:

```haskell
interp :: Output a -> Output (Dibujo a)
interp f = foldDib f rot esp r45 api jun sup 
```

Con `foldDib` logramos hacer la funcion en una sola linea. En cambio al hacer pattern-matching hubiera resultado asi:

```haskell
interp :: Output a -> Output (Dibujo a)
interp f Vacia = simple blank
interp f (Basica a) = f a
interp f (Rot90 a) = rot (interp f a)
interp f (Rot45 a) = rot45 (interp f a)
interp f (Espejar a) = esp (interp f a)
interp f (Apilar x y a b) = api x y (interp f a) (interp f b)
interp f (Juntar x y a b) = jun x y (interp f a) (interp f b)
interp f (Encimar a b) = ov (interp f a) (interp f b)
```

Tambien utilizamos `foldDib` muchas veces en el modulo `pred.hs`.
En resumen, utilizando foldib logramos mayor legibilidad y simplicidad en el codigo, ademas de la reutilizacion de codigo.

## 3.4 ¿Cuál es la diferencia entre los predicados definidos en Pred.hs y los tests?

Los archivos Pred.hs y TestPred.hs cumplen roles diferentes:

- Pred.hs contiene la definicion de funciones (predicados) que realizan ciertas operaciones o verificaciones en nuestros datos.

- TestPred.hs contiene tests que verifican que las funciones en Pred.hs funcionen correctamente. 
Cada test en TestPred.hs usa una funcion de Pred.hs, le pasa algunos valores y verifica que el resultado sea el que nosotros queremos.

Basicamente, Pred.hs define el comportamiento, y TestPred.hs verifica ese comportamiento.

# 4. Extras
- [x] Dibujos adicionales: 
Agregamos un dibujo adicional llamado `Dibujos/Antropia.hs`.
Este dibujito es una sucesion recursiva de lineas que forman un efecto visual cuando se acerca y se aleja.

