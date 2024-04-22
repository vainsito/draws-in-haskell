# LABO

---

## Preguntas
### ¿Por qué separamos las funcionalidades en los módulos indicados?

- **Claridad y organización**:
 Cada módulo tiene su tarea y  propósito, esto facilita la comprensión del código. 
- **Mantenimiento más sencillo**:
Ya que los modulos son independientes entre si, cuando se realizan cambios o se corrigen errores es mas facil indentificar la parte especifica del codigo que necesita ser corregida.
- **Mejor colaboración**:
Con esta separacion de responsabilidades de cada modulo pudimos separar las tareas de cada integrante.

### Explicacion de los modulos
- **Dibujo.hs**: Define el tipo principal para representar dibujos y la sintaxis de las funciones básicas para operar con ellos.
- **Interp.hs**: Es la semantica del lenguaje, toma un dibujo y devuelve su interpretacion grafica con Gloss. 
- **Pred.hs**: Define funciones para trabajar con predicados sobre dibujos.
- **Main.hs**: Permite que un usuario eliga el dibujo a representar.

---

### ¿Por qué las figuras básicas no están incluidas en la definición del lenguaje, y en vez de eso, es un parámetro del tipo?
no sabemos a priori qué figuras básicas tendremos, y por eso el  tipo Dibujo debe ser polimórfico. Una figura basica podria ser cualquier letra del abecedario, un elipse, o un octagono, cada uno con una implementacion diferente.

---

### ¿Qué ventaja tiene utilizar una función de `fold` sobre hacer pattern-matching directo?

La ventaja de `fold` es que se puede crear un codigo mucho mas sencillo y comprensible. Por ejemplo, la utilizamos en `interp.hs` en la funcion `interp` de esta manera:

`
interp :: Output a -> Output (Dibujo a)
interp f = foldDib f rot esp r45 api jun sup 
`

Con `foldDib` logramos hacer la funcion en una sola linea. En cambio al hacer pattern-matching hubiera resultado asi:

`
interp :: Output a -> Output (Dibujo a)
interp f Vacia = simple blank
interp f (Basica a) = f a
interp f (Rot90 a) = rot (interp f a)
interp f (Rot45 a) = rot45 (interp f a)
interp f (Espejar a) = esp (interp f a)
interp f (Apilar x y a b) = api x y (interp f a) (interp f b)
interp f (Juntar x y a b) = jun x y (interp f a) (interp f b)
interp f (Encimar a b) = ov (interp f a) (interp f b)
`

Tambien utilizamos `foldDib` muchas veces en el modulo `pred.hs` para mejorar la legibilidad. 

---

### ¿Cuál es la diferencia entre los predicados definidos en Pred.hs y los tests?
Los archivos Pred.hs y TestPred.hs cumplen roles diferentes:

- Pred.hs contiene la definicion de funciones (predicados) que realizan ciertas operaciones o verificaciones en nuestros datos.

- TestPred.hs contiene tests que verifican que las funciones en Pred.hs funcionen correctamente. 
Cada test en TestPred.hs usa una funcion de Pred.hs, le pasa algunos valores y verifica que el resultado sea el que nosotros queremos.

Basicamente, Pred.hs define el comportamiento, y TestPred.hs verifica ese comportamiento.