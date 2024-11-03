# INFO188
## Descripcion
Este programa es un juego implementado en haskel que se puede jugar entre dos personas en un mismo teclado,y ademas es jugado en el treminal.
El juego consiste en una batalla entre dos catapultas, donde el ganador es el que logra destruir al rival con proyectiles
## Reglas del juego
- **Objetivo**: Destruir la catapulta del oponente,cada catapulta tiene 30Hp,100 de combustible.
- **Turnos**: Cada jugador tiene un turno para lanzar un proyectil.
- **Movimiento**: Cada jugador puede mover su catapulta hacia la izquierda o derecha, gastara 10 de combustible en cada movimiento.
- **Ángulo de disparo**: Cada jugador puede ajustar el ángulo de disparo de su catapulta,gastara 2 de combustible cada ajuste de angulo.
- **Disparo**: Cada jugador puede lanzar un proyectil en la dirección que desee, gastara 0 de combustible y dañara entre 1 y 3 de Hp a la otra catapulta.
- **Ganador**: El jugador que destruya la catapulta del oponente gana la partida, es decir la catapulta que llegue a 0 estara destruida.

## Controles
- **Movimiento**: Utilizar las teclas `A` y `D` para mover la catapulta hacia la izquierda y derecha respectivamente.
- **Ángulo de disparo**: Utilizar las teclas `W` y `S` para ajustar el ángulo de disparo.
- **Disparo**: Utilizar la tecla `Espacio` para lanzar un proyectil.
- **Salir**: Utilizar la tecla `q` para salir del juego.

## Requisitos
- **Haskel**: Contar con Haskel 8.6 o superior.
- **Cabal**: Contar con cabal 3.10 o superior.
- **Random para haskel**: Modulo pra numeros aleatorios haskel System.Random.
- **Console Ansi**:Modulo para manipular la consola System.Console.Ansi
- **Terminal**: Acceso a una terminal para compilar y ejecutar el programa utilizando línea de comandos.
## Instalacion Libereias
**1.** Libreria random: 

```bash
cabal install random
cabal install --lib random
```

**2.** Libreria console-ansi:
```bash
 cabal install console-ansi
 cabal install --lib console-ansi
```

## Compilacion/Ejecucion
Para compilar el programa debe estar en la raiz del proyecto y ejecutar el siguiente comando:
```bash
    make
```
Para ejecutar el programa debe estar en la capreta `bin` del proyecto y ejecutar el siguiente comando:
```bash
    ./canowars
```
