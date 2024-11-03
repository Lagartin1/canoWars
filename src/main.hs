module Main where

import System.Console.ANSI
import System.Random
import Control.Monad
import System.IO
import Data.Array.Accelerate.LLVM.Native.Foreign (Async(put))

import Utils(drawGame, initialGameState, gameOver, handleInput, GameState, player1, hp)



gameLoop :: GameState -> IO ()
gameLoop game = do
    drawGame game
    if gameOver game
        then do
            putStrLn $ "¡" ++ winner ++ " ha ganado!"
            return ()
        else do
            hSetBuffering stdin NoBuffering
            c <- getChar
            newGame <- handleInput c game
            gameLoop newGame
    where
        winner = if hp (player1 game) <= 0 then "Jugador 2" else "Jugador 1"

main :: IO ()
main = do
    showWellcomeMessage
    hSetEcho stdin False
    clearScreen
    gameLoop initialGameState


showWellcomeMessage :: IO ()
showWellcomeMessage = do
    putStrLn "Bienvenido a CanoWars!"
    putStrLn "'a' y 'd' <- para mover la catapulta."
    putStrLn "'w' y 's' <- para ajustar el ángulo de disparo."
    putStrLn "'espacio' <- para disparar."
    putStrLn "'q' <- para salir del juego."
    putStrLn "¡Que gane el mejor!"
    putStrLn "presiona cualquier tecla para continuar"
    _ <- getChar
    return ()