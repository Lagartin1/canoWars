-- Main.hs
module Main where

import Utils (GameState, initializeGame, takeTurn, displayGameState, checkGameOver)
import System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- Desactivar el buffering para experiencia en tiempo real
    gameState <- initializeGame
    gameLoop gameState

gameLoop :: GameState -> IO ()
gameLoop state = do
    displayGameState state
    if checkGameOver state
        then putStrLn "¡Juego terminado!"
        else do
            newState <- takeTurn state
            gameLoop newState
