-- Utils.hs
module Utils (
    GameState,
    initializeGame,
    takeTurn,
    displayGameState,
    checkGameOver
) where

import System.Random (randomRIO)

data GameState = GameState {
    player1 :: Player,
    player2 :: Player,
    turn :: Int
}

data Player = Player {
    name :: String,
    health :: Int,
    fuel :: Int,
    angle :: Int
}

initializeGame :: IO GameState
initializeGame = do
    let player1 = Player "Jugador 1" 30 100 45
    let player2 = Player "Jugador 2" 30 100 45
    return $ GameState player1 player2 1

takeTurn :: GameState -> IO GameState
takeTurn state = do
    let currentPlayer = if turn state == 1 then player1 state else player2 state
    putStrLn $ "Turno de " ++ name currentPlayer
    newAngle <- promptAngle (angle currentPlayer)
    dmg <- calculateDamage
    let newFuel = fuel currentPlayer - 10
    let newPlayer = currentPlayer { angle = newAngle, health = health currentPlayer - dmg, fuel = newFuel }
    let newTurn = if turn state == 1 then 2 else 1
    let newState = if turn state == 1
                   then state { player1 = newPlayer, turn = newTurn }
                   else state { player2 = newPlayer, turn = newTurn }
    putStrLn $ "Daño causado: " ++ show dmg
    return newState

displayGameState :: GameState -> IO ()
displayGameState state = do
    putStrLn "\nEstado del juego:"
    putStrLn $ displayPlayer (player1 state) ++ "  ||  " ++ displayPlayer (player2 state)
    putStrLn $ "Combustible de Jugador 1: " ++ show (fuel (player1 state))
    putStrLn $ "Combustible de Jugador 2: " ++ show (fuel (player2 state))
    putStrLn "--------------------------------------------------"

displayPlayer :: Player -> String
displayPlayer player =
    name player ++ " (HP: " ++ show (health player) ++ ", Ángulo: " ++ show (angle player) ++ ")"

checkGameOver :: GameState -> Bool
checkGameOver state = health (player1 state) <= 0 || health (player2 state) <= 0

calculateDamage :: IO Int
calculateDamage = do
    critChance <- randomRIO (1 :: Int, 100)
    baseDamage <- randomRIO (1 :: Int, 3)
    return $ if critChance <= 5 then baseDamage + 6 else baseDamage

promptAngle :: Int -> IO Int
promptAngle currentAngle = do
    putStrLn $ "Ángulo actual: " ++ show currentAngle ++ " grados."
    putStrLn "Ingrese nuevo ángulo de lanzamiento (entre 0 y 90): "
    input <- getLine
    let newAngle = read input
    return $ max 0 (min 90 newAngle)
