module Utils where
import System.Random
import Control.Monad
import System.IO
import System.Console.ANSI
import Control.Concurrent  -- Importa Control.Concurrent para usar threadDelay

-- Tipos de datos
data Player = Player {
    hp :: Int,
    fuel :: Int,
    position :: (Int, Int),  -- (x, y)
    angle :: Int,            -- ángulo en grados
    isPlayer1 :: Bool        -- True para P1, False para P2
} deriving (Show)

data GameState = GameState {
    player1 :: Player,
    player2 :: Player,
    currentTurn :: Bool,     -- True para P1, False para P2
    gameOver :: Bool
} deriving (Show)

-- Tipo de dato Projectile
data Projectile = Projectile {
    projPosition :: (Int, Int),
    projVx :: Float,           -- Velocidad en x
    projVy :: Float            -- Velocidad en y
} deriving (Show)


-- Constantes del juego
screenWidth :: Int
screenWidth = 80

screenHeight :: Int
screenHeight = 25

wallPosition :: Int
wallPosition = screenWidth `div` 2

-- Inicialización del juego
initialGameState :: GameState
initialGameState = GameState {
    player1 = Player {
        hp = 30,
        fuel = 100,
        position = (10, screenHeight - 3),
        angle = 45,
        isPlayer1 = True
    },
    player2 = Player {
        hp = 30,
        fuel = 100,
        position = (screenWidth - 10, screenHeight - 3),
        angle = 135,
        isPlayer1 = False
    },
    currentTurn = True,
    gameOver = False
}


-- Inicializa el proyectil con velocidad calculada según el ángulo del jugador
initializeProjectile :: Player -> Projectile
initializeProjectile player =
    let (x, y) = position player
        radAngle = fromIntegral (angle player) * pi / 180  -- Ángulo en radianes
        vx = 5.0 * cos radAngle
        vy = 5.0 * sin radAngle
    in Projectile {
        projPosition = (x, y - 2),
        projVx = vx,
        projVy = vy
    }

-- Constante de gravedad
gravity :: Float
gravity = 0.5  -- Ajusta este valor para cambiar la "fuerza" de la gravedad

-- Simula el movimiento del proyectil, aplicando gravedad a `vy`
simulateProjectile :: Projectile -> GameState -> IO GameState
simulateProjectile proj game = do
    let (x, y) = projPosition proj
        vx = projVx proj
        vy = projVy proj - gravity          -- Aplica gravedad solo en la velocidad vertical
        nextX = x + round vx
        nextY = y - round vy
        newProj = proj { projPosition = (nextX, nextY), projVy = vy }

    if nextY >= screenHeight - 1 then
        -- Caso en el que el proyectil alcanza el suelo
        return $ game { currentTurn = not (currentTurn game) }

    else if nextX == wallPosition then
        -- Caso en el que el proyectil choca contra la pared
        return $ game { currentTurn = not (currentTurn game) }

    else if checkHitEnemy newProj game then
        -- Caso en el que el proyectil impacta al jugador enemigo
        let damage = 5  -- Daño fijo o calculado
            updatedGame = updateGameAfterShot game damage
        in return updatedGame

    else do
        -- Actualización de pantalla para mostrar la posición del proyectil
        clearScreen
        drawGame game
        setCursorPosition nextY nextX
        putChar '*'
        hFlush stdout
        threadDelay 100000
        simulateProjectile newProj game


-- Comprueba si el proyectil impacta contra la catapulta del enemigo
checkHitEnemy :: Projectile -> GameState -> Bool
checkHitEnemy proj game =
    let (px, py) = projPosition proj
        targetPlayer = if currentTurn game then player2 game else player1 game
        (tx, ty) = position targetPlayer
        catapultShape = drawCatapult targetPlayer
        targetArea = [(tx + dx, ty + dy) | (dy, line) <- zip [0..] catapultShape, dx <- [0..length line - 1], line !! dx /= ' ']
    in (px, py) `elem` targetArea


-- Dibuja el escenario completo
drawGame :: GameState -> IO ()
drawGame game = do
    clearScreen
    setCursorPosition 0 0
    let screen = replicate screenHeight (replicate screenWidth ' ')
    let withWall = addWall screen
    let withPlayers = addPlayers withWall game
    let withInfo = addGameInfo withPlayers game
    putStr $ unlines withInfo
    where
        addWall screen = 
            let
                topMargin = 10   -- Margen superior antes de que comience la pared
                bottomMargin = 0-- Margen inferior después de que termine la pared
                wallHeight = screenHeight - topMargin - bottomMargin -- Altura total de la pared
            in
                [ if i >= topMargin && i < topMargin + wallHeight
                  then insertAt wallPosition '|' row
                  else row
                  | (i, row) <- zip [0..] screen]
        insertAt n x xs = take n xs ++ [x] ++ drop (n + 1) xs

addGameInfo :: [String] -> GameState -> [String]
addGameInfo screen game =
    let p1Info = "P1: HP=" ++ show (hp $ player1 game) ++ " Fuel=" ++ show (fuel $ player1 game) ++ " Angle=" ++ show (angle $ player1 game)
        p2Info = "P2: HP=" ++ show (hp $ player2 game) ++ " Fuel=" ++ show (fuel $ player2 game) ++ " Angle=" ++ show (angle $ player2 game)
        turnInfo = "Turno: " ++ (if currentTurn game then "P1" else "P2")
    in screen ++ [p1Info, p2Info, turnInfo]


-- Función para dibujar la catapulta con su diseño ASCII multilínea
drawCatapult :: Player -> [String]
drawCatapult player =
    let (x, y) = position player
        isP1 = isPlayer1 player
        angle = if isP1 then '/' else '\\'
        base1 = "|#----#|"
        base2 = "@@@   @@"
        top = "    "++[angle]
    in [top,base1,base2]

-- Función para agregar un jugador (catapulta) al screen
addPlayer :: [String] -> Player -> Int -> Int -> [String]
addPlayer screen player x y =
    let catapultLines = drawCatapult player
        modifyRow row line = take x row ++ line ++ drop (x + length line) row
        -- Asegura que cada línea de catapultLines se aplique a la fila correcta de screen
        indexedLines = zip (take (length catapultLines) (drop y screen)) catapultLines
        modifiedRows = map (uncurry modifyRow) indexedLines
    in take y screen ++ modifiedRows ++ drop (y + length catapultLines) screen

-- Función para agregar ambos jugadores al screen
addPlayers :: [String] -> GameState -> [String]
addPlayers screen game =
    let p1 = player1 game
        p2 = player2 game
        (x1, y1) = position p1
        (x2, y2) = position p2
        screenWithP1 = addPlayer screen p1 x1 y1
    in addPlayer screenWithP1 p2 x2 y2

-- Modifica una lista en un índice específico usando una función de modificación.
modifyAt :: Int -> (a -> a) -> [a] -> [a]
modifyAt idx f xs =
    let (left, right) = splitAt idx xs
    in left ++ [f (head right)] ++ tail right

-- Inserta un string en una posición específica dentro de otro string.
insertAt :: Int -> Char -> String -> String
insertAt idx ch str =
    let (left, right) = splitAt idx str
    in left ++ [ch] ++ right


-- Manejo de input
handleInput :: Char -> GameState -> IO GameState
handleInput input game
    | gameOver game = return game
    | otherwise = case input of
        'a' -> moveCatapult (-1)
        'd' -> moveCatapult 1
        'w' -> adjustAngle 5
        's' -> adjustAngle (-5)
        ' ' -> do
            let currentPlayer = if currentTurn game then player1 game else player2 game
                proj = initializeProjectile currentPlayer
            simulateProjectile proj game
        'q' -> return game { gameOver = True }
        _ -> return game
    where
        currentPlayer = if currentTurn game then player1 game else player2 game
        moveCatapult dx
            | fuel currentPlayer < 10 = return game
            | otherwise = return $ updateCurrentPlayer game $
                currentPlayer { position = (x + dx, y), fuel = fuel currentPlayer - 10 }
            where (x, y) = position currentPlayer
        
        adjustAngle da
            | fuel currentPlayer < 2 = return game
            | otherwise = return $ updateCurrentPlayer game $
                currentPlayer { angle = max 0 (min 180 (angle currentPlayer + da)),
                            fuel = fuel currentPlayer - 5 }


updateCurrentPlayer :: GameState -> Player -> GameState
updateCurrentPlayer game newPlayer =
    if currentTurn game
        then game { player1 = newPlayer }
        else game { player2 = newPlayer }

updateGameAfterShot :: GameState -> Int -> GameState
updateGameAfterShot game damage =
    let targetPlayer = if currentTurn game then player2 game else player1 game
        newHp = max 0 (hp targetPlayer - damage)
        updatedTarget = targetPlayer { hp = newHp }
        newGame = if currentTurn game
            then game { player2 = updatedTarget }
            else game { player1 = updatedTarget }
    in newGame { currentTurn = not (currentTurn game),
                 gameOver = newHp <= 0,
                 player1 = (player1 newGame) { fuel = 100 },
                 player2 = (player2 newGame) { fuel = 100 } }

-- Main game loop