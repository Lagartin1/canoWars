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

topMargin :: Int -- Margen superior antes de que comience la pared
topMargin = 10

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

simulateProjectile :: Projectile -> GameState -> IO GameState
simulateProjectile proj game = do
    let (x, y) = projPosition proj
        vx = projVx proj
        vy = projVy proj - gravity
        nextX = x + round vx
        nextY = y - round vy
        newProj = proj { projPosition = (nextX, nextY), projVy = vy }

    if nextY >= screenHeight - 1 then
        -- Caso en el que el proyectil alcanza el suelo
        return $ game { currentTurn = not (currentTurn game), player1 = (player1 game) { fuel = 100 }, player2 = (player2 game) { fuel = 100 } }

    else if nextX == wallPosition && nextY >= 10 then
        -- Caso en el que el proyectil impacta directamente la muralla
        return $ game { currentTurn = not (currentTurn game), player1 = (player1 game) { fuel = 100 }, player2 = (player2 game) { fuel = 100 } }

    else if checkWallPass newProj game then
        -- Caso en el que el proyectil traspasa la muralla debajo de su altura sin impacto
        return $ game { currentTurn = not (currentTurn game), player1 = (player1 game) { fuel = 100 }, player2 = (player2 game) { fuel = 100 } }

-- Verificar si el proyectil ha golpeado al enemigo
    else if checkHitbox newProj (if currentTurn game then player2 game else player1 game) then
        -- Si se detecta una colisión, aplicar daño al jugador enemigo
        calculateDamage >>= \damage -> do  -- Usamos >>= para extraer el valor de IO Int
            let updatedGame = updateGameAfterHit newProj game damage
            return updatedGame
    else do
        -- Actualización de pantalla para mostrar la posición del proyectil
        clearScreen
        drawGame game
        setCursorPosition nextY nextX
        putChar '*'
        hFlush stdout
        threadDelay 100000
        simulateProjectile newProj game

-- Función que calcula el daño
calculateDamage :: IO Int
calculateDamage = do
    chance <- randomRIO (1, 100) :: IO Int  -- Genera un número entre 1 y 100
    if chance <= 5 then randomRIO (7, 9) :: IO Int else randomRIO (1, 3) :: IO Int



-- Verifica si el proyectil pasa de un lado de la muralla al otro sin impactarla, estando por debajo de su altura.
checkWallPass :: Projectile -> GameState -> Bool
checkWallPass proj game =
    let (px, py) = projPosition proj
        prevX = px - round (projVx proj)  -- La posición x previa del proyectil
    in py >= topMargin && ((prevX < wallPosition && px > wallPosition) || (prevX > wallPosition && px < wallPosition))


-- Comprueba si el proyectil impacta contra la catapulta del enemigo
checkHitEnemy :: Projectile -> GameState -> Bool
checkHitEnemy proj game =
    let (px, py) = projPosition proj
        targetPlayer = if currentTurn game then player2 game else player1 game
        (tx, ty) = position targetPlayer
        catapultShape = drawCatapult targetPlayer
        targetArea = [(tx + dx, ty + dy) | (dy, line) <- zip [0..] catapultShape, dx <- [0..length line - 1], line !! dx /= ' ']
    in (px, py) `elem` targetArea

-- Verifica si el proyectil está dentro del hitbox del jugador
checkHitbox :: Projectile -> Player -> Bool
checkHitbox proj player =
    let (px, py) = projPosition proj
        (tx, ty) = position player
        hitboxWidth = 8  -- El tamaño del hitbox en el eje X (puedes ajustarlo)
        hitboxHeight = 3  -- El tamaño del hitbox en el eje Y (puedes ajustarlo)
        -- Verifica si el proyectil está dentro del rango del hitbox del jugador
    in px >= (tx - hitboxWidth) && px <= (tx + hitboxWidth) &&
        py >= (ty - hitboxHeight) && py <= (ty + hitboxHeight)

-- Actualiza el estado del juego después de un impacto
updateGameAfterHit :: Projectile -> GameState -> Int -> GameState
updateGameAfterHit proj game damage =
    let targetPlayer = if currentTurn game then player2 game else player1 game
        newHp = max 0 (hp targetPlayer - damage)  -- Reducir la vida del jugador, asegurándose de que no sea negativa
        updatedTarget = targetPlayer { hp = newHp }
        newGame = if currentTurn game
            then game { player2 = updatedTarget }
            else game { player1 = updatedTarget }
    in newGame { currentTurn = not (currentTurn game), gameOver = newHp == 0 }

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
                wallHeight = screenHeight - topMargin-- Altura total de la pared
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

-- Main game loop