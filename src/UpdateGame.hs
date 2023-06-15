module UpdateGame where 

import Graphics.Gloss 
import System.Random 
import Data.List (delete, sortBy)
import System.Exit ( exitSuccess ) 
import FileOperations 

data Menu = StartMenu | NickMenu | InstructionMenu | StartBoardMenu | BoardMenu | GameMenu | EndMenu 
  deriving (Eq,Show)

data Player = Player 
  { playerPos :: Point,
    playerVel :: Vector, 
    playerAlive :: Bool,
    playerName :: String, 
    playerPoints :: Int, 
    playerHighscore :: Int, 
    playerScoreSaved :: Bool 
  } deriving (Eq, Show)

data Bullet = Bullet
  { bulletPos :: Point,
    bulletVel :: Vector,
    bulletUsed :: Bool 
  } deriving (Eq, Show)

data Enemy = Enemy 
  { enemyPos :: Point,
    enemyAlive :: Bool
  }
  deriving (Eq, Show)

data EnemyDirection = LeftDir | RightDir | DownDirAfterRight | DownDirAfterLeft 
  deriving (Eq, Show)

data GameState = Game 
  { 
    menu :: Menu,
    player :: Player,
    playerBullets :: [Bullet],
    generalHighscore :: Int,
    bestScores :: [(String, Int)],
    fileScores :: String,
    enemies :: [Enemy],
    newEnemiesTimer :: Int,
    enemyDirection :: EnemyDirection,
    generator :: StdGen,
    enemiesBullets :: [Bullet],
    shootTimer :: Int,
    countOfRound :: Float,
    exit :: Bool
  } deriving (Eq,Show)

seriesOfEnemies :: [Enemy]
seriesOfEnemies = concatMap createRow [2 .. 5]
  where
    createRow y = take 10 [Enemy (fromIntegral x, fromIntegral y * 50) True | x <- [-500 .. 500], x `mod` 60 == 0]

initialState :: StdGen -> GameState
initialState gen = Game
  { 
    menu = StartMenu, 
    player = Player (0, -250) (0,0) True "" 0 0 False,
    playerBullets = [],
    generalHighscore = 0,
    bestScores = [],
    fileScores = "scores.txt",
    enemies = seriesOfEnemies,
    newEnemiesTimer = 0,
    enemyDirection = RightDir,
    generator = gen,
    enemiesBullets = [],
    shootTimer = 0,
    countOfRound = 1,
    exit = False
  }

playerShoot :: GameState -> GameState
playerShoot game = game {playerBullets = newBullet : playerBullets game}
    where 
        newBullet = Bullet {bulletPos = playerPos (player game), bulletVel = (0,3), bulletUsed = False}

replayGame :: GameState -> GameState
replayGame game =  game {
    menu = GameMenu, 
    player = Player (0, -250) (0,0) True (playerName (player game)) 0 (playerHighscore (player game)) False,  
    playerBullets = [],
    enemies = seriesOfEnemies,
    newEnemiesTimer = 0,
    enemyDirection = RightDir,
    enemiesBullets = [],
    shootTimer = 0,
    countOfRound = 1,
    exit = False
    }

addLetter :: String -> Player -> Player 
addLetter string player = player {playerName = (playerName player) ++ string}

deleteLastLetter :: Player -> Player 
deleteLastLetter player | playerName player /= "" = player {playerName = init (playerName player)} | otherwise = player 

stopPlayerMoving :: Player -> Player
stopPlayerMoving player = player {playerVel = (0, 0)}

changePlayerVelocityRight :: Player -> Player
changePlayerVelocityRight player 
    | fst (playerPos player) >= 475 = stopPlayerMoving player 
    | otherwise = player {playerVel = (4, 0)}

changePlayerVelocityLeft :: Player -> Player
changePlayerVelocityLeft player 
    | fst (playerPos player) <= -475 = stopPlayerMoving player 
    | otherwise = player {playerVel = (-4, 0)}

sortScores :: [(String, Int)] -> [(String, Int)]
sortScores = sortBy compareSecond
  where compareSecond (_, x) (_, y)
          | x < y = GT
          | x > y = LT
          | otherwise = EQ

loadTopTen :: GameState -> IO GameState
loadTopTen game = do 
    topScores <- loadHighScores (fileScores game) 
    let topTen = take (min 10 (length topScores)) $ sortScores topScores 
    return game {bestScores = topTen}

changePlayerScoreSaved :: Player -> Player 
changePlayerScoreSaved player = player {playerScoreSaved = True}

changePlayerHighscore :: Int -> Player -> Player
changePlayerHighscore points player = player {playerHighscore = points}

saveAndLoadScores :: GameState -> IO GameState
saveAndLoadScores game = do
    saveScore (fileScores game) (playerName (player game)) (playerPoints (player game)) 
    maxScore <- loadHighScore (fileScores game) 
    maxUserScore <- loadPlayerHighScore (playerName (player game)) (fileScores game) 
    topScores <- loadHighScores (fileScores game) 
    let topTen = take (min 10 (length topScores)) $ sortScores topScores 
    let newPlayer = changePlayerScoreSaved (player game)
    return game {player = changePlayerHighscore maxUserScore newPlayer, generalHighscore = maxScore, bestScores = topTen}

enemyTooFar :: Enemy -> Bool 
enemyTooFar enemy = snd (enemyPos enemy) <= -240 

checkEndGame :: GameState -> GameState
checkEndGame game 
    | (playerAlive (player game)) == False || any enemyTooFar (enemies game) = game {menu = EndMenu} 
    | otherwise = game 

changePlayerAlive :: Player -> Player
changePlayerAlive player = player {playerAlive = False}

bulletKillPlayer :: Player -> Bullet -> Bool 
bulletKillPlayer player bullet = fst (bulletPos bullet) <= (posX + 35) && fst (bulletPos bullet) >= posX - 35 && snd (bulletPos bullet) <= (posY + 10) && snd (bulletPos bullet) >= posY - 10
    where
        posX = fst $ playerPos player 
        posY = snd $ playerPos player 

enemiesKillPlayer :: GameState -> GameState
enemiesKillPlayer game 
    | any (bulletKillPlayer (player game)) (enemiesBullets game) = game {player = changePlayerAlive (player game)} 
    | otherwise = game 

changeEnemyAlive :: Enemy -> Enemy
changeEnemyAlive enemy = enemy {enemyAlive = False}

bulletKillEnemy :: Enemy -> Bullet -> Bool
bulletKillEnemy enemy bullet = fst (bulletPos bullet) <= (posX + 20) && fst (bulletPos bullet) >= (posX - 20) && snd (bulletPos bullet) <= (posY + 15) && snd (bulletPos bullet) >= (posY - 15)
    where 
        posX = fst $ enemyPos enemy
        posY = snd $ enemyPos enemy

bulletsKillEnemy :: [Bullet] -> Enemy -> Bool
bulletsKillEnemy bullets enemy = any (bulletKillEnemy enemy) bullets

bulletsKillEnemies :: [Bullet] -> [Enemy] -> [Enemy]
bulletsKillEnemies bullets enemies = fmap (\x -> if bulletsKillEnemy bullets x then changeEnemyAlive x else x) enemies

usedBullet :: [Enemy] -> Bullet -> Bullet
usedBullet enemies bullet 
    | any (\x -> bulletKillEnemy x bullet) enemies = bullet {bulletUsed = True} 
    | otherwise = bullet 

usedPlayerBullets :: [Enemy] -> [Bullet] -> [Bullet]
usedPlayerBullets enemies bullets = fmap (usedBullet enemies) bullets

removeUsedBullets :: [Bullet] -> [Bullet]
removeUsedBullets bullets = filter (\x -> not (bulletUsed x)) bullets

addPoints :: Int -> Player -> Player
addPoints points player = player {playerPoints = (playerPoints player) + points} 

playerKillEnemies :: GameState -> GameState 
playerKillEnemies game 
    | any (bulletsKillEnemy (playerBullets game)) (enemies game) = game {
        enemies = filter enemyAlive newEnemies, 
        playerBullets = removeUsedBullets usedBullets, 
        player = addPoints points (player game) } 
    | otherwise = game 
    where 
        usedBullets = usedPlayerBullets (enemies game) (playerBullets game)
        newEnemies = bulletsKillEnemies (playerBullets game) (enemies game)
        killedEnemies = filter (\x -> not (enemyAlive x)) newEnemies 
        points = length killedEnemies

playerOutOfScope :: Player -> Player 
playerOutOfScope player 
    | fst (playerPos player) < -475 = player {playerPos = (-475, -250), playerVel = (0, 0)} 
    | fst (playerPos player) > 475 = player {playerPos = (475, -250), playerVel = (0, 0)} 
    | otherwise = player 

updatePlayerPosition :: Player -> Player
updatePlayerPosition player 
    | (fst (playerPos player) <= -475  && fst (playerVel player) == -4 ) || (fst (playerPos player) >= 475  && fst (playerVel player) == 4) = playerOutOfScope player 
    | otherwise =  player { playerPos = (posX + velX, posY)}
    where 
        (velX, _) = playerVel player 
        (posX, posY) = playerPos player

movePlayer :: GameState -> GameState
movePlayer game = game {player = updatePlayerPosition (player game)} 

updateBulletPosition :: Bullet -> Bullet
updateBulletPosition bullet = bullet { bulletPos = (posX, posY + velY)} 
    where 
        (_, velY) = bulletVel bullet 
        (posX, posY) = bulletPos bullet 

bulletOutOfScope :: Bullet -> Bool
bulletOutOfScope bullet = snd (bulletPos bullet) > 300 || snd (bulletPos bullet) < -300 

removeBulletsOutOfScope :: [Bullet] -> [Bullet]
removeBulletsOutOfScope bullets = filter (\bullet -> not (bulletOutOfScope bullet)) bullets

updateBulletsPosition :: [Bullet] -> [Bullet]
updateBulletsPosition bullets = removeBulletsOutOfScope updatedBullets 
    where 
        updatedBullets = map updateBulletPosition bullets

movePlayerBullets :: GameState -> GameState
movePlayerBullets game = game {playerBullets = updateBulletsPosition (playerBullets game)} 

moveEnemiesBullets :: GameState -> GameState 
moveEnemiesBullets game = game {enemiesBullets = updateBulletsPosition (enemiesBullets game)} 

selectRandomEnemies :: StdGen -> [Enemy] -> Int -> ([Enemy], StdGen)
selectRandomEnemies gen _ 0 = ([], gen)
selectRandomEnemies gen enemies count = ((enemy : rest), genNew2)
  where 
    (randomIndex, genNew) = randomR (0, length enemies - 1) gen :: (Int, StdGen)
    enemy = enemies !! randomIndex
    remainingEnemies = delete enemy enemies
    (rest, genNew2) = selectRandomEnemies genNew remainingEnemies (count - 1)

addEnemiesBullets :: Float -> [Enemy] -> [Bullet]
addEnemiesBullets count enemies = fmap (\x -> Bullet {bulletPos = (enemyPos x), bulletVel = (0,((-1.5) - count*(0.5))), bulletUsed = False}) enemies 

enemiesShoot :: GameState -> GameState
enemiesShoot game 
    | shootTimer game == 0 = game { enemiesBullets = newBullets, shootTimer = shootTimer game + 1, generator = gen}
    | shootTimer game >= 1 && shootTimer game <= 40 = game {shootTimer = shootTimer game + 1}
    | otherwise = game {shootTimer = shootTimer game - 41}
    where
        numberOfEnemies = min 4 (length (enemies game))
        (selectedEnemies, gen) = selectRandomEnemies (generator game) (enemies game) numberOfEnemies
        newBullets = enemiesBullets game ++ addEnemiesBullets (countOfRound game) selectedEnemies

moveEnemyRight :: Float -> Enemy -> Enemy
moveEnemyRight deltaX enemy | deltaX > 0 = enemy {enemyPos = (posX + deltaX, posY)} | otherwise = enemy 
    where
        (posX, posY) = enemyPos enemy

moveEnemyLeft :: Float -> Enemy -> Enemy
moveEnemyLeft deltaX enemy | deltaX > 0 = enemy {enemyPos = (posX - deltaX, posY)} | otherwise = enemy 
    where
        (posX, posY) = enemyPos enemy

moveEnemyDown :: Enemy -> Enemy
moveEnemyDown enemy = enemy {enemyPos = (posX, posY - 5)} 
    where
        (posX, posY) = enemyPos enemy

updateEnemyPosition :: Float -> EnemyDirection -> Enemy -> Enemy
updateEnemyPosition deltaX currentDir enemy 
    | currentDir == RightDir = moveEnemyRight deltaX enemy
    | currentDir == LeftDir = moveEnemyLeft deltaX enemy
    | currentDir == DownDirAfterRight = moveEnemyDown enemy
    | currentDir == DownDirAfterLeft = moveEnemyDown enemy

updateEnemiesPosition :: Float -> EnemyDirection -> [Enemy] -> [Enemy]
updateEnemiesPosition deltaX currentDir enemies
    | deltaX <= 10 = map (updateEnemyPosition deltaX currentDir) enemies
    | otherwise = map (updateEnemyPosition 10 currentDir) enemies

getNextDirection :: EnemyDirection -> EnemyDirection
getNextDirection RightDir = DownDirAfterRight
getNextDirection LeftDir = DownDirAfterLeft
getNextDirection DownDirAfterRight = LeftDir
getNextDirection DownDirAfterLeft = RightDir

shouldChangeDirection :: EnemyDirection -> [Enemy] -> Bool
shouldChangeDirection currentDir enemies
    | currentDir == RightDir = any (\enemy -> fst (enemyPos enemy) >= 480) enemies 
    | currentDir == LeftDir = any (\enemy -> fst (enemyPos enemy) <= -480) enemies 
    | otherwise = True

nextDirection :: EnemyDirection -> [Enemy] -> EnemyDirection
nextDirection currentDir enemies 
    | shouldChangeDirection currentDir enemies = getNextDirection currentDir
    | otherwise = currentDir

moveEnemies :: GameState -> GameState
moveEnemies game = game { 
    enemies = updateEnemiesPosition (countOfRound game) (enemyDirection game) (enemies game),
    enemyDirection = nextDirection (enemyDirection game) (enemies game) } 

generateNewEnemies :: GameState -> GameState 
generateNewEnemies game 
    | (enemies game) /= [] = game 
    | newEnemiesTimer game < 200 = game { newEnemiesTimer = (newEnemiesTimer game) + 1} 
    | otherwise = game {enemies = seriesOfEnemies, newEnemiesTimer = 0, countOfRound = (countOfRound game) + 1}

update :: Float -> GameState -> IO GameState
update _ game
  | exit game = exitSuccess 
  | menu game  == StartBoardMenu = loadTopTen game 
  | menu game  == EndMenu && (not (playerScoreSaved (player game))) = saveAndLoadScores game 
  | menu game  == EndMenu  && (playerScoreSaved (player game)) = return game
  | menu game  == GameMenu = return $ (checkEndGame . enemiesKillPlayer . playerKillEnemies .  movePlayer . movePlayerBullets . moveEnemiesBullets . enemiesShoot . moveEnemies . generateNewEnemies) game 
  | otherwise = return game 