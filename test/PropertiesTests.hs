module PropertiesTests where 

import UpdateGame
import System.Random 
import Test.QuickCheck

instance Arbitrary Bullet where
  arbitrary = do
    posX <- fromIntegral <$> chooseInt (-500, 500)
    posY <- fromIntegral <$> chooseInt (-300, 300)
    let velX = 0
    velY <- elements [-2, -2.5, -3, -3.5, -4, -4.5, -5, -5.5, -6, -6.5, 0, 3]
    used <- arbitrary
    return (Bullet (posX, posY) (velX, velY) used)

instance Arbitrary Player where
  arbitrary = do
    posX <- fromIntegral <$> chooseInt (-475, 475)
    let posY = -250
    velX <- elements [-4, 0, 4]
    let velY = 0
    let alive = True 
    name <- arbitrary
    points <- choose (0, 1000)
    highscore <- choose (0, 2000)
    scoreSaved <- arbitrary
    return (Player (posX, posY) (velX, velY) alive name points highscore scoreSaved)

instance Arbitrary Enemy where 
  arbitrary = do
    posX <- fromIntegral <$> chooseInt (-480, 480)
    posY <- fromIntegral <$> chooseInt (-250,280)
    let alive = True 
    return (Enemy (posX,posY) alive)

instance Arbitrary EnemyDirection where
  arbitrary = elements [LeftDir, RightDir, DownDirAfterRight, DownDirAfterLeft]

instance Arbitrary GameState where
  arbitrary = do
    let menu = GameMenu
    player <- arbitrary
    playerBullets <- arbitrary
    generalHighscore <- arbitrary
    bestScores <- arbitrary
    let fileScores = "scores.txt"
    enemies <- arbitrary
    newEnemiesTimer <- chooseInt (0, 200)
    enemyDirection <- arbitrary
    let generator = mkStdGen 0
    enemiesBullets <- arbitrary
    shootTimer <- chooseInt (0, 41)
    countOfRound <- fromIntegral <$> chooseInt (0, 200)
    let exit = False
    return (Game menu player playerBullets generalHighscore bestScores fileScores enemies newEnemiesTimer enemyDirection generator enemiesBullets shootTimer countOfRound exit)

testPlayerShoot :: GameState -> Bool 
testPlayerShoot game = length (playerBullets updatedGame) == length initialBullets + 1
    && bulletPos newBullet == playerPos (player game)
    && bulletVel newBullet == (0, 3)
    && not (bulletUsed newBullet)
    && all (`elem` playerBullets updatedGame) initialBullets 
    where 
        initialBullets = playerBullets game
        updatedGame = playerShoot game
        newBullet = head (playerBullets updatedGame)

testBulletsKillEnemies :: [Bullet] -> [Enemy] -> Bool
testBulletsKillEnemies bullets enemies =  length (bulletsKillEnemies bullets enemies) == length enemies 
    && all (\enemy -> not (bulletsKillEnemy bullets enemy) ) (filter enemyAlive (bulletsKillEnemies bullets enemies))
    && all (\enemy -> bulletsKillEnemy bullets enemy) (filter (\enemy -> not (enemyAlive enemy)) (bulletsKillEnemies bullets enemies))

testRemoveUsedBullets :: [Bullet] -> Bool
testRemoveUsedBullets bullets = all (\bullet -> not (bulletUsed bullet)) filteredBullets && all (\bullet -> bullet `elem` filteredBullets) (filter (not . bulletUsed) bullets)
    where
        filteredBullets = removeUsedBullets bullets 

testPlayerKillEnemies :: GameState -> Bool
testPlayerKillEnemies game = enemiesAfter + killedEnemies == enemiesBefore && bulletsAfter + usedBullets ==  bulletsBefore && pointsAfter == pointsBefore + killedEnemies
  where 
    startGame = game {playerBullets = removeUsedBullets (playerBullets game)}
    newGame = playerKillEnemies startGame
    enemiesBefore = length (enemies startGame)
    enemiesAfter = length (enemies newGame)
    killedEnemies = length (filter (not . enemyAlive) (bulletsKillEnemies (playerBullets startGame) (enemies startGame)))
    bulletsBefore = length (playerBullets startGame)
    bulletsAfter = length (playerBullets newGame)
    usedBullets = length (filter bulletUsed (usedPlayerBullets (enemies startGame) (playerBullets startGame)))
    pointsBefore = playerPoints (player startGame)
    pointsAfter = playerPoints (player newGame)

testUpdatePlayerPosition :: Player -> Bool
testUpdatePlayerPosition player = posX >= -478 && posX <= 478 && posY == -250 
    where 
        updatedPlayer = updatePlayerPosition player
        (posX, posY) = playerPos updatedPlayer

testBulletOutOfScope :: Bullet -> Bool
testBulletOutOfScope bullet = (snd (bulletPos newBullet) > 300 || snd (bulletPos newBullet) < -300 || fst (bulletPos newBullet) > 500 || fst (bulletPos newBullet) < -500) == bulletOutOfScope newBullet
    where 
        (posX, posY) = bulletPos newBullet
        newBullet = updateBulletPosition bullet 

testRemoveBulletsOutOfScope :: [Bullet] -> Bool
testRemoveBulletsOutOfScope bullets = all (\bullet -> not (bulletOutOfScope bullet)) filteredBullets && all (\bullet -> bullet `elem` filteredBullets) newBullets
    where 
        filteredBullets = removeBulletsOutOfScope bullets
        outOfScopeBullets = filter bulletOutOfScope bullets
        newBullets = filter (\bullet -> bullet `notElem` outOfScopeBullets) bullets

testUpdateBulletsPosition :: [Bullet] -> Bool
testUpdateBulletsPosition bullets = updatedBullets == expectedBullets 
    where  
        updatedBullets = updateBulletsPosition bullets
        expectedBullets = filter (\bullet -> (snd (bulletPos bullet) <= 300 && snd (bulletPos bullet) >= -300 && fst (bulletPos bullet) <= 500 && fst (bulletPos bullet) >= -500)) (map updateBulletPosition bullets)

testSelectRandomEnemies :: [Enemy] -> Bool
testSelectRandomEnemies enemies = selectedCount == count && all (`elem` enemies) selected 
    where
        count = min 4 (length enemies) 
        (selected, _) = selectRandomEnemies (mkStdGen 0) enemies count
        selectedCount = length selected

testEnemiesShoot :: GameState -> Bool
testEnemiesShoot game = all (\bullet -> bullet `elem` newBullets) oldBullets 
  && (if oldTimer == 41 then newTimer == 0 else newTimer == oldTimer + 1) 
  && (if oldTimer == 0 then length newBullets == (length oldBullets) + count else length newBullets == length oldBullets)
  where 
    result = enemiesShoot game
    newBullets = enemiesBullets result
    oldBullets = enemiesBullets game 
    newTimer = shootTimer result
    oldTimer = shootTimer game 
    count = min 4 (length (enemies game))

testGenerateNewEnemies :: GameState -> Bool 
testGenerateNewEnemies game 
  | enemies game /= [] = enemies game == enemies newGame
  | newEnemiesTimer game < 200 = newTimer == oldTimer + 1 && enemies game == enemies newGame 
  | otherwise = enemies newGame == seriesOfEnemies && newRound == oldRound + 1 && newTimer == 0 
    where
      newGame = generateNewEnemies game
      newTimer = newEnemiesTimer newGame
      oldTimer = newEnemiesTimer game 
      newRound = countOfRound newGame
      oldRound = countOfRound game 