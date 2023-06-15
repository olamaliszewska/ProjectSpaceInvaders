module UnitTests where 

import UpdateGame
import Test.HUnit
import System.Random 
import FileOperations
import System.IO
import System.Directory

unitTestReplayGame :: Test
unitTestReplayGame = TestList
    [ 
      TestCase $ do        
        let enemy1 =  Enemy (0, -239) True 
        let enemy2 =  Enemy (0, 0) True 
        let game = Game {
              menu = EndMenu,
              player = Player (-100, -250) (-4, 0) False "John" 100 200 False,
              playerBullets = [],
              generalHighscore = 500,
              bestScores = [("Alice", 400), ("Bob", 300)],
              fileScores = "scores.txt",
              enemies = [enemy1, enemy2],
              newEnemiesTimer = 20,
              enemyDirection = LeftDir,
              generator = mkStdGen 0,
              enemiesBullets = [],
              shootTimer = 10,
              countOfRound = 4,
              exit = False
            }
        let expected = game {
              menu = GameMenu,
              player = Player (0, -250) (0, 0) True "John" 0 200 False,
              enemies = seriesOfEnemies,
              newEnemiesTimer = 0,
              enemyDirection = RightDir,
              shootTimer = 0,
              countOfRound = 1
            }
        let result = replayGame game
        assertEqual "Problem with replayGame" expected result
    ]

unitTestAddLetter :: Test
unitTestAddLetter = TestList
  [ TestCase $ do
      let player =  Player (0, -250) (0, 0) True "Joh" 0 0 False
      let newPlayer = addLetter "n" player
      let result = playerName newPlayer
      let expected = "John"
      assertEqual "Problem with addLetter" expected result 
  , TestCase $ do
      let player =  Player (0, -250) (0, 0) True "A" 15 0 False
      let newPlayer = addLetter "nna" player
      let result = playerName newPlayer
      let expected = "Anna"
      assertEqual "Problem with addLetter" expected result 
  , TestCase $ do
      let player =  Player (0, -250) (0, 0) True "Lola" 51 0 False
      let newPlayer = addLetter "" player
      let result = playerName newPlayer
      let expected = "Lola"
      assertEqual "Problem with addLetter" expected result 
 , TestCase $ do
      let player =  Player (0, -250) (0, 0) True "" 0 0 False
      let newPlayer = addLetter "O" player
      let result = playerName newPlayer
      let expected = "O"
      assertEqual "Problem with addLetter" expected result 
  ]

unitTestDeleteLastLetter :: Test
unitTestDeleteLastLetter = TestList
  [ TestCase $ do
      let player =  Player (0, -250) (0, 0) True "John" 0 0 False
      let newPlayer = deleteLastLetter player 
      let result = playerName newPlayer
      let expected = "Joh"
      assertEqual "Problem with deleteLastLetter" expected result 
  , TestCase $ do
      let player =  Player (0, -250) (0, 0) True "A" 15 0 False
      let newPlayer = deleteLastLetter player 
      let result = playerName newPlayer
      let expected = ""
      assertEqual "Problem with deleteLastLetter" expected result 
  , TestCase $ do
      let player =  Player (0, -250) (0, 0) True "Lola" 51 0 False
      let newPlayer = deleteLastLetter player 
      let result = playerName newPlayer
      let expected = "Lol"
      assertEqual "Problem with deleteLastLetter" expected result 
 , TestCase $ do
      let player =  Player (0, -250) (0, 0) True "" 0 0 False
      let newPlayer = deleteLastLetter player 
      let result = playerName newPlayer
      let expected = ""
      assertEqual "Problem with deleteLastLetter" expected result 
  ]

unitTestStopPlayerMoving :: Test
unitTestStopPlayerMoving = TestList
  [ TestCase $ do
      let player =  Player (0, -250) (0, 0) True "John" 0 0 False
      let newPlayer = stopPlayerMoving player 
      let result = playerVel newPlayer
      let expected = (0,0)
      assertEqual "Problem with stopPlayerMoving" expected result 
  , TestCase $ do
      let player =  Player (0, -250) (-4, 0) True "Ala" 15 0 False
      let newPlayer = stopPlayerMoving player 
      let result = playerVel newPlayer
      let expected = (0,0)
      assertEqual "Problem with stopPlayerMoving" expected result 
  , TestCase $ do
      let player =  Player (0, -250) (4, 0) True "Lola" 51 0 False
      let newPlayer = stopPlayerMoving player 
      let result = playerVel newPlayer
      let expected = (0,0)
      assertEqual "Problem with stopPlayerMoving" expected result 
  ]

unitTestChangePlayerVelocityRight :: Test
unitTestChangePlayerVelocityRight = TestList
  [ TestCase $ do
      let player =  Player (0, -250) (0, 0) True "John" 0 0 False
      let newPlayer = changePlayerVelocityRight player 
      let result = playerVel newPlayer
      let expected = (4,0)
      assertEqual "Problem with changePlayerVelocityRight" expected result 
  , TestCase $ do
      let player =  Player (0, -250) (-4, 0) True "John" 0 0 False
      let newPlayer = changePlayerVelocityRight player 
      let result = playerVel newPlayer
      let expected = (4,0)
      assertEqual "Problem with changePlayerVelocityRight" expected result 
  , TestCase $ do
      let player =  Player (474, -250) (4, 0) True "John" 0 0 False
      let newPlayer = changePlayerVelocityRight player 
      let result = playerVel newPlayer
      let expected = (4,0)
      assertEqual "Problem with changePlayerVelocityRight" expected result 
  , TestCase $ do
      let player =  Player (475, -250) (-4, 0) True "Ala" 15 0 False
      let newPlayer = changePlayerVelocityRight player 
      let result = playerVel newPlayer
      let expected = (0,0)
      assertEqual "Problem with changePlayerVelocityRight" expected result 
  , TestCase $ do
      let player =  Player (480, -250) (4, 0) True "Lola" 51 0 False
      let newPlayer = changePlayerVelocityRight player 
      let result = playerVel newPlayer
      let expected = (0,0)
      assertEqual "Problem with changePlayerVelocityRight" expected result 
  ]

unitTestChangePlayerVelocityLeft :: Test
unitTestChangePlayerVelocityLeft  = TestList
  [ TestCase $ do
      let player =  Player (0, -250) (0, 0) True "John" 0 0 False
      let newPlayer = changePlayerVelocityLeft player 
      let result = playerVel newPlayer
      let expected = (-4,0)
      assertEqual "Problem with changePlayerVelocityLeft" expected result 
  , TestCase $ do
      let player =  Player (0, -250) (-4, 0) True "John" 0 0 False
      let newPlayer = changePlayerVelocityLeft player 
      let result = playerVel newPlayer
      let expected = (-4,0)
      assertEqual "Problem with changePlayerVelocityLeft" expected result 
  , TestCase $ do
      let player =  Player (-474, -250) (4, 0) True "John" 0 0 False
      let newPlayer = changePlayerVelocityLeft player 
      let result = playerVel newPlayer
      let expected = (-4,0)
      assertEqual "Problem with changePlayerVelocityLeft" expected result 
  , TestCase $ do
      let player =  Player (-475, -250) (-4, 0) True "Ala" 15 0 False
      let newPlayer = changePlayerVelocityLeft player 
      let result = playerVel newPlayer
      let expected = (0,0)
      assertEqual "Problem with changePlayerVelocityLeft" expected result 
  , TestCase $ do
      let player =  Player (-480, -250) (4, 0) True "Lola" 51 0 False
      let newPlayer = changePlayerVelocityLeft player 
      let result = playerVel newPlayer
      let expected = (0,0)
      assertEqual "Problem with changePlayerVelocityLeft" expected result 
  ]

unitTestSortScores :: Test 
unitTestSortScores = TestList
   [ TestCase $ do 
        let list = [("a", 3), ("c", 2), ("b", 1)]
        let result = sortScores list
        let expected = [("a", 3), ("c", 2), ("b", 1)]
        assertEqual "Problem with sortScores" expected result,
     TestCase $ do 
        let list = [("a", 3), ("c", 1), ("b", 1)]
        let result = sortScores list
        let expected = [("a", 3), ("c", 1), ("b", 1)]
        assertEqual "Problem with sortScores" expected result,
     TestCase $ do 
        let list = [("a", 2), ("c", 3), ("b", 1)]
        let result = sortScores list
        let expected = [("c", 3), ("a", 2), ("b", 1)]
        assertEqual "Problem with sortScores" expected result]

unitTestChangePlayerScoreSaved :: Test
unitTestChangePlayerScoreSaved = TestList
    [ TestCase $ do
        let player = Player (0, -250) (0, 0) True "John" 0 0 False
        let result = changePlayerScoreSaved player
        let expected = player  { playerScoreSaved = True}
        assertEqual "Problem with changePlayerScoreSaved" expected result,
      TestCase $ do 
        let player = Player (0, -250) (0, 0) True "John" 0 0 True
        let result = changePlayerScoreSaved player
        let expected = player  
        assertEqual "Problem with changePlayerScoreSaved" expected result]

unitTestChangePlayerHighscore :: Test
unitTestChangePlayerHighscore = TestList
    [ TestCase $ do 
        let result = changePlayerHighscore 100 player
        let expected = player { playerHighscore = 100}
        assertEqual "Problem with changePlayerHighscore" expected result,
      TestCase $ do
        let result = changePlayerHighscore 0 player
        let expected = player { playerHighscore = 0}
        assertEqual "Problem with changePlayerHighscore" expected result]
  where
    player = Player (0, -250) (0, 0) True "John" 0 0 False

unitTestEnemyTooFar :: Test
unitTestEnemyTooFar = TestList
   [ TestCase $ do 
        let enemy =  Enemy (0, -239) True 
        let result = enemyTooFar enemy
        let expected = False 
        assertEqual "Problem with enemyTooFar " expected result,
     TestCase $ do 
        let enemy =  Enemy (0, -241) True 
        let result = enemyTooFar enemy
        let expected = True 
        assertEqual "Problem with enemyTooFar " expected result]

unitTestCheckEndGame :: Test
unitTestCheckEndGame = TestList
    [ 
      TestCase $ do
        let enemy1 =  Enemy (0, -239) True 
        let enemy2 =  Enemy (0, 0) True 
        let game = Game {
              menu = GameMenu,
              player = Player (-100, -250) (-4, 0) True "John" 100 200 False,
              playerBullets = [],
              generalHighscore = 500,
              bestScores = [("Alice", 400), ("Bob", 300)],
              fileScores = "scores.txt",
              enemies = [enemy1, enemy2],
              newEnemiesTimer = 20,
              enemyDirection = LeftDir,
              generator = mkStdGen 0,
              enemiesBullets = [],
              shootTimer = 10,
              countOfRound = 4,
              exit = False
            }
        let result = checkEndGame game
        let expected = game 
        assertEqual "Problem with checkEndGame" expected result,
        
      TestCase $ do
        let enemy1 =  Enemy (0, -240) True 
        let enemy2 =  Enemy (0, 0) True 
        let game = Game {
              menu = GameMenu,
              player = Player (-100, -250) (-4, 0) True "John" 100 200 False,
              playerBullets = [],
              generalHighscore = 500,
              bestScores = [("Alice", 400), ("Bob", 300)],
              fileScores = "scores.txt",
              enemies = [enemy1, enemy2],
              newEnemiesTimer = 20,
              enemyDirection = LeftDir,
              generator = mkStdGen 0,
              enemiesBullets = [],
              shootTimer = 10,
              countOfRound = 4,
              exit = False
            }
        let result = checkEndGame game
        let expected = game {menu = EndMenu}
        assertEqual "Problem with checkEndGame" expected result,
        
      TestCase $ do
        let enemy1 =  Enemy (0, -239) True 
        let enemy2 =  Enemy (0, 0) True 
        let game = Game {
              menu = GameMenu,
              player = Player (-100, -250) (-4, 0) False "John" 100 200 False,
              playerBullets = [],
              generalHighscore = 500,
              bestScores = [("Alice", 400), ("Bob", 300)],
              fileScores = "scores.txt",
              enemies = [enemy1, enemy2],
              newEnemiesTimer = 20,
              enemyDirection = LeftDir,
              generator = mkStdGen 0,
              enemiesBullets = [],
              shootTimer = 10,
              countOfRound = 4,
              exit = False
            }
        let result = checkEndGame game
        let expected = game {menu = EndMenu}
        assertEqual "Problem with checkEndGame" expected result,

        TestCase $ do
        let enemy1 =  Enemy (0, -240) True 
        let enemy2 =  Enemy (0, 0) True 
        let game = Game {
              menu = GameMenu,
              player = Player (-100, -250) (-4, 0) False "John" 100 200 False,
              playerBullets = [],
              generalHighscore = 500,
              bestScores = [("Alice", 400), ("Bob", 300)],
              fileScores = "scores.txt",
              enemies = [enemy1, enemy2],
              newEnemiesTimer = 20,
              enemyDirection = LeftDir,
              generator = mkStdGen 0,
              enemiesBullets = [],
              shootTimer = 10,
              countOfRound = 4,
              exit = False
            }
        let result = checkEndGame game
        let expected = game {menu = EndMenu}
        assertEqual "Problem with checkEndGame" expected result
    ]

unitTestChangePlayerAlive :: Test
unitTestChangePlayerAlive = TestList
    [ TestCase $ do
        let player =  Player (0, -250) (0, 0) True "John" 0 0 False
        let result = changePlayerAlive player
        let expected =  Player (0, -250) (0, 0) False "John" 0 0 False
        assertEqual "Problem with changePlayerAlive" expected result,
      TestCase $ do 
        let player =  Player (0, -250) (0, 0) False "John" 0 0 False
        let result = changePlayerAlive player
        let expected = player 
        assertEqual "Problem with changePlayerAlive" expected result]

unitTestBulletKillPlayer :: Test
unitTestBulletKillPlayer = TestList
    [ TestCase $ do
        let player = Player (0, -250) (4, 0) True "John" 10 100 False
            bullet = Bullet (0, 0) (0, -3) False
        let result = bulletKillPlayer player bullet
        let expected = False 
        assertEqual "Problem with bulletKillPlayer" expected result
    , TestCase $ do
        let player = Player (0, -250) (4, 0) True "John" 10 100 False
            bullet = Bullet (0, -250) (0, -3) False
        let result = bulletKillPlayer player bullet
        let expected = True
        assertEqual "Problem with bulletKillPlayer" expected result
    , TestCase $ do
        let player = Player (0, -250) (4, 0) True "John" 10 100 False
            bullet = Bullet (34, -260) (0, -3) False
        let result = bulletKillPlayer player bullet
        let expected = True
        assertEqual "Problem with bulletKillPlayer" expected result
    , TestCase $ do
        let player = Player (0, -250) (4, 0) True "John" 10 100 False
            bullet = Bullet (34, -270) (0, -3) False
        let result = bulletKillPlayer player bullet
        let expected = False
        assertEqual "Problem with bulletKillPlayer" expected result
    ]

unitTestEnemiesKillPlayer :: Test
unitTestEnemiesKillPlayer = TestList
    [ TestCase $ do
        let playerGame = Player (0, -250) (0, 0) True "John" 0 0 False
            bullet1 = Bullet (0, 0) (0, 0) False
            bullet2 = Bullet (100, 100) (0, 0) False
            game = Game GameMenu playerGame [] 0 [] "scores.txt" [] 0 RightDir (mkStdGen 0) [bullet1, bullet2] 0 1 False
            newGame = enemiesKillPlayer game
        let result = playerAlive (player newGame)
        let expected = True
        assertEqual "Problem with enemiesKillPlayer" expected result
    , TestCase $ do
        let playerGame = Player (0, -250) (0, 0) True "John" 0 0 False
            bullet1 = Bullet (0, -250) (0, 0) False
            bullet2 = Bullet (100, 100) (0, 0) False
            game = Game GameMenu playerGame [] 0 [] "scores.txt" [] 0 RightDir (mkStdGen 0) [bullet1, bullet2] 0 1 False
            newGame = enemiesKillPlayer game      
        let result = playerAlive (player newGame)
        let expected = False       
        assertEqual "Problem with enemiesKillPlayer" expected result
    , TestCase $ do
        let playerGame = Player (0, -250) (0, 0) True "John" 0 0 False
            bullet1 = Bullet (0, 0) (0, 0) False
            bullet2 = Bullet (34, -240) (0, 0) False
            game = Game GameMenu playerGame [] 0 [] "scores.txt" [] 0 RightDir (mkStdGen 0) [bullet1, bullet2] 0 1 False
            newGame = enemiesKillPlayer game 
        let result = playerAlive (player newGame)
        let expected = False 
        assertEqual "Problem with enemiesKillPlayer" expected result
    , TestCase $ do
        let playerGame = Player (0, -250) (0, 0) True "John" 0 0 False
            bullet1 = Bullet (-32, -260) (0, 0) False
            bullet2 = Bullet (34, -240) (0, 0) False
            game = Game GameMenu playerGame [] 0 [] "scores.txt" [] 0 RightDir (mkStdGen 0) [bullet1, bullet2] 0 1 False
            newGame = enemiesKillPlayer game 
        let result = playerAlive (player newGame)
        let expected = False 
        assertEqual "Problem with enemiesKillPlayer" expected result
    , TestCase $ do
        let playerGame = Player (0, -250) (0, 0) True "John" 0 0 False
            bullet1 = Bullet (-36, -239) (0, 0) False
            bullet2 = Bullet (36, -261) (0, 0) False
            game = Game GameMenu playerGame [] 0 [] "scores.txt" [] 0 RightDir (mkStdGen 0) [bullet1, bullet2] 0 1 False
            newGame = enemiesKillPlayer game 
        let result = playerAlive (player newGame)
        let expected = True
        assertEqual "Problem with enemiesKillPlayer" expected result
    , TestCase $ do
        let playerGame = Player (0, -250) (0, 0) True "John" 0 0 False
            game = Game GameMenu playerGame [] 0 [] "scores.txt" [] 0 RightDir (mkStdGen 0) [] 0 1 False
            newGame = enemiesKillPlayer game 
        let result = playerAlive (player newGame)
        let expected = True
        assertEqual "Problem with enemiesKillPlayer" expected result
    ]

unitTestChangeEnemyAlive :: Test
unitTestChangeEnemyAlive = TestList
    [ TestCase $ do
        let enemy =  Enemy (0, 0) True 
        let result = changeEnemyAlive enemy
        let expected = enemy { enemyAlive = False}
        assertEqual "Problem with changeEnemyAlive" expected result,
      TestCase $ do 
        let enemy =  Enemy (0, 0) False
        let result = changeEnemyAlive enemy
        let expected = enemy { enemyAlive = False}
        assertEqual "Problem with changeEnemyAlive" expected result]

unitTestBulletKillEnemy :: Test
unitTestBulletKillEnemy = TestList
    [ TestCase $ do
        let enemy = Enemy (100, 100) True
            bullet = Bullet (100, 100) (0, 3) False
        let result = bulletKillEnemy enemy bullet
        let expected = True
        assertEqual "Problem with bulletKillEnemy" expected result
    , TestCase $ do
        let enemy = Enemy (100, 100) True
            bullet = Bullet (80, 115) (0, 3) False
        let result = bulletKillEnemy enemy bullet
        let expected = True
        assertEqual "Problem with bulletKillEnemy" expected result
    , TestCase $ do
        let enemy = Enemy (100, 100) True
            bullet = Bullet (79, 116) (0, 3) False
        let result = bulletKillEnemy enemy bullet
        let expected = False 
        assertEqual "Problem with bulletKillEnemy" expected result
    , TestCase $ do
        let enemy = Enemy (100, 100) True
            bullet = Bullet (100, 70) (0, 3) False
        let result = bulletKillEnemy enemy bullet
        let expected = False 
        assertEqual "Problem with bulletKillEnemy" expected result
    , TestCase $ do
        let enemy = Enemy (100, 100) False
            bullet = Bullet (130, 110) (0, 0) False
        let result = bulletKillEnemy enemy bullet
        let expected = False 
        assertEqual "Problem with bulletKillEnemy" expected result
    ]
    
unitTestBulletsKillEnemy :: Test
unitTestBulletsKillEnemy = TestList
    [ TestCase $ do
        let bullets = [Bullet (110, 110) (0, 3) False, Bullet (90, 90) (0, 3) False]
            enemy = Enemy (100, 100) True
        let result = bulletsKillEnemy bullets enemy
        let expected = True 
        assertEqual "Problem with bulletsKillEnemy" expected result
    , TestCase $ do
        let bullets = [Bullet (70, 70) (0, 3) False, Bullet (110, 110) (0, 3) False]
            enemy = Enemy (100, 100) True
        let result = bulletsKillEnemy bullets enemy
        let expected = True 
        assertEqual "Problem with bulletsKillEnemy" expected result
    , TestCase $ do
        let bullets = [Bullet (130, 130) (0, 3) False, Bullet (70, 70) (0, 3) False]
            enemy = Enemy (100, 100) True
        let result = bulletsKillEnemy bullets enemy
        let expected = False 
        assertEqual "Problem with bulletsKillEnemy" expected result
    , TestCase $ do
        let bullets = []
            enemy = Enemy (100, 100) True
        let result = bulletsKillEnemy bullets enemy
        let expected = False 
        assertEqual "Problem with bulletsKillEnemy" expected result
    ]

unitTestBulletsKillEnemies :: Test
unitTestBulletsKillEnemies = TestList
    [ TestCase $ do
        let bullets = [Bullet (110, 110) (0, 3) False, Bullet (210, 210) (0, 3) False]
            enemies = [Enemy (100, 100) True, Enemy (200, 200) True]
            result = bulletsKillEnemies bullets enemies
            expected = [Enemy (100, 100) False, Enemy (200, 200) False]
        assertEqual "Problem with bulletsKillEnemies" expected result
    , TestCase $ do
        let bullets = [Bullet (110, 110) (0, 3) False, Bullet (90, 90) (0, 3) False]
            enemies = [Enemy (100, 100) True, Enemy (200, 200) True]
            result = bulletsKillEnemies bullets enemies
            expected = [Enemy (100, 100) False, Enemy (200, 200) True]
        assertEqual "Problem with bulletsKillEnemies" expected result
    , TestCase $ do
        let bullets = [Bullet (130, 130) (0, 3) False, Bullet (70, 70) (0, 3) False]
            enemies = [Enemy (100, 100) True, Enemy (200, 200) True]
            result = bulletsKillEnemies bullets enemies
            expected = enemies 
        assertEqual "Problem with bulletsKillEnemies" expected result
    , TestCase $ do
        let bullets = []
            enemies = [Enemy (100, 100) True, Enemy (200, 200) True]
            result = bulletsKillEnemies bullets enemies
            expected = enemies 
        assertEqual "Problem with bulletsKillEnemies" expected result
    , TestCase $ do
        let bullets = [Bullet (110, 110) (0, 3) False, Bullet (90, 90) (0, 3) False]
            enemies = []
            result = bulletsKillEnemies bullets enemies
            expected = enemies 
        assertEqual "Problem with bulletsKillEnemies" expected result
    ]

unitTestUsedBullet :: Test
unitTestUsedBullet = TestList
    [ TestCase $ do
        let bullet = Bullet (100, 100) (0, 3) False
            enemies = [Enemy (110, 110) True, Enemy (90, 90) True]
            result = usedBullet enemies bullet
            expected = bullet {bulletUsed = True}
        assertEqual "Problem with usedBullet" expected result
    , TestCase $ do
        let bullet = Bullet (130, 130) (0, 3) False
            enemies = [Enemy (110, 110) True, Enemy (90, 90) True]
            result = usedBullet enemies bullet
            expected = bullet 
        assertEqual "Problem with usedBullet" expected result
    , TestCase $ do
        let bullet = Bullet (100, 100) (0, 3) False
            enemies = []
            result = usedBullet enemies bullet
            expected = bullet 
        assertEqual "Problem with usedBullet" expected result
    ]

unitTestUsedPlayerBullets :: Test
unitTestUsedPlayerBullets = TestList
    [ TestCase $ do
        let bullets = [Bullet (100, 100) (0, 0) False, Bullet (200, 200) (0, 0) False]
            enemies = [Enemy (110, 110) True, Enemy (90, 90) True]
            result = usedPlayerBullets enemies bullets
            expected = [Bullet (100, 100) (0, 0) True, Bullet (200, 200) (0, 0) False]
        assertEqual "Problem with usedPlayerBullets" expected result
    , TestCase $ do
        let bullets = [Bullet (100, 100) (0, 0) False, Bullet (80, 80) (0, 0) False]
            enemies = [Enemy (110, 110) True, Enemy (90, 90) True]
            result = usedPlayerBullets enemies bullets
            expected = [Bullet (100, 100) (0, 0) True, Bullet (80, 80) (0, 0) True]
        assertEqual "Problem with usedPlayerBullets" expected result
    , TestCase $ do
        let bullets = [Bullet (130, 130) (0, 0) False, Bullet (200, 200) (0, 0) False]
            enemies = [Enemy (110, 110) True, Enemy (90, 90) True]
            result = usedPlayerBullets enemies bullets
            expected = bullets
        assertEqual "Problem with usedPlayerBullets" expected result
    , TestCase $ do
        let bullets = [Bullet (100, 100) (0, 0) False, Bullet (200, 200) (0, 0) False]
            enemies = []
            result = usedPlayerBullets enemies bullets
            expected = bullets
        assertEqual "Problem with usedPlayerBullets" expected result
    , TestCase $ do
        let bullets = []
            enemies = [Enemy (110, 110) True, Enemy (90, 90) True]
            result = usedPlayerBullets enemies bullets
            expected = bullets
        assertEqual "Problem with usedPlayerBullets" expected result
    ]

unitTestAddPoints :: Test
unitTestAddPoints = TestList
  [ TestCase $ do
      let player =  Player (0, -250) (0, 0) True "John" 0 0 False
      let points = 20
      let newPlayer = addPoints points player 
      let result = playerPoints newPlayer
      let expected = 20
      assertEqual "Problem with addPoints" expected result
  , TestCase $ do
      let player =  Player (0, -250) (0, 0) True "John" 15 0 False
      let points = 0
      let newPlayer = addPoints points player 
      let result = playerPoints newPlayer
      let expected = 15
      assertEqual "Problem with addPoints" expected result
  , TestCase $ do
      let player =  Player (0, -250) (0, 0) True "John" 51 0 False
      let points = 1
      let newPlayer = addPoints points player 
      let result = playerPoints newPlayer
      let expected = 52
      assertEqual "Problem with addPoints" expected result
  ]

unitTestPlayerKillEnemies :: Test
unitTestPlayerKillEnemies = TestList
    [ TestCase $ do
        let player = Player (0, -250) (0, 0) True "John" 0 0 False
            enemy1 = Enemy (100, 100) True
            enemy2 = Enemy (20, 20) True
            bullets = [Bullet (105, 105) (0, 0) False, Bullet (15, 15) (0, 0) False]
            game = Game GameMenu player bullets 0 [] "scores.txt" [enemy1, enemy2] 0 RightDir (mkStdGen 0) [] 0 1 False
            newGame = playerKillEnemies game
        let result = enemies newGame
        let expected = []
        assertEqual "Problem with playerKillEnemies" expected result
    , TestCase $ do
        let player = Player (0, -250) (0, 0) True "John" 0 0 False
            enemy1 = Enemy (100, 100) True
            enemy2 = Enemy (200, 200) True
            bullets = [Bullet (105, 105) (0, 0) False, Bullet (15, 15) (0, 0) False]
            game = Game GameMenu player bullets 0 [] "scores.txt" [enemy1, enemy2] 0 RightDir (mkStdGen 0) [] 0 1 False
            newGame = playerKillEnemies game
        let result = enemies newGame
        let expected = [enemy2]
        assertEqual "Problem with playerKillEnemies" expected result
    , TestCase $ do
        let player = Player (0, -250) (0, 0) True "John" 0 0 False
            enemy1 = Enemy (150, 150) True
            enemy2 = Enemy (200, 200) True
            bullets = [Bullet (105, 105) (0, 0) False, Bullet (15, 15) (0, 0) False]
            game = Game GameMenu player bullets 0 [] "scores.txt" [enemy1, enemy2] 0 RightDir (mkStdGen 0) [] 0 1 False
            newGame = playerKillEnemies game
        let result = enemies newGame
        let expected = [enemy1, enemy2]
        assertEqual "Problem with playerKillEnemies" expected result
    , TestCase $ do
        let playerGame = Player (0, -250) (0, 0) True "John" 10 0 False
            enemy1 = Enemy (100, 100) True
            enemy2 = Enemy (20, 20) True
            bullets = [Bullet (105, 105) (0, 0) False, Bullet (15, 15) (0, 0) False]
            game = Game GameMenu playerGame bullets 0 [] "scores.txt" [enemy1, enemy2] 0 RightDir (mkStdGen 0) [] 0 1 False
            newGame = playerKillEnemies game
        let result = playerPoints (player newGame)
        let expected = 12
        assertEqual "Problem with playerKillEnemies" expected result
    , TestCase $ do
        let playerGame = Player (0, -250) (0, 0) True "John" 10 0 False
            enemy1 = Enemy (100, 100) True
            enemy2 = Enemy (200, 200) True
            bullets = [Bullet (105, 105) (0, 0) False, Bullet (15, 15) (0, 0) False]
            game = Game GameMenu playerGame bullets 0 [] "scores.txt" [enemy1, enemy2] 0 RightDir (mkStdGen 0) [] 0 1 False
            newGame = playerKillEnemies game
        let result = playerPoints (player newGame)
        let expected = 11
        assertEqual "Problem with playerKillEnemies" expected result
    , TestCase $ do
        let playerGame = Player (0, -250) (0, 0) True "John" 10 0 False
            enemy1 = Enemy (150, 150) True
            enemy2 = Enemy (200, 200) True
            bullets = [Bullet (105, 105) (0, 0) False, Bullet (15, 15) (0, 0) False]
            game = Game GameMenu playerGame bullets 0 [] "scores.txt" [enemy1, enemy2] 0 RightDir (mkStdGen 0) [] 0 1 False
            newGame = playerKillEnemies game
        let result = playerPoints (player newGame)
        let expected = 10
        assertEqual "Problem with playerKillEnemies" expected result
    , TestCase $ do
        let player = Player (0, -250) (0, 0) True "John" 0 0 False
            enemy1 = Enemy (100, 100) True
            enemy2 = Enemy (20, 20) True
            bullets = [Bullet (105, 105) (0, 0) False, Bullet (15, 15) (0, 0) False]
            game = Game GameMenu player bullets 0 [] "scores.txt" [enemy1, enemy2] 0 RightDir (mkStdGen 0) [] 0 1 False
            newGame = playerKillEnemies game
        let result = playerBullets newGame
        let expected = []
        assertEqual "Problem with playerKillEnemies" expected result
    , TestCase $ do
        let player = Player (0, -250) (0, 0) True "John" 0 0 False
            enemy1 = Enemy (100, 100) True
            enemy2 = Enemy (200, 200) True
            bullets = [Bullet (105, 105) (0, 0) False, Bullet (15, 15) (0, 0) False]
            game = Game GameMenu player bullets 0 [] "scores.txt" [enemy1, enemy2] 0 RightDir (mkStdGen 0) [] 0 1 False
            newGame = playerKillEnemies game
        let result = playerBullets newGame
        let expected = [Bullet (15, 15) (0, 0) False]
        assertEqual "Problem with playerKillEnemies" expected result
    , TestCase $ do
        let player = Player (0, -250) (0, 0) True "John" 0 0 False
            enemy1 = Enemy (150, 150) True
            enemy2 = Enemy (200, 200) True
            bullets = [Bullet (105, 105) (0, 0) False, Bullet (15, 15) (0, 0) False]
            game = Game GameMenu player bullets 0 [] "scores.txt" [enemy1, enemy2] 0 RightDir (mkStdGen 0) [] 0 1 False
            newGame = playerKillEnemies game
        let result = playerBullets newGame
        let expected = bullets
        assertEqual "Problem with playerKillEnemies" expected result
    ]

unitTestPlayerOutOfScope :: Test
unitTestPlayerOutOfScope = TestList
  [ TestCase $ do
      let player =  Player (0, -250) (0, 0) True "John" 0 0 False
      let result = playerOutOfScope player
      let expected = player
      assertEqual "Problem with playerOutOfScope" expected result
  , TestCase $ do
      let player =  Player (0, -250) (-4, 0) True "John" 0 0 False
      let result = playerOutOfScope player
      let expected = player
      assertEqual "Problem with playerOutOfScope" expected result
  , TestCase $ do
      let player =  Player (475, -250) (4, 0) True "Ala" 15 0 False
      let result = playerOutOfScope player
      let expected = player
      assertEqual "Problem with playerOutOfScope" expected result
  , TestCase $ do
      let player =  Player (-475, -250) (-4, 0) True "Ala" 15 0 False
      let result = playerOutOfScope player
      let expected = player
      assertEqual "Problem with playerOutOfScope" expected result
  , TestCase $ do
      let player =  Player (480, -250) (4, 0) True "Ala" 15 0 False
      let result = playerOutOfScope player
      let expected = player {playerPos = (475, -250), playerVel = (0, 0)}
      assertEqual "Problem with playerOutOfScope" expected result
  , TestCase $ do
      let player =  Player (-480, -250) (-4, 0) True "Ala" 15 0 False
      let result = playerOutOfScope player
      let expected = player {playerPos = (-475, -250), playerVel = (0, 0)}
      assertEqual "Problem with playerOutOfScope" expected result
  ]

unitTestUpdatePlayerPosition :: Test
unitTestUpdatePlayerPosition = TestList
  [ TestCase $ do
      let player =  Player (0, -250) (0, 0) True "John" 0 0 False
      let result = updatePlayerPosition player 
      let expected = player 
      assertEqual "Problem with updatePlayerPosition" expected result
  , TestCase $ do
      let player =  Player (0, -250) (-4, 0) True "John" 0 0 False
      let result = updatePlayerPosition player 
      let expected = player {playerPos = (-4,-250)}
      assertEqual "Problem with updatePlayerPosition" expected result
  , TestCase $ do
      let player =  Player (0, -250) (4, 0) True "John" 0 0 False
      let result = updatePlayerPosition player 
      let expected = player {playerPos = (4,-250)}
      assertEqual "Problem with updatePlayerPosition" expected result
  , TestCase $ do
      let player =  Player (475, -250) (-4, 0) True "Ala" 15 0 False
      let result = updatePlayerPosition player 
      let expected = player {playerPos = (471,-250)}
      assertEqual "Problem with updatePlayerPosition" expected result
  , TestCase $ do
      let player =  Player (-475, -250) (4, 0) True "Ala" 15 0 False
      let result = updatePlayerPosition player 
      let expected = player {playerPos = (-471,-250)}
      assertEqual "Problem with updatePlayerPosition" expected result
  , TestCase $ do
      let player =  Player (480, -250) (4, 0) True "Ala" 15 0 False
      let result = updatePlayerPosition player 
      let expected = player {playerPos = (475, -250), playerVel = (0, 0)}
      assertEqual "Problem with updatePlayerPosition" expected result
  , TestCase $ do
      let player =  Player (-480, -250) (-4, 0) True "Ala" 15 0 False
      let result = updatePlayerPosition player 
      let expected = player {playerPos = (-475, -250), playerVel = (0, 0)}
      assertEqual "Problem with updatePlayerPosition" expected result
  ]
  
unitTestUpdateBulletPosition :: Test
unitTestUpdateBulletPosition = TestList
  [ TestCase $ do
      let bullet = Bullet (0, 0) (0, 2) False
      let updatedBullet = updateBulletPosition bullet
      let result = bulletPos updatedBullet
      let expected = (0,2)
      assertEqual "Problem with updateBulletPosition" expected result
  , TestCase $ do
      let bullet = Bullet (10, 10) (0, 0) False
      let updatedBullet = updateBulletPosition bullet
      let result = bulletPos updatedBullet
      let expected = (10,10)
      assertEqual "Problem with updateBulletPosition" expected result
  , TestCase $ do
      let bullet = Bullet (5, 5) (0, -3) False
      let updatedBullet = updateBulletPosition bullet
      let result = bulletPos updatedBullet
      let expected = (5,2)
      assertEqual "Problem with updateBulletPosition" expected result
  ]

unitTestBulletOutOfScope :: Test
unitTestBulletOutOfScope = TestList
  [ TestCase $ do
      let bullet = Bullet (-10, 320) (0, 2) False
      let result = bulletOutOfScope bullet
      let expected = True 
      assertEqual "Problem with bulletOutOfScope" expected result
  , TestCase $ do
      let bullet = Bullet (20, -301) (0, 0) False
      let result = bulletOutOfScope bullet
      let expected = True 
      assertEqual "Problem with bulletOutOfScope" expected result
  , TestCase $ do
      let bullet = Bullet (100, 300) (0, -3) False
      let result = bulletOutOfScope bullet
      let expected = False 
      assertEqual "Problem with bulletOutOfScope" expected result
  ]

unitTestAddEnemiesBullets :: Test
unitTestAddEnemiesBullets = TestList
  [ 
    TestCase $ do
      let count = 1
          enemies = []
          result = addEnemiesBullets count enemies
          expected = []
      assertEqual "Problem with addEnemiesBullets" expected result,
    TestCase $ do
      let count = 1
          enemies = [Enemy (0, 0) True]
          result = addEnemiesBullets count enemies
          expected = [Bullet { bulletPos = (0, 0), bulletVel = (0, -2.0), bulletUsed = False }]
      assertEqual "Problem with addEnemiesBullets" expected result,
    TestCase $ do
      let count = 1
          enemies = [(Enemy (0, 0) True), (Enemy (10,10) True)]
          result = addEnemiesBullets count enemies
          expected = [(Bullet { bulletPos = (0, 0), bulletVel = (0, -2.0), bulletUsed = False }), (Bullet { bulletPos = (10, 10), bulletVel = (0, -2.0), bulletUsed = False })]
      assertEqual "Problem with addEnemiesBullets" expected result
  ]

unitTestMoveEnemyRight :: Test
unitTestMoveEnemyRight = TestList
   [ TestCase $ do
        let enemy =  Enemy (0, 0) True 
        let result = moveEnemyRight 2 enemy
        let expected = enemy {enemyPos = (2,0)}
        assertEqual "Problem with moveEnemyRight" expected result,
     TestCase $ do 
        let enemy =  Enemy (0, 0) True 
        let result = moveEnemyRight (-2) enemy
        let expected = enemy 
        assertEqual "Problem with moveEnemyRight" expected result,
     TestCase $ do 
        let enemy =  Enemy (0, 0) True 
        let result = moveEnemyRight 0 enemy
        let expected = enemy 
        assertEqual "Problem with moveEnemyRight" expected result]

unitTestMoveEnemyLeft :: Test
unitTestMoveEnemyLeft = TestList
   [ TestCase $ do
        let enemy =  Enemy (0, 0) True 
        let result = moveEnemyLeft 2 enemy
        let expected = enemy {enemyPos = (-2,0)}
        assertEqual "Problem with moveEnemyLeft" expected result,
     TestCase $ do 
        let enemy =  Enemy (0, 0) True 
        let result = moveEnemyLeft (-2) enemy
        let expected = enemy 
        assertEqual "Problem with moveEnemyLeft" expected result,
     TestCase $ do 
        let enemy =  Enemy (0, 0) True 
        let result = moveEnemyLeft 0 enemy
        let expected = enemy 
        assertEqual "Problem with moveEnemyLeft" expected result]

unitTestMoveEnemyDown :: Test
unitTestMoveEnemyDown = TestList
   [ TestCase $ do
        let enemy =  Enemy (0, 0) True 
        let result = moveEnemyDown enemy
        let expected = enemy {enemyPos = (0,-5)}
        assertEqual "Problem with moveEnemyLeft" expected result]

unitTestUpdateEnemyPosition :: Test
unitTestUpdateEnemyPosition = TestList 
    [
        TestCase $ do 
            let deltaX = 4 
                currentDir = RightDir 
                enemy = Enemy (0,0) True 
                result = updateEnemyPosition deltaX currentDir enemy
                expected = Enemy (4,0) True
            assertEqual "Problem with updateEnemyPosition" expected result,
        TestCase $ do 
            let deltaX = 4 
                currentDir = LeftDir 
                enemy = Enemy (0,0) True 
                result = updateEnemyPosition deltaX currentDir enemy
                expected = Enemy (-4,0) True
            assertEqual "Problem with updateEnemyPosition" expected result,
        TestCase $ do 
            let deltaX = 4 
                currentDir = DownDirAfterLeft
                enemy = Enemy (0,0) True 
                result = updateEnemyPosition deltaX currentDir enemy
                expected = Enemy (0,-5) True
            assertEqual "Problem with updateEnemyPosition" expected result,
        TestCase $ do 
            let deltaX = 4 
                currentDir = DownDirAfterRight
                enemy = Enemy (0,0) True 
                result = updateEnemyPosition deltaX currentDir enemy
                expected = Enemy (0,-5) True
            assertEqual "Problem with updateEnemyPosition" expected result
    ]

unitTestUpdateEnemiesPosition :: Test
unitTestUpdateEnemiesPosition = TestList 
    [
        TestCase $ do 
            let deltaX = 4 
                currentDir = RightDir 
                enemies = [(Enemy (0,0) True), (Enemy (5,0) True)] 
                result = updateEnemiesPosition deltaX currentDir enemies
                expected = [(Enemy (4,0) True), (Enemy (9,0) True)]
            assertEqual "Problem with updateEnemiesPosition" expected result,
        TestCase $ do 
            let deltaX = 10 
                currentDir = LeftDir 
                enemies = [(Enemy (0,0) True), (Enemy (5,0) True)] 
                result = updateEnemiesPosition deltaX currentDir enemies
                expected = [(Enemy (-10,0) True), (Enemy (-5,0) True)]
            assertEqual "Problem with updateEnemiesPosition" expected result,
        TestCase $ do 
            let deltaX = 20 
                currentDir = LeftDir 
                enemies = [(Enemy (0,0) True), (Enemy (5,0) True)] 
                result = updateEnemiesPosition deltaX currentDir enemies
                expected = [(Enemy (-10,0) True), (Enemy (-5,0) True)]
            assertEqual "Problem with updateEnemiesPosition" expected result,
        TestCase $ do 
            let deltaX = 20
                currentDir = DownDirAfterLeft
                enemies = [(Enemy (0,0) True), (Enemy (5,0) True)]  
                result = updateEnemiesPosition deltaX currentDir enemies
                expected = [(Enemy (0,-5) True), (Enemy (5,-5) True)]
            assertEqual "Problem with updateEnemiesPosition" expected result,
        TestCase $ do 
            let deltaX = 4 
                currentDir = DownDirAfterRight
                enemies = []
                result = updateEnemiesPosition deltaX currentDir enemies
                expected = []
            assertEqual "Problem with updateEnemiesPosition" expected result
    ]

unitTestGetNextDirection :: Test
unitTestGetNextDirection =
  TestList
    [ TestCase $ do
        let result = getNextDirection LeftDir
        let expected = DownDirAfterLeft 
        assertEqual "Problem with getNextDirection" expected result,
      TestCase $ do
        let result = getNextDirection RightDir
        let expected = DownDirAfterRight 
        assertEqual "Problem with getNextDirection" expected result,
      TestCase $ do
        let result = getNextDirection DownDirAfterLeft
        let expected = RightDir
        assertEqual "Problem with getNextDirection" expected result,
      TestCase $ do
        let result = getNextDirection DownDirAfterRight
        let expected = LeftDir 
        assertEqual "Problem with getNextDirection" expected result
    ]

unitTestShouldChangeDirection :: Test
unitTestShouldChangeDirection = TestList
  [ TestCase $ do
    let enemies = [Enemy {enemyPos = (480, 0), enemyAlive = True}, Enemy {enemyPos = (400, 0), enemyAlive = True}]
    let result = shouldChangeDirection RightDir enemies 
    let expected = True 
    assertEqual "Problem with shouldChangeDirection" expected result
  , TestCase $ do
    let enemies = [Enemy {enemyPos = (-480, 0), enemyAlive = True}, Enemy {enemyPos = (-400, 0), enemyAlive = True}]
    let result = shouldChangeDirection LeftDir enemies 
    let expected = True 
    assertEqual "Problem with shouldChangeDirection" expected result
  , TestCase $ do
    let enemies = [Enemy {enemyPos = (400, 0), enemyAlive = True}, Enemy {enemyPos = (300, 0), enemyAlive = True}]
    let result = shouldChangeDirection RightDir enemies 
    let expected = False
    assertEqual "Problem with shouldChangeDirection" expected result
  , TestCase $ do
    let enemies = [Enemy {enemyPos = (-400, 0), enemyAlive = True}, Enemy {enemyPos = (-300, 0), enemyAlive = True}]
    let result = shouldChangeDirection LeftDir enemies 
    let expected = False 
    assertEqual "Problem with shouldChangeDirection" expected result
  , TestCase $ do 
    let enemies = [Enemy {enemyPos = (480, 0), enemyAlive = True}, Enemy {enemyPos = (400, 0), enemyAlive = True}]
    let result = shouldChangeDirection LeftDir enemies 
    let expected = False
    assertEqual "Problem with shouldChangeDirection" expected result
  , TestCase $ do
    let enemies = [Enemy {enemyPos = (-480, 0), enemyAlive = True}, Enemy {enemyPos = (-400, 0), enemyAlive = True}]
    let result = shouldChangeDirection RightDir enemies 
    let expected = False 
    assertEqual "Problem with shouldChangeDirection" expected result
  , TestCase $ do
    let enemies = [Enemy {enemyPos = (480, 0), enemyAlive = True}, Enemy {enemyPos = (400, 0), enemyAlive = True}]
    let result = shouldChangeDirection DownDirAfterLeft enemies 
    let expected = True 
    assertEqual "Problem with shouldChangeDirection" expected result
  , TestCase $ do
    let enemies = [Enemy {enemyPos = (-480, 0), enemyAlive = True}, Enemy {enemyPos = (-400, 0), enemyAlive = True}]
    let result = shouldChangeDirection DownDirAfterRight enemies 
    let expected = True 
    assertEqual "Problem with shouldChangeDirection" expected result
  ]

unitTestNextDirection :: Test
unitTestNextDirection = TestList
  [ TestCase $ do
    let enemies = [Enemy {enemyPos = (480, 0), enemyAlive = True}, Enemy {enemyPos = (400, 0), enemyAlive = True}]
    let result = nextDirection RightDir enemies 
    let expected = DownDirAfterRight  
    assertEqual "Problem with nextDirection" expected result
  , TestCase $ do
    let enemies = [Enemy {enemyPos = (-480, 0), enemyAlive = True}, Enemy {enemyPos = (-400, 0), enemyAlive = True}]
    let result = nextDirection LeftDir enemies 
    let expected = DownDirAfterLeft 
    assertEqual "Problem with nextDirection" expected result
  , TestCase $ do
    let enemies = [Enemy {enemyPos = (400, 0), enemyAlive = True}, Enemy {enemyPos = (300, 0), enemyAlive = True}]
    let result = nextDirection RightDir enemies 
    let expected = RightDir  
    assertEqual "Problem with nextDirection" expected result
  , TestCase $ do
    let enemies = [Enemy {enemyPos = (-400, 0), enemyAlive = True}, Enemy {enemyPos = (-300, 0), enemyAlive = True}]
    let result = nextDirection LeftDir enemies 
    let expected = LeftDir
    assertEqual "Problem with nextDirection" expected result
  , TestCase $ do
    let enemies = [Enemy {enemyPos = (480, 0), enemyAlive = True}, Enemy {enemyPos = (400, 0), enemyAlive = True}]
    let result = nextDirection LeftDir enemies 
    let expected = LeftDir
    assertEqual "Problem with nextDirection" expected result
  , TestCase $ do
    let enemies = [Enemy {enemyPos = (-480, 0), enemyAlive = True}, Enemy {enemyPos = (-400, 0), enemyAlive = True}]
    let result = nextDirection RightDir enemies 
    let expected = RightDir  
    assertEqual "Problem with nextDirection" expected result
  , TestCase $ do
    let enemies = [Enemy {enemyPos = (480, 0), enemyAlive = True}, Enemy {enemyPos = (400, 0), enemyAlive = True}]
    let result = nextDirection DownDirAfterLeft enemies 
    let expected = RightDir  
    assertEqual "Problem with nextDirection" expected result
  , TestCase $ do
    let enemies = [Enemy {enemyPos = (-480, 0), enemyAlive = True}, Enemy {enemyPos = (-400, 0), enemyAlive = True}]
    let result = nextDirection DownDirAfterRight enemies 
    let expected = LeftDir
    assertEqual "Problem with nextDirection" expected result
   ]

unitTestSaveScore :: Test
unitTestSaveScore = TestList 
    [ 
    TestCase $ do
        let name = "John"
        let score = 30 
        let expected = name ++ ": " ++ show score ++ "\n"
        saveScore "unitTestSaveScore.txt" name score 
        result <- readFile "unitTestSaveScore.txt"
        assertEqual "Problem with saveScore" expected result
        removeFile "unitTestSaveScore.txt" ,
    TestCase $ do
        let name1 = "John"
        let score1 = 30 
        let name2 = "Alicia"
        let score2 = 0
        let expected = name1 ++ ": " ++ show score1 ++ "\n" ++ name2 ++ ": " ++ show score2 ++ "\n"
        saveScore "unitTestSaveScore.txt" name1 score1 
        saveScore "unitTestSaveScore.txt" name2 score2 
        result <- readFile "unitTestSaveScore.txt"
        assertEqual "Problem with saveScore" expected result
        removeFile "unitTestSaveScore.txt"
    ]

unitTestReadFromFile :: Test
unitTestReadFromFile = TestList
    [
        TestCase $ do
            handle <- openFile "unitTestReadFromFile.txt" WriteMode
            hClose handle
            handle' <- openFile "unitTestReadFromFile.txt" ReadMode
            result <- readFromFile handle'
            hClose handle'
            let expected = []
            assertEqual "Problem with readFromFile" expected result
            removeFile "unitTestReadFromFile.txt",
        TestCase $ do
            let name = "John"
            let score = 30 
            let expected = [name ++ ": " ++ show score]
            saveScore "unitTestReadFromFile.txt" name score 
            handle <- openFile "unitTestReadFromFile.txt" ReadMode
            result <- readFromFile handle
            hClose handle
            assertEqual "Problem with saveScore" expected result 
            removeFile "unitTestReadFromFile.txt" ,
        TestCase $ do
            let name1 = "John"
            let score1 = 30 
            let expected1 = name1 ++ ": " ++ show score1
            let name2 = "Alicia"
            let score2 = 50 
            let expected2 = name2 ++ ": " ++ show score2
            let expected = [expected1, expected2]
            saveScore "unitTestReadFromFile.txt" name1 score1 
            saveScore "unitTestReadFromFile.txt" name2 score2 
            handle <- openFile "unitTestReadFromFile.txt" ReadMode
            result <- readFromFile handle
            hClose handle
            assertEqual "Problem with saveScore" expected result 
            removeFile "unitTestReadFromFile.txt"  
    ]   

unitTestLoadHighScore :: Test
unitTestLoadHighScore = TestList
    [
        TestCase $ do
            handle <- openFile "unitTestLoadHighScore.txt" WriteMode
            hClose handle
            result <- loadHighScore "unitTestLoadHighScore.txt"
            let expected = 0
            assertEqual "Problem with loadHighScore" expected result
            removeFile "unitTestLoadHighScore.txt",
        TestCase $ do
            let name = "John"
            let score = 30 
            saveScore "unitTestLoadHighScore.txt" name score 
            result <- loadHighScore "unitTestLoadHighScore.txt"
            let expected = 30
            assertEqual "Problem with loadHighScore" expected result
            removeFile "unitTestLoadHighScore.txt",
        TestCase $ do
            let name1 = "John"
            let score1 = 30 
            let name2 = "Alicia"
            let score2 = 50 
            let name3 = "Justin"
            let score3 = 100 
            saveScore "unitTestLoadHighScore.txt" name1 score1 
            saveScore "unitTestLoadHighScore.txt" name2 score2 
            saveScore "unitTestLoadHighScore.txt" name3 score3 
            result <- loadHighScore "unitTestLoadHighScore.txt"
            let expected = 100
            assertEqual "Problem with loadHighScore" expected result
            removeFile "unitTestLoadHighScore.txt"
    ]   

unitTestLoadPlayerHighScore :: Test
unitTestLoadPlayerHighScore = TestList
    [
        TestCase $ do
            handle <- openFile "unitTestLoadPlayerHighScore.txt" WriteMode
            hClose handle
            result <- loadPlayerHighScore "John" "unitTestLoadPlayerHighScore.txt"
            let expected = 0
            assertEqual "Problem with loadPlayerHighScore" expected result
            removeFile "unitTestLoadPlayerHighScore.txt",
        TestCase $ do
            let name = "John"
            let score = 30 
            saveScore "unitTestLoadPlayerHighScore.txt" name score 
            result <- loadPlayerHighScore "John" "unitTestLoadPlayerHighScore.txt"
            let expected = 30
            assertEqual "Problem with loadPlayerHighScore" expected result
            removeFile "unitTestLoadPlayerHighScore.txt",
        TestCase $ do
            let name = "John"
            let score = 30 
            saveScore "unitTestLoadPlayerHighScore.txt" name score 
            result <- loadPlayerHighScore "Tom" "unitTestLoadPlayerHighScore.txt"
            let expected = 0
            assertEqual "Problem with loadPlayerHighScore" expected result
            removeFile "unitTestLoadPlayerHighScore.txt",
        TestCase $ do
            let name1 = "John"
            let score1 = 30 
            let name2 = "Alicia"
            let score2 = 50 
            let name3 = "John"
            let score3 = 100 
            saveScore "unitTestLoadPlayerHighScore.txt" name1 score1 
            saveScore "unitTestLoadPlayerHighScore.txt" name2 score2 
            saveScore "unitTestLoadPlayerHighScore.txt" name3 score3 
            result <- loadPlayerHighScore "John" "unitTestLoadPlayerHighScore.txt"
            let expected = 100
            assertEqual "Problem with loadPlayerHighScore" expected result
            removeFile "unitTestLoadPlayerHighScore.txt",
        TestCase $ do
            let name1 = "John"
            let score1 = 30 
            let name2 = "Alicia"
            let score2 = 50 
            let name3 = "John"
            let score3 = 100 
            let name4 = "Alicia"
            let score4 = 10
            saveScore "unitTestLoadPlayerHighScore.txt" name1 score1 
            saveScore "unitTestLoadPlayerHighScore.txt" name2 score2 
            saveScore "unitTestLoadPlayerHighScore.txt" name3 score3 
            saveScore "unitTestLoadPlayerHighScore.txt" name4 score4 
            result <- loadPlayerHighScore "Alicia" "unitTestLoadPlayerHighScore.txt"
            let expected = 50
            assertEqual "Problem with loadPlayerHighScore" expected result
            removeFile "unitTestLoadPlayerHighScore.txt"
    ]   

unitTestLoadHighScores :: Test
unitTestLoadHighScores = TestList
    [
        TestCase $ do
            handle <- openFile "unitTestLoadHighScores.txt" WriteMode
            hClose handle
            result <- loadHighScores "unitTestLoadHighScores.txt"
            let expected = []
            assertEqual "Problem with loadHighScores" expected result
            removeFile "unitTestLoadHighScores.txt",
        TestCase $ do
            let name = "John"
            let score = 30 
            saveScore "unitTestLoadHighScores.txt" name score 
            result <- loadHighScores "unitTestLoadHighScores.txt"
            let expected = [("John",30)]
            assertEqual "Problem with loadHighScores" expected result
            removeFile "unitTestLoadHighScores.txt",
        TestCase $ do
            let name1 = "John"
            let score1 = 30 
            let name2 = "Alicia"
            let score2 = 50 
            let name3 = "Justin"
            let score3 = 100 
            saveScore "unitTestLoadHighScores.txt" name1 score1 
            saveScore "unitTestLoadHighScores.txt" name2 score2 
            saveScore "unitTestLoadHighScores.txt" name3 score3 
            result <- loadHighScores "unitTestLoadHighScores.txt"
            let expected = [("John",30), ("Alicia",50),("Justin",100)]
            assertEqual "Problem with loadHighScores" expected result
            removeFile "unitTestLoadHighScores.txt"
    ]   