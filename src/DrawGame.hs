module DrawGame where 

import UpdateGame
import Graphics.Gloss

drawStartMenu :: Picture -> Picture
drawStartMenu bgPicture = pictures [bgPicture, titleLabel, startLabel, instructionLabel, boardLabel, exitLabel]
  where
    titleLabel = translate (-340) 50 $ color white $ scale 0.75 0.75 $ text "Space Invaders"
    startLabel = translate (-210) (-20) $ color white $ scale 0.2 0.2 $ text "press Enter to start the game"
    instructionLabel = translate (-210) (-70) $ color white $ scale 0.2 0.2 $ text "press i to show the instruction"
    boardLabel = translate (-210) (-120) $ color white $ scale 0.2 0.2 $ text "press x to show the leaderboard"
    exitLabel = translate (-210) (-170) $ color white $ scale 0.2 0.2 $ text "press Esc to exit the game"

drawInstructionMenu :: Picture -> Picture
drawInstructionMenu bgPicture = pictures [bgPicture, titleLabel, ruleFirstLabel, ruleSecondLabel, rightLabel, leftLabel, spaceLabel, startLabel, boardLabel, exitLabel]
  where
    titleLabel = translate (-340) 100 $ color white $ scale 0.75 0.75 $ text "Space Invaders"
    ruleFirstLabel = translate (-230) 50 $ color white $ scale 0.2 0.2 $ text "Kill as many enemies as you can"
    ruleSecondLabel = translate (-280) 0 $ color white $ scale 0.2 0.2 $ text "Don't let them kill you as long as you can"
    rightLabel = translate (-170) (-50) $ color white $ scale 0.2 0.2 $ text "press -> to move right"
    leftLabel = translate (-160) (-100) $ color white $ scale 0.2 0.2 $ text "press <- to move left"
    spaceLabel = translate (-150) (-150) $ color white $ scale 0.2 0.2 $ text "press Space to shoot"
    startLabel = translate (-490) (-250) $ color black $ scale 0.1 0.1 $ text "press Enter to start the game"
    boardLabel = translate (-490) (-270) $ color black $ scale 0.1 0.1 $ text "press x to show the leaderboard"
    exitLabel = translate (-490) (-290) $ color black $ scale 0.1 0.1 $ text "press Esc to exit the game"

drawNickMenu :: Picture -> GameState -> Picture
drawNickMenu bgPicture game = pictures [bgPicture, nameLabel, playLabel]
  where
    nameLabel = translate (-120) 0 $ color white $ scale 0.15 0.15 $ text $ "Enter your name: " ++ playerName (player game)
    playLabel =  translate (-100) (-100) $ color white $ scale 0.15 0.15 $ text "press Enter to play" 

drawStartBoardMenu :: Picture -> GameState -> Picture 
drawStartBoardMenu bgPicture game = pictures $ [bgPicture, startLabel, instructionLabel, exitLabel] ++ (zipWith drawTopTen [0..] (bestScores game))
  where 
    startLabel = translate (-490) (-250) $ color black $ scale 0.1 0.1 $ text "press Enter to start the game"
    instructionLabel = translate (-490) (-270) $ color black $ scale 0.1 0.1 $ text "press i to show the instruction"
    exitLabel = translate (-490) (-290) $ color black $ scale 0.1 0.1 $ text "press Esc to exit the game"

drawBoardMenu :: Picture -> GameState -> Picture 
drawBoardMenu bgPicture game = pictures $ [bgPicture, startLabel, exitLabel] ++ (zipWith drawTopTen [0..] (bestScores game))
  where 
    startLabel = translate (-490) (-270) $ color black $ scale 0.1 0.1 $ text "press Enter to start the game again"
    exitLabel = translate (-490) (-290) $ color black $ scale 0.1 0.1 $ text "press Esc to exit the game"

drawTopTen :: Int -> (String, Int) -> Picture
drawTopTen index (name, points) = translate (-120) (220 - fromIntegral index * 40) $ scale 0.2 0.2 $ color white $ text (show (index + 1) ++ ". " ++ name ++ ": " ++ show points)

drawPlayer :: Picture -> Player -> Picture
drawPlayer picturePlayer player = translate (fst (playerPos player)) (snd (playerPos player)) $ scale 0.4 0.4 picturePlayer 

drawEnemiesBullets :: Picture -> [Bullet] -> Picture
drawEnemiesBullets pictureEnemyBullet bullets = pictures [drawEnemyBullet pictureEnemyBullet bullet | bullet <- bullets]

drawEnemyBullet :: Picture -> Bullet -> Picture
drawEnemyBullet pictureEnemyBullet bullet = translate (fst (bulletPos bullet)) (snd (bulletPos bullet)) $ scale 0.5 0.5 pictureEnemyBullet

drawPlayerBullets :: Picture -> [Bullet] -> Picture
drawPlayerBullets picturePlayerBullet bullets = pictures [drawPlayerBullet picturePlayerBullet bullet | bullet <- bullets]

drawPlayerBullet :: Picture -> Bullet -> Picture
drawPlayerBullet picturePlayerBullet bullet = translate (fst (bulletPos bullet)) (snd (bulletPos bullet)) $ scale 0.2 0.2 picturePlayerBullet

drawEnemies :: Picture -> [Enemy] -> Picture
drawEnemies pictureEnemy enemies = pictures [drawEnemy pictureEnemy enemy | enemy <- enemies]

drawEnemy :: Picture -> Enemy -> Picture
drawEnemy pictureEnemy enemy = translate (fst (enemyPos enemy)) (snd (enemyPos enemy)) $ scale 0.25 0.25 pictureEnemy 

drawPoints :: Int -> Picture
drawPoints points = translate (-490) 260 $ color white $ scale 0.1 0.1 $ text ("Your points: " ++ show points)

drawGameMenu :: Picture -> Picture -> Picture -> Picture -> Picture -> GameState -> Picture 
drawGameMenu bgPicture picturePlayer pictureEnemy picturePlayerBullet pictureEnemyBullet game = pictures [bgPicture, drawPlayerBullets picturePlayerBullet (playerBullets game), drawEnemiesBullets pictureEnemyBullet (enemiesBullets game), drawPlayer picturePlayer (player game), drawEnemies pictureEnemy (enemies game), drawPoints (playerPoints (player game))]

drawEndMenu :: Picture -> GameState -> Picture
drawEndMenu bgPicture game = pictures [ bgPicture, overLabel, playerScoreLabel, playerHighscoreLabel, globalHighscoreLabel, startLabel, boardLabel, exitLabel]
  where 
    overLabel = translate (-210) 50 $ color white $ scale 0.5 0.5 $ text "GAME OVER"
    playerScoreLabel = translate (-205) 0 $ color white $ scale 0.3 0.3 $ text $ "Score: " ++ show (playerPoints (player game))
    playerHighscoreLabel = translate (-205) (-50) $ color white $ scale 0.3 0.3 $ text $ "Your highscore: " ++ show (playerHighscore (player game))
    globalHighscoreLabel = translate (-205) (-100) $ color white $ scale 0.3 0.3 $ text $ "Global highscore: " ++ show (generalHighscore game)
    startLabel = translate (-490) (-250) $ color black $ scale 0.1 0.1 $ text "press Enter to start the game again"
    boardLabel = translate (-490) (-270) $ color black $ scale 0.1 0.1 $ text "press x to show the leaderboard"
    exitLabel = translate (-490) (-290) $ color black $ scale 0.1 0.1 $ text "press Esc to exit the game"

drawGameIO :: Picture -> Picture -> Picture -> Picture -> Picture -> GameState -> IO Picture
drawGameIO bgPicture picturePlayer pictureEnemy picturePlayerBullet pictureEnemyBullet game
  | menu game == StartMenu = return $ drawStartMenu bgPicture 
  | menu game == InstructionMenu = return $ drawInstructionMenu bgPicture
  | menu game == NickMenu = return $ drawNickMenu bgPicture game
  | menu game == BoardMenu = return $ drawBoardMenu bgPicture game 
  | menu game == StartBoardMenu = return $ drawStartBoardMenu bgPicture game 
  | menu game == GameMenu = return $ drawGameMenu bgPicture picturePlayer pictureEnemy picturePlayerBullet pictureEnemyBullet game
  | menu game == EndMenu = return $ drawEndMenu bgPicture game
