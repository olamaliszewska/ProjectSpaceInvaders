module Game where 

import HandleGame
import DrawGame 
import UpdateGame
import Graphics.Gloss.Interface.IO.Game 
import Graphics.Gloss.Juicy
import System.Random 
import System.Exit (exitWith, ExitCode(..))

window :: Display
window = InWindow "Space Invaders" (1000, 600) (10,10)  

playGame :: IO ()
playGame = do
  gen <- getStdGen
  bgPicture <- loadJuicyPNG "bgPicture.png"
  picturePlayer <- loadJuicyPNG "player.png"
  pictureEnemy <- loadJuicyPNG "enemy.png"
  picturePlayerBullet <- loadJuicyPNG "playerBullet.png"
  pictureEnemyBullet <- loadJuicyPNG "enemyBullet.png"
  case (,,,,) <$> bgPicture <*> picturePlayer <*> pictureEnemy <*> picturePlayerBullet <*> pictureEnemyBullet of
    Just (bgPic, playerPic, enemyPic, playerBulletPic, enemyBulletPic) -> do
      playIO window black 60 (initialState gen) (drawGameIO bgPic playerPic enemyPic playerBulletPic enemyBulletPic) handleEventIO update
    _ -> do
      putStrLn "Error loading images"
      exitWith (ExitFailure 1)