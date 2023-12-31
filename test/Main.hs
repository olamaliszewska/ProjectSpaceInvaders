module Main (main) where

import UnitTests
import PropertiesTests
import Test.QuickCheck
import Test.HUnit

main :: IO ()
main = do
  runTestTT unitTestReplayGame
  runTestTT unitTestAddLetter
  runTestTT unitTestDeleteLastLetter
  runTestTT unitTestStopPlayerMoving
  runTestTT unitTestChangePlayerVelocityRight
  runTestTT unitTestChangePlayerVelocityLeft
  runTestTT unitTestSortScores
  runTestTT unitTestChangePlayerScoreSaved
  runTestTT unitTestChangePlayerHighscore
  runTestTT unitTestEnemyTooFar
  runTestTT unitTestCheckEndGame
  runTestTT unitTestChangePlayerAlive
  runTestTT unitTestBulletKillPlayer
  runTestTT unitTestEnemiesKillPlayer
  runTestTT unitTestChangeEnemyAlive
  runTestTT unitTestBulletKillEnemy
  runTestTT unitTestBulletsKillEnemy
  runTestTT unitTestBulletsKillEnemies
  runTestTT unitTestUsedBullet
  runTestTT unitTestUsedPlayerBullets
  runTestTT unitTestAddPoints
  runTestTT unitTestPlayerKillEnemies
  runTestTT unitTestPlayerOutOfScope
  runTestTT unitTestUpdatePlayerPosition
  runTestTT unitTestUpdateBulletPosition
  runTestTT unitTestBulletOutOfScope
  runTestTT unitTestAddEnemiesBullets
  runTestTT unitTestMoveEnemyRight
  runTestTT unitTestMoveEnemyLeft
  runTestTT unitTestMoveEnemyDown
  runTestTT unitTestUpdateEnemyPosition
  runTestTT unitTestUpdateEnemiesPosition
  runTestTT unitTestGetNextDirection
  runTestTT unitTestShouldChangeDirection
  runTestTT unitTestNextDirection
  runTestTT unitTestSaveScore
  runTestTT unitTestReadFromFile
  runTestTT unitTestLoadHighScore
  runTestTT unitTestLoadPlayerHighScore
  runTestTT unitTestLoadHighScores
  quickCheck testPlayerShoot
  quickCheck testBulletsKillEnemies
  quickCheck testRemoveUsedBullets
  quickCheck testPlayerKillEnemies
  quickCheck testUpdatePlayerPosition
  quickCheck testBulletOutOfScope
  quickCheck testRemoveBulletsOutOfScope
  quickCheck testUpdateBulletsPosition
  quickCheck testSelectRandomEnemies
  quickCheck testEnemiesShoot
  quickCheck testGenerateNewEnemies
