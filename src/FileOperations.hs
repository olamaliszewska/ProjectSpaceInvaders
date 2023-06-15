module FileOperations where 

import System.IO
import Data.List.Split (splitOn)
import Data.List (isPrefixOf)

saveScore :: String -> String -> Int -> IO ()
saveScore scoreFile name score = do
  handle <- openFile scoreFile AppendMode
  hPutStrLn handle (name ++ ": " ++ show score)
  hClose handle

readFromFile :: Handle -> IO [String]
readFromFile handle = do
  eof <- hIsEOF handle
  if eof
    then return []
    else do
      x <- hGetLine handle
      xs <- readFromFile handle
      return (x:xs) 

loadHighScore :: String -> IO Int
loadHighScore scoreFile = do
  withFile scoreFile ReadMode $ \handle -> do
    scores <- readFromFile handle
    let points = map (read . last . splitOn ":") scores :: [Int]
    case points of
      [] -> return 0
      _ -> return (maximum points)

loadPlayerHighScore :: String -> String -> IO Int
loadPlayerHighScore username scoreFile = do
  withFile scoreFile ReadMode $ \handle -> do
    scores <- readFromFile handle
    let userScores = filter (\record -> username `isPrefixOf` record) scores
        points = map (read . last . splitOn ":") userScores :: [Int]
    case points of
      [] -> return 0
      _ -> return (maximum points)

loadHighScores :: String -> IO [(String, Int)]
loadHighScores scoreFile = do
  withFile scoreFile ReadMode $ \handle -> do
    scores <- readFromFile handle
    let users = map (head . splitOn ":") scores
        points = map (read . last . splitOn ":") scores :: [Int]
    return $ zip users points 