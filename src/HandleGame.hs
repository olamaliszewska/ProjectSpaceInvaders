module HandleGame where

import UpdateGame
import Graphics.Gloss.Interface.IO.Game 

handleEventNick :: Event -> GameState -> GameState
handleEventNick (EventKey (Char c) Down _ _) game
    | c == '\b' = game {player = deleteLastLetter (player game)} 
    | otherwise =  game { player = addLetter [c] (player game)} 
handleEventNick _ game = game

handleEvent :: Event -> GameState -> GameState  
handleEvent (EventKey (SpecialKey KeyLeft) Down _ _) game 
    | menu game == GameMenu = game {player = changePlayerVelocityLeft (player game)} 
    | otherwise = game 
handleEvent (EventKey (SpecialKey KeyRight) Down _ _) game 
    | menu game == GameMenu = game {player = changePlayerVelocityRight (player game)} 
    | otherwise = game 
handleEvent (EventKey (SpecialKey KeyLeft) Up _ _) game 
    | menu game == GameMenu = game {player = stopPlayerMoving (player game)} 
    | otherwise = game 
handleEvent (EventKey (SpecialKey KeyRight) Up _ _) game 
    | menu game == GameMenu = game {player = stopPlayerMoving (player game)} 
    | otherwise = game 
handleEvent (EventKey (SpecialKey KeySpace) Down _ _) game 
    | menu game == GameMenu = playerShoot game 
    | otherwise = game 
handleEvent (EventKey (SpecialKey KeyEsc) Down _ _) game 
    | menu game == StartMenu || menu game == EndMenu || menu game == BoardMenu || menu game == StartBoardMenu || menu game == InstructionMenu = game {exit = True}
    | otherwise = game 
handleEvent (EventKey (SpecialKey KeyEnter) Down _ _) game 
    | menu game == EndMenu || menu game  == BoardMenu = replayGame game 
    | menu game == StartMenu || menu game == StartBoardMenu || menu game == InstructionMenu = game {menu = NickMenu}
    | menu game == NickMenu && playerName (player game) /= "" = game {menu = GameMenu}
    | otherwise = game
handleEvent event@(EventKey (Char 'x') Down _ _) game 
    | menu game == StartMenu || menu game == InstructionMenu = game {menu = StartBoardMenu}
    | menu game == EndMenu = game {menu = BoardMenu} 
    | menu game == NickMenu = handleEventNick event game 
    | otherwise = game  
handleEvent event@(EventKey (Char 'i') Down _ _) game 
    | menu game == StartMenu || menu game == StartBoardMenu = game {menu = InstructionMenu}
    | menu game == NickMenu = handleEventNick event game 
    | otherwise = game  
handleEvent event game 
    | menu game == NickMenu = handleEventNick event game 
    | otherwise = game 

handleEventIO :: Event -> GameState -> IO GameState 
handleEventIO event game = return $ handleEvent event game  