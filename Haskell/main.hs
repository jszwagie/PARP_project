module Main where

import Data.Char (toLower)
import Data.List (find)
import Data.Maybe (fromJust, fromMaybe, isJust)
import System.IO (hFlush, stdout)

introductionText :: [String]
introductionText =
    [ "Welcome to The Hidden Realm"
    , "An interactive fiction adventure set in the icy depths of Antarctica."
    , "Unravel ancient secrets, face modern dangers, and shape your fate through your choices."
    , ""
    ]

instructionsText :: [String]
instructionsText =
    [ "Available commands are:"
    , "look           -- to look around you and describe surroundings"
    , "go <place>     -- to go to a place."
    , "examine <obj>  -- to examine an object or person closely."
    , "talk <person>  -- to talk to someone."
    , "take <obj>     -- to pick up an object."
    , "drop <obj>     -- to put down an object."
    , "inventory      -- list currently held items."
    , "instructions   -- to see these instructions."
    , "hint           -- to get a hint if you're stuck."
    , "quit           -- to end the game and quit."
    , ""
    ]

data Location = Barrack | Yard | Runway | Depot | Tent | Unknown deriving (Eq, Show)

newtype GameState = GameState { currentLocation :: Location }

initialState :: GameState
initialState = GameState { currentLocation = Yard }

canMove :: Location -> Location -> Bool
canMove Yard Barrack = True
canMove Yard Runway = True
canMove Yard Depot = True
canMove Yard Tent = True
canMove Barrack Yard = True
canMove Runway Yard = True
canMove Depot Yard = True
canMove Tent Yard = True
canMove _ _ = False

describeLocation :: Location -> String
describeLocation location = "This is the " ++ case location of
    Barrack -> "barrack"
    Yard -> "yard"
    Runway -> "runway"
    Depot -> "depot"
    Tent -> "tent"

parseLocation :: String -> Location
parseLocation str = case str of
    "barrack" -> Barrack
    "yard" -> Yard
    "runway" -> Runway
    "depot" -> Depot
    "tent" -> Tent
    _ -> Unknown

printLines :: [String] -> IO ()
printLines xs = putStr (unlines xs)

printIntroduction :: IO ()
printIntroduction = printLines introductionText

printInstructions :: IO ()
printInstructions = printLines instructionsText

readCommand :: IO String
readCommand = do
    putStr "> "
    hFlush stdout
    getLine

gameLoop :: GameState -> IO ()
gameLoop state = do
    cmd <- readCommand
    case words cmd of
        ["instructions"] -> do
            printInstructions
            gameLoop state
        ["quit"] -> return ()
        ["look"] -> do
            printLines [describeLocation (currentLocation state), ""]
            gameLoop state
        ["go", place] -> do
            let location = parseLocation place
            if location == Unknown
                then do
                    printLines ["Unknown place: " ++ place, ""]
                    gameLoop state
            else if canMove (currentLocation state) location
                then do
                    printLines [describeLocation location, ""]
                    gameLoop (state { currentLocation = location })
                else do
                    printLines ["You can't go to " ++ place ++ " from here.", ""]
                    gameLoop state
        _ -> do
            printLines ["Unknown command.", ""]
            gameLoop state


main :: IO ()
main = do
    printIntroduction
    printInstructions
    printLines [describeLocation Yard, ""]
    gameLoop initialState