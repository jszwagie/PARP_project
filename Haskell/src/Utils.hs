module Utils
  ( printLines,
    printPrompt,
    readCommand,
    instructionsText,
    Lines,
  )
where

import System.IO (hFlush, stdout)

type Lines = [String]

instructionsText :: Lines
instructionsText =
  [ "Available commands are:",
    "look               -- look around you and describe surroundings",
    "go <place>         -- go to a place",
    "examine <obj>      -- examine an object or person closely",
    "talk <person>      -- talk to someone",
    "take <obj>         -- pick up an object",
    "drop <obj>         -- put down an object",
    "use <obj>          -- use an object you're carrying",
    "inventory          -- list currently held items",
    "instructions       -- see these instructions",
    "hint               -- get a hint if you're stuck",
    "quit               -- end the game and quit",
    ""
  ]

printPrompt :: IO ()
printPrompt = putStr "> " >> hFlush stdout

readCommand :: IO String
readCommand = printPrompt >> getLine

printLines :: Lines -> IO ()
printLines = mapM_ putStrLn