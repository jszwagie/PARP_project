{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module Utils
  ( printLines,
    printPrompt,
    readCommand,
    instructionsText,
    Entity (..),
    EntityType (..),
    emptyPlayer,
    PlayerState (..),
    Lines,
    GameState (..),
    Location (..),
    parseCommand,
    findHere,
    findVisible,
    entitiesAt,
    isSupply,
    supplyNames,
    extractPlayerState,
    Command (..),
    findEntity,
    isInInventory,
    addToInventory,
    removeFromInventory,
    markExamined,
    markTalked,
    addTask,
    removeTask,
    hasTask,
    hasExamined,
    hasItem,
  )
where

import Control.Applicative ((<|>))
import Data.Char (toLower)
import Data.List (delete, find)
import Data.Maybe (fromMaybe, isJust, isNothing)
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
    "act2               -- skip directly to Act 2",
    "act3               -- skip directly to Act 3",
    "quit               -- end the game and quit",
    ""
  ]

data EntityType = Item | Person deriving (Eq, Show)

data Location
  = Barrack
  | Yard
  | Runway
  | Depot
  | Tent
  | Cockpit
  | CrashSite
  | Cave
  | Wreck
  | Tunnel
  | Compartment
  | Ledge
  | Tree
  | Ruins
  | City
  | Rock
  | Unknown
  deriving (Eq, Show)

data GameState = GameState
  { currentLocation :: Location,
    locationEntities :: [(Location, [Entity])],
    inventory :: [Entity],
    examined :: [String],
    talked :: [String],
    tasks :: [String]
  }
  deriving (Show)

data Entity = Entity
  { entityType :: EntityType,
    entityName :: String,
    entityDescription :: String,
    takeableByDefault :: Bool
  }
  deriving (Eq, Show)

supplyNames :: [String]
supplyNames =
  ["food", "water", "geiger", "medkit", "radio", "gear", "tools"]

isSupply :: String -> Bool
isSupply n = map toLower n `elem` supplyNames

extractPlayerState :: GameState -> PlayerState
extractPlayerState st =
  PlayerState
    { inventory_ = inventory st
    }

data PlayerState = PlayerState
  {inventory_ :: [Entity]}
  deriving (Eq, Show)

emptyPlayer :: PlayerState
emptyPlayer = PlayerState {inventory_ = []}

printPrompt :: IO ()
printPrompt = putStr "> " >> hFlush stdout

readCommand :: IO String
readCommand = printPrompt >> getLine

printLines :: Lines -> IO ()
printLines = mapM_ putStrLn

data Command
  = CmdLook
  | CmdQuit
  | CmdInventory
  | CmdHint
  | CmdInstructions
  | CmdGo String
  | CmdExamine String
  | CmdTalk String
  | CmdTake String
  | CmdDrop String
  | CmdUse String
  | CmdNext
  | CmdAct2
  | CmdAct3
  | CmdUnknown
  deriving (Eq, Show)

type Args = [String]

parseCommand :: Args -> Command
parseCommand ["look"] = CmdLook
parseCommand ["quit"] = CmdQuit
parseCommand ["inventory"] = CmdInventory
parseCommand ["hint"] = CmdHint
parseCommand ["instructions"] = CmdInstructions
parseCommand ["talk", p] = CmdTalk p
parseCommand ["take", o] = CmdTake o
parseCommand ["drop", o] = CmdDrop o
parseCommand ["use", o] = CmdUse o
parseCommand ("go" : p : _) = CmdGo p
parseCommand ("examine" : x : _) = CmdExamine x
parseCommand ["next"] = CmdNext
parseCommand ["start"] = CmdNext
parseCommand ["act2"] = CmdAct2
parseCommand ["act3"] = CmdAct3
parseCommand _ = CmdUnknown

entitiesAt :: Location -> GameState -> [Entity]
entitiesAt loc st = fromMaybe [] (lookup loc (locationEntities st))

findHere :: String -> GameState -> Maybe Entity
findHere nm st =
  let lname = map toLower nm
   in find ((== lname) . map toLower . entityName) (entitiesAt (currentLocation st) st)

findVisible :: String -> GameState -> Maybe Entity
findVisible nm st =
  findHere nm st
    <|> find
      ( (== map toLower nm)
          . map toLower
          . entityName
      )
      (inventory st)

findEntity :: String -> GameState -> Maybe Entity
findEntity nm st =
  let locEs = fromMaybe [] (lookup (currentLocation st) (locationEntities st))
      allEs = locEs ++ inventory st
   in find ((== map toLower nm) . map toLower . entityName) allEs

isInInventory :: String -> GameState -> Bool
isInInventory nm st = any ((== map toLower nm) . map toLower . entityName) (inventory st)

addToInventory :: Entity -> GameState -> GameState
addToInventory ent st =
  let loc = currentLocation st
      remaining = delete ent (fromMaybe [] (lookup loc (locationEntities st)))
      newLocs = map (\(l, es) -> if l == loc then (l, remaining) else (l, es)) (locationEntities st)
   in st {inventory = ent : inventory st, locationEntities = newLocs}

removeFromInventory :: Entity -> GameState -> GameState
removeFromInventory ent st =
  let loc = currentLocation st
      inv' = delete ent (inventory st)
      locEs = fromMaybe [] (lookup loc (locationEntities st))
      newLocs = map (\(l, es) -> if l == loc then (l, ent : locEs) else (l, es)) (locationEntities st)
   in st {inventory = inv', locationEntities = newLocs}

markExamined :: String -> GameState -> GameState
markExamined nm st = st {examined = nm : examined st}

markTalked :: String -> String -> GameState -> GameState
markTalked p t st = st {talked = (p ++ "_" ++ t) : talked st}

addTask, removeTask :: String -> GameState -> GameState
addTask t st = if t `elem` tasks st then st else st {tasks = t : tasks st}
removeTask t st = st {tasks = filter (/= t) (tasks st)}

hasTask :: GameState -> String -> Bool
hasTask st t = t `elem` tasks st

hasItem :: GameState -> String -> Bool
hasItem st n = any ((== map toLower n) . map toLower . entityName) (inventory st)

hasExamined :: GameState -> String -> Bool
hasExamined st e = e `elem` examined st
