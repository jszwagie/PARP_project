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