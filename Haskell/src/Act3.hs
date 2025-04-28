module Act3 (startAct3) where

import Control.Applicative ((<|>))
import Data.Char (toLower)
import Data.List (delete, find, partition)
import Data.Maybe (fromJust, fromMaybe, isJust, isNothing)
import System.Exit (exitSuccess)
import System.IO (hFlush, stdout)
import Utils
  ( Command (..),
    Entity (..),
    EntityType (..),
    GameState (..),
    Lines,
    Location (..),
    PlayerState (..),
    addTask,
    addToInventory,
    emptyPlayer,
    entitiesAt,
    extractPlayerState,
    findEntity,
    findHere,
    findVisible,
    hasExamined,
    hasItem,
    hasTask,
    instructionsText,
    isInInventory,
    isSupply,
    markExamined,
    markTalked,
    parseCommand,
    printLines,
    printPrompt,
    readCommand,
    removeFromInventory,
    removeTask,
    supplyNames,
  )

initialState :: GameState
initialState =
  GameState
    { currentLocation = Ledge,
      locationEntities = initialEntities,
      inventory = [],
      examined = [],
      talked = [],
      tasks = ["ledge_talk"]
    }

act3Prolog :: Lines
act3Prolog =
  [ "ACT 3: INTO THE HEART OF THE UNKNOWN",
    "",
    "You and Clara carefully climb down from the ledge, your boots sinking into the soft, mossy ground.",
    "The valley pulses with life—chirping insects fill the air, leaves rustle in a gentle breeze, and the distant roar of an unseen beast sends a shiver down your spine.",
    "The memory of your crash-landed supplies lingers, a heavy burden as you take your first cautious steps into this strange, uncharted world.",
    ""
  ]

initialEntities :: [(Location, [Entity])]
initialEntities =
  [ (Ledge, [Entity Person "clara" "Clara stands beside you, looking out over the hidden valley." False]),
    (Tree, []),
    (Ruins, [Entity Person "creature" "A tall, slender figure with luminous eyes studying you with quiet intrigue." False]),
    (Tunnel, []),
    (City, []),
    (Rock, [])
  ]

canMove :: Location -> Location -> Bool
canMove Ledge Tree = True
canMove Tree Ledge = True
canMove Tree Ruins = True
canMove Ruins Tree = True
canMove Tunnel Tree = True
canMove Tree Tunnel = True
canMove Tree City = True
canMove City Tree = True
canMove _ _ = False

parseLocation :: String -> Location
parseLocation s = case map toLower s of
  "ledge" -> Ledge
  "tree" -> Tree
  "ruins" -> Ruins
  "woods" -> Ruins
  "tunnel" -> Tunnel
  "city" -> City
  "rock" -> Rock
  _ -> Unknown
