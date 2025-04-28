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
describeLocation :: Location -> String
describeLocation Ledge = ledgeDesc
describeLocation Tree = treeDesc
describeLocation Ruins = ruinsDesc
describeLocation Tunnel = tunnelDesc
describeLocation City = cityDesc
describeLocation Rock = rockDesc

ledgeDesc, treeDesc, ruinsDesc, tunnelDesc, cityDesc, rockDesc :: String
ledgeDesc =
  "You both stand on a rocky ledge overlooking a hidden realm—an expansive, verdant valley cradled beneath Antarctica's icy crust.\n\
  \Bioluminescent plants emit a soft, ethereal glow, casting light across towering ferns and crystalline rivers that shimmer like liquid glass.\n\
  \The air hangs warm and humid, thick with the scent of exotic blooms, a jarring contrast to the frozen desolation above.\n\
  \Flying saucers, eerily similar to the wreck you stumbled upon, glide silently through the skies, their presence a quiet warning of something watchful and alive down here."
treeDesc =
  "From the tree's upper branches, the valley sprawls before you in breathtaking detail.\n\
  \To the east, ancient-looking RUINS emerge from the foliage—crumbling pyramids and temples etched with cryptic symbols, remnants of a lost civilization.\n\
  \To the west, the stark silhouette of a CITY cuts through the greenery, its dark gray buildings festooned with swastika flags fluttering ominously in the breeze, their bold red and black stark against the muted stone.\n\
  \Behind you, the TUNNEL exit gapes like a dark maw, leading back to the frozen surface—a lifeline or a trap, depending on your next move."
ruinsDesc =
  "The ruins before you are a marvel of ancient architecture, reminiscent of Egypt's pyramids or the jungle temples of South America, yet distinctly alien.\n\
  \Crumbling stone facades are adorned with intricate carvings of starships and celestial beings, hinting at a civilization far beyond human comprehension.\n\
  \The air here feels thick with history and unspoken secrets."
tunnelDesc =
  "The crash site lies in ruins, the plane's twisted metal half-buried in snow.\n\
  \The wind howls mercilessly, and the sky above is a bleak, unforgiving gray.\n\
  \Your breath fogs in the frigid air, a stark contrast to the warmth of the hidden valley below."
cityDesc =
  "The city cuts a stark silhouette against the valley's greenery, its dark gray buildings rising like monolithic sentinels.\n\
  \Swastika flags flutter ominously from every structure, their bold red and black stark against the muted stone.\n\
  \The atmosphere is heavy with foreboding, as if the very walls are watching your every move."
rockDesc =
  "The massive boulder provides a makeshift shelter, its surface slick with glowing moss.\n\
  \You press against the cold stone, your breath ragged as the Nazi patrol draws closer.\n\
  \From this vantage point, you can see several Nazis, their faces twisted in determination as they search the area."

