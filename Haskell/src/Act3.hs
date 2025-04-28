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

getHint :: GameState -> String
getHint st
  | st `hasTask` "act_finished" =
      "You've already finished this act. Type \"quit\" to exit the game."
  | st `hasTask` "woods" =
      "We should GO to the WOODS."
  | currentLocation st == Tunnel && st `hasItem` "radio" && st `hasTask` "tunnel" =
      "I should USE the RADIO, as I said."
  | currentLocation st == Rock && st `hasTask` "tunnel" =
      "We should go to TUNNEL."
  | currentLocation st == Rock && st `hasTask` "after_fight" =
      "I should talk to Clara."
  | currentLocation st == Rock && st `hasItem` "pistol" && st `hasTask` "fight" =
      "I should hand the PISTOL to Clara."
  | st `hasTask` "ambush_beginning" =
      "Maybe Clara knows what to do in this situation."
  | st `hasTask` "after_radio" =
      "I should talk to Clara."
  | st `hasTask` "radio" && st `hasItem` "radio" && st `hasExamined` "note" =
      "The note says 'Four's the square, Seven's luck, Two's pair.' That could point to the settings for A, B, and C. The plaque might help confirm it."
  | st `hasTask` "radio" && st `hasItem` "radio" =
      "I need to tune the dials to the right numbers to reach the Marines. The NOTE or the RADIO might hold the key."
  | st `hasTask` "hide" && st `hasItem` "pistol" =
      "I should GO behind that ROCK."
  | currentLocation st == Ledge && st `hasTask` "ledge_talk" =
      "I should talk to Clara."
  | currentLocation st == Ledge && not (st `hasExamined` "ledge") =
      "I should LOOK around"
  | currentLocation st == Ledge && st `hasTask` "tree" && st `hasExamined` "ledge" =
      "I think I could GO up on that TREE."
  | currentLocation st == Ledge && st `hasTask` "tree" =
      "I should find a high place for recon."
  | currentLocation st == Tree =
      "I should LOOK around and decide where to GO."
  | currentLocation st == Ruins && any ((== "creature") . entityName) (entitiesAt Ruins st) =
      "Is the TALK the answer?"
  | currentLocation st == Ruins && st `hasTask` "hide" =
      "I should hide behind that ROCK"
  | currentLocation st == City && st `hasTask` "hide" =
      "I should hide behind that ROCK"
  | currentLocation st == Ruins && st `hasTask` "tunnel" =
      "I should GO to the TUNNEL"
  | currentLocation st == City && st `hasTask` "tunnel" =
      "I should GO to the TUNNEL"
  | currentLocation st == Ruins && st `hasTask` "woods" =
      "I should GO to the WOODS"
  | currentLocation st == City && st `hasTask` "woods" =
      "I should GO to the WOODS"
  | otherwise =
      "I should try to LOOK around to get my bearings."

examine :: String -> GameState -> Maybe (GameState, Lines)
examine key st = case key of
  "creature"
    | currentLocation st == Ruins && any ((== "creature") . entityName) (entitiesAt Ruins st) ->
        Just
          ( markExamined "creature" st,
            [ "The creature stands tall and slender, its luminous eyes studying you with an intelligence that feels ancient.",
              "Its skin seems to shimmer faintly, and as you look closer, you realize it's communicating directly into your mind—a melodic hum that bypasses your ears.",
              "It exudes an aura of wisdom and otherworldliness, as if it holds secrets older than time itself.",
              ""
            ]
          )
  "tree"
    | currentLocation st `elem` [Tree, Ledge] ->
        Just
          ( markExamined "tree" st,
            [ "The tree stands ancient and imposing, its roots plunging into the earth like the veins of the valley itself.",
              ""
            ]
          )
  "ruins"
    | currentLocation st `elem` [Ruins, Tree] ->
        Just
          ( markExamined "ruins" st,
            [ "The ruins are a marvel of ancient architecture, reminiscent of Egypt's pyramids or the jungle temples of South America, yet distinctly alien.",
              ""
            ]
          )
  "city"
    | currentLocation st `elem` [City, Tree] ->
        Just
          ( markExamined "city" st,
            [ "The city cuts a stark silhouette against the valley's greenery, its dark gray buildings rising like monolithic sentinels.",
              ""
            ]
          )
  "tunnel"
    | currentLocation st `elem` [Tunnel, Tree] ->
        Just
          ( markExamined "tunnel" st,
            [ "The tunnel exit gapes like a dark maw, leading back to the frozen surface-a lifeline or a trap, depending on your next move.",
              ""
            ]
          )
  "radio"
    | "radio" `elem` tasks st && isInInventory "radio" st ->
        Just
          ( markExamined "radio" st,
            [ "The RADIO is a rugged military device, scratched and dented but still working.",
              "Each dial can be set to a number between 1 and 9.",
              "The dials click stiffly as you turn them. A small plaque beneath them reads: \"Standard Marine Corps Protocol: A=Even, B=Prime, C=Square.\"",
              ""
            ]
          )
  "note"
    | "radio" `elem` tasks st && isInInventory "radio" st ->
        Just
          ( markExamined "note" st,
            [ "The note is weathered, its ink blurred but readable: \"Marine Corps Frequency: Alpha-Bravo-Charlie. Remember the code: Four's the square, Seven's luck, Two's pair.\"",
              ""
            ]
          )
  _ -> Nothing

