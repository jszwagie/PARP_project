module Act2 (startAct2) where

import Data.Bool (Bool (False, True))
import Data.Char (toLower)
import Data.List (init)
import GHC.Generics (C)
import Utils
  ( Entity (..),
    EntityType (..),
    GameState (..),
    Lines,
    Location (..),
    PlayerState (..),
    emptyPlayer,
    entitiesAt,
    extractPlayerState,
    findHere,
    findVisible,
    instructionsText,
    isSupply,
    parseCommand,
    printLines,
    printPrompt,
    readCommand,
    supplyNames,
  )

act2Prolog :: Lines
act2Prolog =
  [ "ACT 2: DESCENT INTO THE UNKNOWN",
    "You are sitting in the co-pilot chair of a Douglas A-20 Havoc, soaring over",
    "Antarctica's icy expanse. Clara pilots beside you, and you hold Admiral Byrd's diary."
  ]

initialState :: GameState
initialState =
  GameState
    { currentLocation = Cockpit,
      locationEntities = initialEntities,
      inventory = [],
      examined = [],
      talked = [],
      tasks = []
    }

initialEntities :: [(Location, [Entity])]
initialEntities =
  [ ( Cockpit,
      [ Entity Item "diary" ("*It is open on the coordinates*" ++ "There must be some truth in it.") False,
        Entity Item "radio" ("*The radio has a frequency adjuster*" ++ "Maybe I could run into something interesting by switching frequencies.") False,
        Entity Person "clara" "Clara pilots beside you, focused on the controls." False
      ]
    ),
    ( CrashSite,
      [ Entity Item "plane" ("The plane's a lost cause, but the luggage COMPARTMENT is intact." ++ "The supplies you took are probably still there.") False,
        Entity Item "compartment" "The plane compartment for supplies." False
      ]
    ),
    ( Wreck,
      [ Entity Item "wreck object" "A wrecked plane" False,
        Entity Item "pistol" "A pistol, probably from the wreck." True
      ]
    )
  ]

moveSuppliesToCompartment :: PlayerState -> PlayerState
moveSuppliesToCompartment ps =
  let compartment =
        Entity
          Item
          "compartment"
          "The plane compartment for supplies."
          False
   in ps {inventory_ = compartment : inventory_ ps}

canMove :: Location -> Location -> Bool
canMove CrashSite Cave = True
canMove Cave CrashSite = True
canMove Cave Wreck = True
canMove Wreck Cave = True
canMove Cave Tunnel = True
canMove Tunnel Cave = True
canMove _ _ = False

parseLocation :: String -> Location
parseLocation s = case map toLower s of
  "cockpit" -> Cockpit
  "crashsite" -> CrashSite
  "cave" -> Cave
  "wreck" -> Wreck
  "tunnel" -> Tunnel
  _ -> Unknown

describeLocation :: Location -> String
describeLocation Cockpit = cockpitDesc
describeLocation Wreck = wreckDesc
describeLocation Unknown = "You see nothing special."

cockpitDesc, wreckDesc :: String
cockpitDesc =
  "The cockpit is tight and utilitarian, filled with glowing dials and humming switches.\n\
  \Through the windshield, Antarctica's endless snow glitters under a gray sky.\n\
  \Turbulence occasionally rocks the plane, rattling the controls.\n\
  \Before the control panel, next to you, on the pilot's seat is CLARA, bravely piloting the plane.\n\
  \On the panel sits the RADIO, and Byrd's DIARY rests on your lap."
wreckDesc =
  "The interior is cramped and dark, with control panels covered in dust and frost.\n\
  \Wires hang loosely, and a faint smell of oil lingers.\n\
  \On a seat, you spot an old German PISTOL—a Mauser C96—still holstered."

infix 4 `has`

has :: GameState -> String -> Bool
has st t = t `elem` tasks st

hasItem :: String -> GameState -> Bool
hasItem n st = any ((== map toLower n) . map toLower . entityName) (inventory st)

getHint :: GameState -> String
getHint st
  | currentLocation st == Cockpit,
    not (st `has` "radio_examined") =
      "I need to find something to pass the time."
  | currentLocation st == Cockpit,
    st `has` "radio_examined",
    not (st `has` "radio_used") =
      "I could USE the radio to pass some time."
  | currentLocation st == Cockpit,
    st `has` "radio_used",
    not (st `has` "crashed") =
      "I should talk to Clara."
  | currentLocation st == CrashSite,
    st `has` "injured_clara",
    not (st `has` "plane_examined") =
      "Clara needs help fast, and the wreckage of the PLANE might have something useful."
  | currentLocation st == CrashSite,
    st `has` "injured_clara",
    st `has` "plane_examined" =
      "Clara needs help fast, and a MEDKIT should be in the luggage COMPARTMENT."
  | currentLocation st == CrashSite,
    not (st `has` "injured_clara"),
    not (st `has` "compartment_checked") =
      "I should check the luggage COMPARTMENT for the rest of the supplies."
  | currentLocation st == CrashSite,
    not (st `has` "injured_clara") =
      "I should talk to Clara about our next move."
  | currentLocation st == Cave,
    st `has` "wreck_discovery",
    not (st `has` "wreck_examined") =
      "I should EXAMINE the WRECK."
  | currentLocation st == Cave,
    st `has` "wreck_examined",
    not (st `has` "entered_wreck"),
    not (st `has` "wreck_discovery2") =
      "I should talk to Clara."
  | currentLocation st == Cave,
    st `has` "wreck_discovery2",
    not (st `has` "entered_wreck") =
      "I must decide, should I GO to WRECK or GO DEEPER?"
  | currentLocation st == Cave =
      "I should talk to Clara."
  | currentLocation st == Wreck,
    not (hasItem "pistol" st) =
      "I should look around for anything useful."
  | currentLocation st == Wreck =
      "I should talk to Clara."
  | otherwise =
      "I should try to LOOK around to get my bearings."

startAct2 :: PlayerState -> IO PlayerState
startAct2 ps0 = do
  let ps = moveSuppliesToCompartment ps0
  printLines act2Prolog
  pure ps