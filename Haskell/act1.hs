module Act1
  ( GameState
  , initialState
  , step
  , gameLoop
  , introductionText
  , instructionsText
  , printLines
  , describeLocation
  , currentLocation
  ) where

import Data.Char (toLower)
import Data.List (find)
import Data.Maybe (fromMaybe)
import System.IO (hFlush, stdout)


type Lines = [String]

introductionText :: Lines
introductionText =
    [ "Welcome to The Hidden Realm"
    , "An interactive fiction adventure set in the icy depths of Antarctica."
    , "Unravel ancient secrets, face modern dangers, and shape your fate through your choices."
    , ""
    ]

instructionsText :: Lines
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


data Location = Barrack | Yard | Runway | Depot | Tent | Unknown
  deriving (Eq, Show)

data EntityType = Item | Person deriving (Eq, Show)

data Entity = Entity
    { entityType        :: EntityType
    , entityName        :: String
    , entityDescription :: String
    , canTake           :: Bool
    } deriving (Eq, Show)


data GameState = GameState
    { currentLocation  :: Location
    , locationEntities :: [(Location, [Entity])]
    , inventory        :: [Entity]
    , examined         :: [String]
    , talked           :: [String]
    , tasks            :: [String]
    } deriving (Show)


initialState :: GameState
initialState = GameState
    { currentLocation  = Yard
    , locationEntities = initialEntities
    , inventory        = []
    , examined         = []
    , talked           = []
    , tasks            = []
    }

initialEntities :: [(Location, [Entity])]
initialEntities =
    [ (Yard, [])
    , (Barrack,
        [ Entity Item "photo"   "A photo of your late wife sits on the dresser." False
        , Entity Item "lighter" "A simple silver lighter. You should really quit smoking." True
        , Entity Item "calendar" "August 26, 1946" False
        ])
    , (Runway,
        [ Entity Person "clara" "Clara stands near the plane, wearing a military pilot's uniform with rolled-up sleeves." False
        , Entity Item   "plane"  "Your type served well in the war." False
        , Entity Item   "tanks"  "Fuel tanks for the plane. They're running low." False
        ])
    , (Depot, [ Entity Item "canister" "A heavy fuel canister. Necessary for the journey." True ])
    , (Tent,
        [ Entity Item "food"   "Canned goods and dried meals." True
        , Entity Item "water"  "Fresh water in sealed containers." True
        , Entity Item "geiger" "A standard radiation detector." True
        , Entity Item "medkit" "Bandages, antiseptic, morphine..." True
        , Entity Item "radio"  "A shortwave field radio." True
        , Entity Item "gear"   "Ropes, pitons, carabiners." True
        , Entity Item "tools"  "A compass, maps, and a sextant." True
        , Entity Item "list"   "A supply list showing available items to take." False
        ])
    ]

-- Commands

data Command
    = CmdLook | CmdQuit | CmdInventory | CmdHint | CmdInstructions
    | CmdGo String | CmdExamine String | CmdTalk String
    | CmdTake String | CmdDrop String | CmdUnknown
    deriving (Eq, Show)

type Args = [String]

-- Command parsing

parseCommand :: Args -> Command
parseCommand ["look"]         = CmdLook
parseCommand ["quit"]         = CmdQuit
parseCommand ["inventory"]    = CmdInventory
parseCommand ["hint"]         = CmdHint
parseCommand ["instructions"] = CmdInstructions
parseCommand ["talk", p]      = CmdTalk p
parseCommand ["take", o]      = CmdTake o
parseCommand ["drop", o]      = CmdDrop o
parseCommand ("go":p:_)       = CmdGo p
parseCommand ("examine":x:_)  = CmdExamine x
parseCommand _                 = CmdUnknown


canMove :: Location -> Location -> Bool
canMove Yard    Barrack = True
canMove Yard    Runway  = True
canMove Yard    Depot   = True
canMove Yard    Tent    = True
canMove Barrack Yard    = True
canMove Runway  Yard    = True
canMove Depot   Yard    = True
canMove Tent    Yard    = True
canMove _       _       = False

parseLocation :: String -> Location
parseLocation s = case map toLower s of
  "barrack" -> Barrack
  "yard"    -> Yard
  "runway"  -> Runway
  "depot"   -> Depot
  "tent"    -> Tent
  _           -> Unknown

-- Descriptions of locations

describeLocation :: Location -> String
describeLocation Yard =
    "You're on the BARRACK yard. Nearby, a sturdy twin-engine plane rests\n"
    ++ "on a makeshift RUNWAY, its metal hull glinting faintly in the weak sunlight.\n"
    ++ "To the side, there's a fuel DEPOT and a supply TENT. The air is frigid,\n"
    ++ "the wind howls intermittently, and the isolation weighs heavily."
describeLocation Barrack =
    "This is your resting place during the mission - small but convenient.\n"
    ++ "Your bed is neatly made, and a PHOTO of your late wife sits on the dresser beside it.\n"
    ++ "Across the room, your working desk holds mission documents, a small lamp, and a LIGHTER.\n"
    ++ "A CALENDAR hangs above the desk.\n"
    ++ "Outside - the YARD, covered in snow."
describeLocation Runway =
    "The sunlight, reflected off the steel plates, blinds you as you approach the aircraft -\n"
    ++ "a Douglas A-20 Havoc. It's not the newest PLANE, but it's reliable.\n"
    ++ "CLARA is tinkering with one of the engines.\n"
    ++ "There are fuel TANKS that need to be checked.\n"
    ++ "Behind you - the YARD, covered in snow."
describeLocation Depot =
    "You step into the depot, a rough but functional structure shielding fuel CANISTERs from the Antarctic cold.\n"
    ++ "Outside - the YARD, covered in snow."
describeLocation Tent =
    "You enter the supply tent, a cramped space cluttered with gear.\n"
    ++ "Boxes and crates are labeled with essentials: FOOD, WATER, scientific tools, and survival equipment.\n"
    ++ "A LIST of stock hangs on the wall.\n"
    ++ "You see a GEIGER counter, MEDKIT, RADIO, climbing GEAR, and navigation TOOLS.\n"
    ++ "Outside - the YARD, covered in snow."
describeLocation Unknown = "You see nothing special."

-- Hint system

getHint :: GameState -> String
getHint st
  | currentLocation st == Barrack
    && not (any ((== "lighter") . entityName) (inventory st))
      = "I should gather something useful."
  | "clara_fuel_request" `notElem` tasks st
      = "I think I should talk to Clara."
  | "fuel_request" `elem` tasks st
      = "I should check the fuel TANKS."
  | otherwise
      = "Try exploring more or talking to Clara."


findEntity :: String -> GameState -> Maybe Entity
findEntity nm st =
  let locEs = fromMaybe [] (lookup (currentLocation st) (locationEntities st))
      allEs = locEs ++ inventory st
  in find (\e -> map toLower (entityName e) == map toLower nm) allEs

isInInventory :: String -> GameState -> Bool
isInInventory nm st = any ((== map toLower nm) . map toLower . entityName) (inventory st)

addToInventory :: Entity -> GameState -> GameState
addToInventory ent st =
  let loc      = currentLocation st
      removeEs = filter ((/= entityName ent) . entityName) (fromMaybe [] (lookup loc (locationEntities st)))
      newLocs  = map (\(l,es) -> if l==loc then (l,removeEs) else (l,es)) (locationEntities st)
  in st { inventory = ent:inventory st, locationEntities = newLocs }

removeFromInventory :: Entity -> GameState -> GameState
removeFromInventory ent st =
  let loc     = currentLocation st
      inv'    = filter ((/= entityName ent) . entityName) (inventory st)
      locEs   = fromMaybe [] (lookup loc (locationEntities st))
      newLocs = map (\(l,es) -> if l==loc then (l,ent:locEs) else (l,es)) (locationEntities st)
  in st { inventory = inv', locationEntities = newLocs }

markExamined :: String -> GameState -> GameState
markExamined nm st = st { examined = nm:examined st }

markTalked :: String -> String -> GameState -> GameState
markTalked p t st = st { talked = (p++"_"++t):talked st }

addTask :: String -> GameState -> GameState
addTask t st = st { tasks = t:tasks st }

removeTask :: String -> GameState -> GameState
removeTask t st = st { tasks = filter (/=t) (tasks st) }

-- Dialog with Clara

pureDialogWithClara :: GameState -> (GameState, Lines)
pureDialogWithClara st
    | currentLocation st /= Runway =
        let st'  = addTask "go_to_clara" st in
        (st', ["You're not close enough to talk to Clara. She's at the RUNWAY."])
    | "clara_cockpit" `elem` talked st =
        ( st
        , [ "Clara: \"We're bound to find something there—I can feel it in my bones.\""
          , "You: \"Hopefully, or all our efforts will be for nothing.\""
          ]
        )
    | "clara_fuel_request" `notElem` talked st =
        let st1 = markTalked "clara" "fuel_request" st
            st2 = addTask "awaiting_clara_choice" st1
        in ( st2
           , [ "Clara: \"Morning, doc. We need to get this bird fueled up. Could you check the TANKS for me?\""
             , "Your choices:"
             , "1. \"Okay, I'll handle it now.\""
             , "2. \"I think it's good enough, but I could double-check.\""
             , "3. \"Why don't you take care of it?\""
             ]
           )
    | otherwise = (st, ["Clara is busy working on the plane."])

pureClaraChoice :: String -> GameState -> (GameState, Lines)
pureClaraChoice choice st
    | choice == "1" =
        let base        = removeTask "awaiting_clara_choice" st
            withFuel    = addTask "fuel_request" base
            withCockpit = markTalked "clara" "cockpit" withFuel
        in ( withCockpit, ["You: \"Okay, I'll handle it now.\""] )
    | choice == "2" =
        let base        = removeTask "awaiting_clara_choice" st
            withFuel    = addTask "fuel_request" base
            withCockpit = markTalked "clara" "cockpit" withFuel
        in ( withCockpit
           , [ "You: \"I think it's good enough, but I could double-check.\""
             , "Clara: \"Good enough doesn't cut it out here. Antarctica doesn't forgive mistakes. Check it properly.\""
             ]
           )
    | choice == "3" =
        let base        = removeTask "awaiting_clara_choice" st
            withSupp    = addTask "supplies" base
            withCockpit = markTalked "clara" "cockpit" withSupp
        in ( withCockpit
           , [ "You: \"Why don't you take care of it?\""
             , "Clara: (frowning) \"Oh, you're lazy, aren't you? Fine, I'll handle it after I finish checking the oil, but you're not off the hook, doc. Go gather mandatory supplies and drop them near the plane.\""
             ]
           )
    | otherwise = (st, ["Invalid choice. Please enter 1, 2, or 3."])


step :: GameState -> Command -> IO (GameState, Lines)
step st CmdQuit        = return (st, ["Goodbye."])
step st CmdLook        = return (st, [describeLocation (currentLocation st), ""])
step st CmdInventory   =
    let inv = inventory st
        out = if null inv
              then ["You are not carrying anything.", ""]
              else "You are carrying:": map entityName inv ++ [""]
    in return (st, out)
step st CmdHint        = return (st, [getHint st, ""])
step st CmdInstructions= return (st, instructionsText)
step st (CmdGo p)      =
    let loc = parseLocation p in
    case loc of
      Unknown -> return (st, ["Unknown place: " ++ p, ""])
      _       -> if canMove (currentLocation st) loc
                 then return (st { currentLocation = loc }, [describeLocation loc, ""])                
                 else return (st, ["You can't go to " ++ p ++ " from here.", ""])
step st (CmdExamine x) = case findEntity x st of
                           Just e  -> let st' = markExamined (entityName e) st
                                      in return (st', [entityDescription e, ""])
                           Nothing -> return (st, ["I can't see " ++ x ++ " here or there's nothing special about it.", ""])
step st (CmdTalk who)  = return (st, []) -- handled in gameLoop
step st (CmdTake o)    = case findEntity o st of
                           Just e | entityType e == Person -> return (st, ["You can't take " ++ o ++ "!", ""])
                                  | canTake e && not (isInInventory o st) -> return (addToInventory e st, ["You take the " ++ entityName e ++ ".", ""])
                                  | canTake e -> return (st, ["You're already holding it!", ""])
                                  | otherwise -> return (st, ["You can't take that.", ""])
                           Nothing -> return (st, ["I don't see " ++ o ++ " here.", ""])
step st (CmdDrop o)    = if isInInventory o st
                           then let Just e = findEntity o st
                                in return (removeFromInventory e st, ["You drop the " ++ entityName e ++ ".", ""])
                           else return (st, ["You aren't carrying that!", ""])
step st CmdUnknown     = return (st, ["Unknown command.", ""])


printPrompt :: IO ()
printPrompt = putStr "> " >> hFlush stdout

readCommand :: IO String
readCommand = printPrompt >> getLine

printLines :: Lines -> IO ()
printLines = mapM_ putStrLn


gameLoop :: GameState -> IO ()
gameLoop st = do
    line <- readCommand
    let cmdLine = map toLower line
        args    = words cmdLine
        cmd     = parseCommand args

    (st', out) <-
      if "awaiting_clara_choice" `elem` tasks st
      then return (pureClaraChoice cmdLine st)
      else case cmd of
          CmdTalk p | p == "clara" -> return (pureDialogWithClara st)
          _                          -> step st cmd

    printLines out

    case cmd of
      CmdQuit -> return ()
      _       -> gameLoop st'
