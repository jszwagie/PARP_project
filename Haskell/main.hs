module Main where

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

-- Entity represents either an item or a person
data EntityType = Item | Person deriving (Eq, Show)

data Entity = Entity
    { entityType        :: EntityType
    , entityName        :: String
    , entityDescription :: String
    , canTake           :: Bool
    }
    deriving (Eq, Show)

data GameState = GameState
    { currentLocation  :: Location
    , locationEntities :: [(Location, [Entity])]
    , inventory        :: [Entity]
    , examined         :: [String]
    , talked           :: [String]
    , tasks            :: [String]
    }
    deriving (Show)

-- Define the entities in the game worldinitialEntities :: [(Location, [Entity])]
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

initialState :: GameState
initialState = GameState
    { currentLocation  = Yard
    , locationEntities = initialEntities
    , inventory        = []
    , examined         = []
    , talked           = []
    , tasks            = []
    }

-- Define which locations connect to each other
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

-- Description for each location
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

parseLocation :: String -> Location
parseLocation str = case map toLower str of
    "barrack" -> Barrack
    "yard"    -> Yard
    "runway"  -> Runway
    "depot"   -> Depot
    "tent"    -> Tent
    _          -> Unknown

-- Generate a hint based on game state
getHint :: GameState -> String
getHint state
  | currentLocation state == Barrack
    && not (any ((=="lighter") . entityName) (inventory state))
      = "I should gather something useful."
  | "clara_fuel_request" `notElem` tasks state
      = "I think I should talk to Clara."
  | "fuel_request" `elem` tasks state
      = "I should check the fuel TANKS."
  | otherwise
      = "Try exploring more or talking to Clara."

findEntity :: String -> GameState -> Maybe Entity
findEntity nm st =
    let locEs = fromMaybe [] (lookup (currentLocation st) (locationEntities st))
        allEs = locEs ++ inventory st
    in find (\e -> map toLower (entityName e) == map toLower nm) allEs

-- Check if an entity is in the player's inventory
isInInventory :: String -> GameState -> Bool
isInInventory nm st = any ((== map toLower nm) . map toLower . entityName) (inventory st)

-- Add an entity to inventory and remove from current location
addToInventory :: Entity -> GameState -> GameState
addToInventory ent st =
    let loc = currentLocation st
        removeHere = filter ((/= entityName ent) . entityName)
            (fromMaybe [] (lookup loc (locationEntities st)))
        newLocs = map (\(l,es) -> if l==loc then (l,removeHere) else (l,es))
            (locationEntities st)
    in st { inventory = ent : inventory st, locationEntities = newLocs }

-- Remove an entity from inventory and add to current location
removeFromInventory :: Entity -> GameState -> GameState
removeFromInventory ent st =
    let loc = currentLocation st
        inv' = filter ((/= entityName ent) . entityName) (inventory st)
        locEs = fromMaybe [] (lookup loc (locationEntities st))
        newLocs = map (\(l,es) -> if l==loc then (l,ent:locEs) else (l,es))
            (locationEntities st)
    in st { inventory = inv', locationEntities = newLocs }

-- Mark an entity as examined
markExamined :: String -> GameState -> GameState
markExamined nm st = st { examined = nm : examined st }

-- Mark that we've talked to someone
markTalked :: String -> String -> GameState -> GameState
markTalked person topic st = st { talked = (person ++ "_" ++ topic) : talked st }

-- Add a task to the state
addTask :: String -> GameState -> GameState
addTask t st = st { tasks = t : tasks st }

-- Remove a task from the state
removeTask :: String -> GameState -> GameState
removeTask task state = state{tasks = filter (/= task) (tasks state)}

-- Commands
data Command
    = CmdLook | CmdQuit | CmdInventory | CmdHint | CmdInstructions
    | CmdGo String | CmdExamine String | CmdTalk String
    | CmdTake String | CmdDrop String | CmdUnknown
    deriving (Eq, Show)

type Args = [String]

parseCommand :: Args -> Command
parseCommand ["look"]         = CmdLook
parseCommand ["quit"]         = CmdQuit
parseCommand ["inventory"]    = CmdInventory
parseCommand ["hint"]         = CmdHint
parseCommand ["instructions"] = CmdInstructions
parseCommand ["talk",p]       = CmdTalk p
parseCommand ["take",o]       = CmdTake o
parseCommand ["drop",o]       = CmdDrop o
parseCommand ("go":p:_)       = CmdGo p
parseCommand ("examine":x:_)  = CmdExamine x
parseCommand _                = CmdUnknown


type StepResult = (GameState, Lines)

step :: GameState -> Command -> IO StepResult
step st CmdQuit = return (st, ["Goodbye."])
step st CmdLook = return (st, [describeLocation (currentLocation st), ""])
step st CmdInventory =
    let inv = inventory st
        out = if null inv
              then ["You are not carrying anything.", ""]
              else "You are carrying:": map entityName inv ++ [""]
    in return (st, out)
step st CmdHint = return (st, [getHint st, ""])
step st CmdInstructions = return (st, instructionsText)
step st (CmdGo place) =
    let loc = parseLocation place in
    if loc == Unknown
      then return (st, ["Unknown place: " ++ place, ""])
      else if canMove (currentLocation st) loc
        then return (st { currentLocation = loc }, [describeLocation loc, ""])
        else return (st, ["You can't go to " ++ place ++ " from here.", ""])
step st (CmdExamine thing) =
    case findEntity thing st of
      Just ent ->
        let desc = entityDescription ent
            st'  = markExamined (entityName ent) st
        in return (st', [desc, ""])
      Nothing -> return (st, ["I can't see " ++ thing ++ " here or there's nothing special about it.", ""])
step st (CmdTalk person) =
    -- dialog with Clara handled in gameLoop
    return (st, [])
step st (CmdTake obj) =
    case findEntity obj st of
      Just ent | entityType ent == Person ->
        return (st, ["You can't take " ++ obj ++ "!", ""])
      Just ent | canTake ent ->
        if isInInventory (entityName ent) st
          then return (st, ["You're already holding it!", ""])
          else return (addToInventory ent st, ["You take the " ++ entityName ent ++ ".", ""])
      Just _ -> return (st, ["You can't take that.", ""])
      Nothing -> return (st, ["I don't see " ++ obj ++ " here.", ""])
step st (CmdDrop obj) =
    if isInInventory obj st
      then case findEntity obj st of
        Just ent -> return (removeFromInventory ent st, ["You drop the " ++ entityName ent ++ ".", ""])
        _        -> return (st, ["Error: Item in inventory but not found.", ""])
      else return (st, ["You aren't carrying that!", ""])
step st CmdUnknown = return (st, ["Unknown command.", ""])


pureDialogWithClara :: GameState -> (GameState, Lines)
pureDialogWithClara st
    | currentLocation st /= Runway =
        let st' = addTask "go_to_clara" st
            st'' = st'  -- no clara_cockpit here yet
        in (st'', ["You're not close enough to talk to Clara. She's at the RUNWAY."])
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
    | otherwise =
        ( st
        , ["Clara is busy working on the plane."]
        )

pureClaraChoice :: String -> GameState -> (GameState, Lines)
pureClaraChoice choice st
    | choice == "1" =
        let base       = removeTask "awaiting_clara_choice" st
            withFuel   = addTask "fuel_request" base
            withCookpit= markTalked "clara" "cockpit" withFuel
        in ( withCookpit
           , ["You: \"Okay, I'll handle it now.\""]
           )
    | choice == "2" =
        let base       = removeTask "awaiting_clara_choice" st
            withFuel   = addTask "fuel_request" base
            withCookpit= markTalked "clara" "cockpit" withFuel
        in ( withCookpit
           , [ "You: \"I think it's good enough, but I could double-check.\""
             , "Clara: \"Good enough doesn't cut it out here. Antarctica doesn't forgive mistakes. Check it properly.\""
             ]
           )
    | choice == "3" =
        let base       = removeTask "awaiting_clara_choice" st
            withSupp   = addTask "supplies" base
            withCookpit= markTalked "clara" "cockpit" withSupp
        in ( withCookpit
           , [ "You: \"Why don't you take care of it?\""
             , "Clara: (frowning) \"Oh, you're lazy, aren't you? Fine, I'll handle it after I finish checking the oil, but you're not off the hook, doc. Go gather mandatory supplies and drop them near the plane.\""
             ]
           )
    | otherwise =
        ( st
        , ["Invalid choice. Please enter 1, 2, or 3."]
        )

printLines :: Lines -> IO ()
printLines = mapM_ putStrLn

printStr :: String -> IO ()
printStr s = putStr s >> hFlush stdout

readCommand :: IO String
readCommand = do
    printStr "> "
    getLine

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
          CmdTalk person | person == "clara" ->
              return (pureDialogWithClara st)
          _ ->
              step st cmd

    printLines out

    case cmd of
      CmdQuit -> return ()
      _       -> gameLoop st'

main :: IO ()
main = do
    printLines introductionText
    printLines instructionsText
    printLines [describeLocation Yard, ""]
    gameLoop initialState
