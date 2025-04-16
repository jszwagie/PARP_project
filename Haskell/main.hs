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

-- Entity represents either an item or a person
data EntityType = Item | Person deriving (Eq, Show)
data Entity = Entity
    { entityType :: EntityType
    , entityName :: String
    , entityDescription :: String
    , canTake :: Bool -- Whether the entity can be picked up
    }
    deriving (Eq, Show)

-- For dialog options
data DialogOption = DialogOption
    { optionText :: String
    , optionResponse :: String
    , optionEffect :: GameState -> GameState
    }

data GameState = GameState
    { currentLocation :: Location
    , locationEntities :: [(Location, [Entity])]
    , inventory :: [Entity]
    , examined :: [String] -- Names of things that have been examined
    , talked :: [String] -- Names of people that have been talked to
    , tasks :: [String] -- Current tasks/quests
    }
    deriving (Show)

initialState :: GameState
initialState =
    GameState
        { currentLocation = Yard
        , locationEntities = initialEntities
        , inventory = []
        , examined = []
        , talked = []
        , tasks = []
        }

-- Define the entities in the game world
initialEntities :: [(Location, [Entity])]
initialEntities =
    [ (Yard, [])
    ,
        ( Barrack
        ,
            [ Entity Item "photo" "A photo of your late wife sits on the dresser." False
            , Entity Item "lighter" "A simple silver lighter. You should really quit smoking." True
            , Entity Item "calendar" "August 26, 1946" False
            ]
        )
    ,
        ( Runway
        ,
            [ Entity Person "clara" "Clara stands near the plane, wearing a military pilot's uniform with rolled-up sleeves." False
            , Entity Item "plane" "Your type served well in the war." False
            , Entity Item "tanks" "Fuel tanks for the plane. They're running low." False
            ]
        )
    ,
        ( Depot
        ,
            [ Entity Item "canister" "A heavy fuel canister. Necessary for the journey." True
            ]
        )
    ,
        ( Tent
        ,
            [ Entity Item "food" "Canned goods and dried meals." True
            , Entity Item "water" "Fresh water in sealed containers." True
            , Entity Item "geiger" "A standard radiation detector." True
            , Entity Item "medkit" "Bandages, antiseptic, morphine..." True
            , Entity Item "radio" "A shortwave field radio." True
            , Entity Item "gear" "Ropes, pitons, carabiners." True
            , Entity Item "tools" "A compass, maps, and a sextant." True
            , Entity Item "list" "A supply list showing available items to take." False
            ]
        )
    ]

-- Define which locations connect to each other
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
    "The sunlight, reflected off the steel plates, blinds you as you approach the aircraft - \n"
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

parseLocation :: String -> Location
parseLocation str = case map toLower str of
    "barrack" -> Barrack
    "yard" -> Yard
    "runway" -> Runway
    "depot" -> Depot
    "tent" -> Tent
    _ -> Unknown

-- Generate a hint based on game state
getHint :: GameState -> String
getHint state
    | currentLocation state == Barrack && not (any (\e -> entityName e == "lighter" && canTake e) (inventory state)) =
        "I should gather something useful."
    | "clara_fuel_request" `notElem` talked state =
        "I think I should talk with Clara."
    | "fuel_request" `elem` tasks state =
        "I should check the fuel TANKS."
    | otherwise =
        "Try exploring more or talking to Clara."

-- Find an entity at a location or in inventory
findEntity :: String -> GameState -> Maybe Entity
findEntity name state =
    let
        locEntities =
            Data.Maybe.fromMaybe
                []
                (lookup (currentLocation state) (locationEntities state))
        allAccessibleEntities = locEntities ++ inventory state
     in
        find (\e -> map toLower (entityName e) == map toLower name) allAccessibleEntities

-- Check if an entity is in the player's inventory
isInInventory :: String -> GameState -> Bool
isInInventory name state = any (\e -> map toLower (entityName e) == map toLower name) (inventory state)

-- Add an entity to inventory and remove from current location
addToInventory :: Entity -> GameState -> GameState
addToInventory entity state =
    let
        loc = currentLocation state
        locEntities = fromMaybe [] (lookup loc (locationEntities state))
        updatedLocEntities = filter (\e -> entityName e /= entityName entity) locEntities
        updatedLocationEntities =
            map
                ( \(l, es) ->
                    if l == loc
                        then (l, updatedLocEntities)
                        else (l, es)
                )
                (locationEntities state)
     in
        state
            { inventory = entity : inventory state
            , locationEntities = updatedLocationEntities
            }

-- Remove an entity from inventory and add to current location
removeFromInventory :: Entity -> GameState -> GameState
removeFromInventory entity state =
    let
        loc = currentLocation state
        locEntities = fromMaybe [] (lookup loc (locationEntities state))
        updatedInventory = filter (\e -> entityName e /= entityName entity) (inventory state)
        updatedLocEntities = entity : locEntities
        updatedLocationEntities =
            map
                ( \(l, es) ->
                    if l == loc
                        then (l, updatedLocEntities)
                        else (l, es)
                )
                (locationEntities state)
     in
        state
            { inventory = updatedInventory
            , locationEntities = updatedLocationEntities
            }

-- Mark an entity as examined
markExamined :: String -> GameState -> GameState
markExamined name state = state{examined = name : examined state}

-- Mark that we've talked to someone
markTalked :: String -> String -> GameState -> GameState
markTalked person topic state = state{talked = (person ++ "_" ++ topic) : talked state}

-- Add a task to the state
addTask :: String -> GameState -> GameState
addTask task state = state{tasks = task : tasks state}

-- Remove a task from the state
removeTask :: String -> GameState -> GameState
removeTask task state = state{tasks = filter (/= task) (tasks state)}

-- Handle dialog with Clara at runway
dialogWithClara :: GameState -> IO GameState
dialogWithClara state
    | currentLocation state /= Runway =
        ( \s -> do
            printLines ["You're not close enough to talk to Clara. She's at the RUNWAY."]
            return s
        )
            state{tasks = ["go_to_clara"]}
    | "clara_cockpit" `elem` talked state =
        ( \s -> do
            printLines
                [ "Clara: \"We're bound to find something there—I can feel it in my bones.\""
                , "You: \"Hopefully, or all our efforts will be for nothing.\""
                ]
            return s
        )
            state
    | "clara_fuel_request" `notElem` talked state =
        let
            promptForChoice s = do
                printStr "> "
                choice <- getLine
                case choice of
                    "1" -> do
                        printLines ["You: \"Okay, I'll handle it now.\""]
                        return (addTask "fuel_request" s)
                    "2" -> do
                        printLines
                            [ "You: \"I think it's good enough, but I could double-check.\""
                            , "Clara: \"Good enough doesn't cut it out here. Antarctica doesn't forgive mistakes. Check it properly.\""
                            ]
                        return (addTask "fuel_request" s)
                    "3" -> do
                        printLines
                            [ "You: \"Why don't you take care of it?\""
                            , "Clara: (frowning) \"Oh, you're lazy, aren't you? Fine, I'll handle it after I finish checking the oil,"
                            , "but you're not off the hook, doc. Go gather mandatory supplies and drop them near the plane.\""
                            ]
                        return (addTask "supplies" s)
                    _ -> do
                        printLines ["Invalid choice. Please enter 1, 2, or 3."]
                        promptForChoice s
         in
            ( \s -> do
                printLines
                    [ "Clara: \"Morning, doc. We need to get this bird fueled up. Could you check the TANKS for me?\""
                    , "Your choices:"
                    , "1. \"Okay, I'll handle it now.\""
                    , "2. \"I think it's good enough, but I could double-check.\""
                    , "3. \"Why don't you take care of it?\""
                    ]
                promptForChoice s
            )
                (markTalked "clara" "fuel_request" state)
    | otherwise =
        ( \s -> do
            printLines ["Clara is busy working on the plane."]
            return s
        )
            state

printLines :: [String] -> IO ()
printLines xs = putStr (unlines xs)

printStr :: String -> IO ()
printStr s = do
    putStr s
    hFlush stdout

printIntroduction :: IO ()
printIntroduction = printLines introductionText

printInstructions :: IO ()
printInstructions = printLines instructionsText

readCommand :: IO String
readCommand = do
    printStr "> "
    getLine

gameLoop :: GameState -> IO ()
gameLoop state = do
    cmd <- readCommand
    case words (map toLower cmd) of
        ["instructions"] -> do
            printInstructions
            gameLoop state
        ["quit"] -> return ()
        ["look"] -> do
            printLines [describeLocation (currentLocation state), ""]
            gameLoop state
        ["inventory"] -> do
            if null (inventory state)
                then printLines ["You are not carrying anything."]
                else do
                    printLines ["You are carrying:"]
                    printLines (map entityName (inventory state))
            gameLoop state
        ["hint"] -> do
            printLines [getHint state, ""]
            gameLoop state
        ["go", place] -> do
            let location = parseLocation place
            if location == Unknown
                then do
                    printLines ["Unknown place: " ++ place, ""]
                    gameLoop state
                else
                    if canMove (currentLocation state) location
                        then do
                            printLines [describeLocation location, ""]
                            gameLoop (state{currentLocation = location})
                        else do
                            printLines ["You can't go to " ++ place ++ " from here.", ""]
                            gameLoop state
        ["examine", thing] -> do
            case findEntity thing state of
                Just entity -> do
                    printLines [entityDescription entity, ""]
                    gameLoop (markExamined (entityName entity) state)
                Nothing -> do
                    printLines ["I can't see " ++ thing ++ " here or there's nothing special about it.", ""]
                    gameLoop state
        ["talk", person] -> do
            case findEntity person state of
                Just entity | entityType entity == Person -> do
                    if entityName entity == "clara"
                        then do
                            newState <- dialogWithClara state
                            gameLoop newState
                        else do
                            printLines ["There's not much to discuss with " ++ person ++ ".", ""]
                            gameLoop state
                _ -> do
                    printLines ["There's no one here to talk to by that name.", ""]
                    gameLoop state
        ["take", obj] -> do
            case findEntity obj state of
                Just entity | entityType entity == Person -> do
                    printLines ["You can't take " ++ obj ++ "!", ""]
                    gameLoop state
                Just entity | canTake entity -> do
                    if isInInventory (entityName entity) state
                        then do
                            printLines ["You're already holding it!", ""]
                            gameLoop state
                        else do
                            printLines ["You take the " ++ entityName entity ++ ".", ""]
                            gameLoop (addToInventory entity state)
                Just entity -> do
                    printLines ["You can't take that.", ""]
                    gameLoop state
                Nothing -> do
                    printLines ["I don't see " ++ obj ++ " here.", ""]
                    gameLoop state
        ["drop", obj] -> do
            if isInInventory obj state
                then case findEntity obj state of
                    Just entity -> do
                        printLines ["You drop the " ++ entityName entity ++ ".", ""]
                        gameLoop (removeFromInventory entity state)
                    Nothing -> do
                        printLines ["Error: Item in inventory but not found.", ""]
                        gameLoop state
                else do
                    printLines ["You aren't carrying that!", ""]
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
