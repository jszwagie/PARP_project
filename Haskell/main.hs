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
parseLocation str = case str of
    "barrack" -> Barrack
    "yard" -> Yard
    "runway" -> Runway
    "depot" -> Depot
    "tent" -> Tent
    _ -> Unknown

printLines :: [String] -> IO ()
printLines xs = putStr (unlines xs)

printIntroduction :: IO ()
printIntroduction = printLines introductionText

printInstructions :: IO ()
printInstructions = printLines instructionsText

readCommand :: IO String
readCommand = do
    putStr "> "
    hFlush stdout
    getLine

gameLoop :: GameState -> IO ()
gameLoop state = do
    cmd <- readCommand
    case words cmd of
        ["instructions"] -> do
            printInstructions
            gameLoop state
        ["quit"] -> return ()
        ["look"] -> do
            printLines [describeLocation (currentLocation state), ""]
            gameLoop state
        ["go", place] -> do
            let location = parseLocation place
            if location == Unknown
                then do
                    printLines ["Unknown place: " ++ place, ""]
                    gameLoop state
            else if canMove (currentLocation state) location
                then do
                    printLines [describeLocation location, ""]
                    gameLoop (state { currentLocation = location })
                else do
                    printLines ["You can't go to " ++ place ++ " from here.", ""]
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