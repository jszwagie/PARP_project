module Main where

import Act1 (startAct1)
import Act2 (startAct2)
import Act3 (startAct3)
import GHC.Unicode (toLower)
import Utils (Command (CmdAct2, CmdAct3, CmdNext), Entity (..), EntityType (..), Lines, PlayerState (..), emptyPlayer, parseCommand, printLines, printPrompt)

introductionText :: Lines
introductionText =
  [ "INTRODUCTION:",
    "Welcome to \"The Hidden Realm\", an interactive fiction adventure",
    "set in the icy depths of Antarctica. Unravel ancient secrets,",
    "face modern dangers, and shape your fate through your choices.",
    "Don't be afraid to use HINTS - they will help you navigate",
    "challenges and uncover the truth. Stay sharp - the unknown awaits.",
    "",
    "Type \"start\" to play from Act 1, or \"act2\" / \"act3\" to start from later acts.",
    ""
  ]

act2SuppliesList :: Lines
act2SuppliesList =
  [ "You need to select supplies for your journey. The plane has capacity for 5 items:",
    "1. FOOD rations",
    "2. WATER",
    "3. GEIGER Counter",
    "4. MEDKIT",
    "5. RADIO",
    "6. Climbing GEAR",
    "7. Navigation TOOLS",
    "Enter the numbers of 5 items you want to take (e.g., '1 2 3 4 5'):",
    ""
  ]

act3SuppliesList :: Lines
act3SuppliesList =
  [ "You need to select supplies for your journey. The plane has capacity for 5 items:",
    "1. FOOD rations",
    "2. WATER",
    "3. GEIGER Counter",
    "4. RADIO",
    "5. Climbing GEAR",
    "6. Navigation TOOLS",
    "7. PISTOL",
    "Enter the numbers of 5 items you want to take (e.g., '1 2 3 4 5'):",
    ""
  ]

mapSelectionsToEntitiesAct2 :: [Int] -> [Entity]
mapSelectionsToEntitiesAct2 = map getItemEntity
  where
    getItemEntity 1 = Entity Item "food" "Canned goods and dried meals." True
    getItemEntity 2 = Entity Item "water" "Fresh water in sealed containers." True
    getItemEntity 3 = Entity Item "geiger" "A standard radiation detector." True
    getItemEntity 4 = Entity Item "medkit" "Bandages, antiseptic, morphine…" True
    getItemEntity 5 = Entity Item "radio" "A shortwave field radio." True
    getItemEntity 6 = Entity Item "gear" "Ropes, pitons, carabiners." True
    getItemEntity 7 = Entity Item "tools" "A compass, maps, and a sextant." True
    getItemEntity _ = Entity Item "food" "Canned goods and dried meals." True -- Domyślnie dajemy jedzenie

mapSelectionsToEntitiesAct3 :: [Int] -> [Entity]
mapSelectionsToEntitiesAct3 = map getItemEntity
  where
    getItemEntity 1 = Entity Item "food" "Canned goods and dried meals." True
    getItemEntity 2 = Entity Item "water" "Fresh water in sealed containers." True
    getItemEntity 3 = Entity Item "geiger" "A standard radiation detector." True
    getItemEntity 4 = Entity Item "radio" "A shortwave field radio." True
    getItemEntity 5 = Entity Item "gear" "Ropes, pitons, carabiners." True
    getItemEntity 6 = Entity Item "tools" "A compass, maps, and a sextant." True
    getItemEntity 7 = Entity Item "pistol" "An old German Mauser C96 pistol." True
    getItemEntity _ = Entity Item "food" "Canned goods and dried meals." True -- Domyślnie dajemy jedzenie

getSuppliesSelection :: Lines -> ([Int] -> [Entity]) -> IO [Entity]
getSuppliesSelection supplyList mapFunc = do
  printLines supplyList
  printPrompt
  input <- getLine
  let selections = map read (words input) :: [Int]
      validSelections = take 5 $ filter (\n -> n >= 1 && n <= 7) selections
  if length validSelections < 5
    then do
      printLines ["Please select exactly 5 valid items (numbers 1-7)."]
      getSuppliesSelection supplyList mapFunc
    else
      return $ mapFunc validSelections

main :: IO ()
main = do
  printLines introductionText
  getInitialCommand

getInitialCommand :: IO ()
getInitialCommand = do
  printPrompt
  firstCmd <- getLine
  let args = words (map toLower firstCmd)
      cmd = parseCommand args

  case cmd of
    CmdAct2 -> do
      supplies <- getSuppliesSelection act2SuppliesList mapSelectionsToEntitiesAct2
      let ps = PlayerState {inventory_ = supplies}
      ps2 <- startAct2 ps
      ps3 <- startAct3 ps2
      return ()
    CmdAct3 -> do
      supplies <- getSuppliesSelection act3SuppliesList mapSelectionsToEntitiesAct3
      let ps = PlayerState {inventory_ = supplies}
      ps3 <- startAct3 ps
      return ()
    CmdNext -> do
      ps1 <- startAct1 emptyPlayer
      ps2 <- startAct2 ps1
      ps3 <- startAct3 ps2
      return ()
    _ -> do
      printLines ["Unknown command. Please type 'next' to start the game or 'act2'/'act3' to skip to a specific act.", ""]
      getInitialCommand
