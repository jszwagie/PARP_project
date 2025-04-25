module Act1
  ( GameState,
    startAct1,
  )
where

import Act2 (startAct2)
import Control.Applicative ((<|>))
import Data.Char (toLower)
import Data.List (delete, find)
import Data.Maybe (fromMaybe, isJust, isNothing)
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

act1Prolog :: Lines
act1Prolog =
  [ "ACT 1: DEPARTURE FROM THE EDGE OF THE WORLD",
    "",
    "You awaken to a stark view from your window at an Antarctic base camp in",
    "New Swabia. A desolate expanse of ice and snow stretches endlessly under",
    "a pale, gray sky. You get up, dress in layers against the cold and step",
    "outside.",
    ""
  ]

initialState :: GameState
initialState =
  GameState
    { currentLocation = Yard,
      locationEntities = initialEntities,
      inventory = [],
      examined = [],
      talked = [],
      tasks = []
    }

initialEntities :: [(Location, [Entity])]
initialEntities =
  [ (Yard, []),
    ( Barrack,
      [ Entity Item "photo" "A photo of your late wife sits on the dresser." False,
        Entity Item "lighter" "A simple silver lighter. You should really quit smoking." True,
        Entity Item "calendar" "August 26, 1946." False
      ]
    ),
    ( Runway,
      [ Entity Person "clara" "Clara stands near the plane, wearing a military pilot's uniform with rolled-up sleeves." False,
        Entity Item "plane" "Your type served well in the war." False,
        Entity Item "tanks" "Fuel tanks for the plane. They're running low." False
      ]
    ),
    ( Depot,
      [Entity Item "canister" "A heavy fuel canister. Necessary for the journey." True]
    ),
    ( Tent,
      [ Entity Item "food" "Canned goods and dried meals." True,
        Entity Item "water" "Fresh water in sealed containers." True,
        Entity Item "geiger" "A standard radiation detector." True,
        Entity Item "medkit" "Bandages, antiseptic, morphine…" True,
        Entity Item "radio" "A shortwave field radio." True,
        Entity Item "gear" "Ropes, pitons, carabiners." True,
        Entity Item "tools" "A compass, maps, and a sextant." True,
        Entity Item "list" "A supply list showing available items." False
      ]
    )
  ]

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

parseLocation :: String -> Location
parseLocation s = case map toLower s of
  "barrack" -> Barrack
  "yard" -> Yard
  "runway" -> Runway
  "depot" -> Depot
  "tent" -> Tent
  _ -> Unknown

describeLocation :: Location -> String
describeLocation Yard = yardDesc
describeLocation Barrack = barrackDesc
describeLocation Depot = depotDesc
describeLocation Tent = tentDesc
describeLocation Unknown = "You see nothing special."

yardDesc, barrackDesc, depotDesc, tentDesc :: String
yardDesc =
  "You're on the BARRACK yard. Nearby, a sturdy twin-engine plane rests\n\
  \on a makeshift RUNWAY, its metal hull glinting faintly in the weak sunlight.\n\
  \To the side, there's a fuel DEPOT and a supply TENT. The air is frigid,\n\
  \the wind howls intermittently, and the isolation weighs heavily. By the plane,\n\
  \you spot your partner, Lt. CLARA Voss, a pragmatic military pilot assigned\n\
  \to join you on this mission."
barrackDesc =
  "This is your resting place during the mission - small but convenient.\n\
  \Your bed is neatly made, and a PHOTO of your late wife sits on the dresser beside it.\n\
  \Across the room, your working desk holds mission documents, a small lamp, and a LIGHTER.\n\
  \A CALENDAR hangs above the desk.\n\
  \Outside - the YARD, covered in snow."
depotDesc =
  "You step into the depot, a rough but functional structure shielding fuel CANISTERs from the Antarctic cold.\n\
  \Outside - the YARD, covered in snow."
tentDesc =
  "You enter the supply tent, a cramped space cluttered with gear.\n\
  \Boxes and crates are labeled with essentials: FOOD, WATER, scientific tools, and survival equipment.\n\
  \A LIST of stock hangs on the wall.\n\
  \Outside - the YARD, covered in snow."

getHint :: GameState -> String
getHint st
  | "act_finished" `elem` tasks st =
      "You've already finished this act. Type \"quit\" to leave the game."
  | currentLocation st == Barrack
      && not (isInInventory "lighter" st) =
      "I should gather something useful."
  | "fuel_request" `elem` tasks st
      && "can_take_canister" `notElem` tasks st =
      "I should check the fuel TANKS."
  | "fuel_request" `elem` tasks st
      && not (isInInventory "canister" st) =
      "I should gather some fuel from the DEPOT."
  | "fuel_request" `elem` tasks st
      && isInInventory "canister" st =
      "I should give the CANISTER to Clara."
  | "collect_supplies" `elem` tasks st
      && currentLocation st == Runway
      && countSupplies (inventory st) > 0 =
      "I should thank her for the coffee."
  | "collect_supplies" `elem` tasks st
      && currentLocation st /= Tent =
      "I should gather supplies in the supply TENT."
  | "collect_supplies" `elem` tasks st
      && currentLocation st == Tent =
      "I should take only the most necessary items for the mission."
  | otherwise =
      "I think I should talk with Clara."

countSupplies :: [Entity] -> Int
countSupplies = length . filter (isSupply . entityName)

describeRunway :: GameState -> String
describeRunway st
  | countSupplies (inventory st) > 0 =
      "Clara has finished fueling and has something waiting for you.\n\
      \You pack the supplies into the plane. The reason for your journey –\n\
      \Admiral Byrd's diary - lies open on a box in front of you, its cryptic\n\
      \coordinates circled in red ink: 70S, 10E.\n\
      \Clara hands you a cup of lukewarm coffee."
  | not (isInInventory "canister" st) =
      "The sunlight, reflected off the steel plates, blinds you as you approach\n\
      \the aircraft - a Douglas A-20 Havoc. It's not the newest PLANE, but it's\n\
      \reliable. CLARA is tinkering with one of the engines.\n\
      \Behind you - the YARD, covered in snow."
  | otherwise =
      "The sunlight, reflected off the steel plates, blinds you as you approach\n\
      \the Douglas A-20 Havoc - a reliable, if not modern, PLANE.\n\
      \Clara is still tinkering with one of the engines.\n\
      \Behind you - the YARD, covered in snow."

pureDialogWithClara :: GameState -> (GameState, Lines)
pureDialogWithClara st
  | currentLocation st /= Runway =
      ( addTask "go_to_clara" st,
        ["I'm not going to shout; I should go to her at the RUNWAY."]
      )
pureDialogWithClara st
  | "fuel_request" `elem` tasks st
      && isInInventory "canister" st =
      let Just can = findEntity "canister" st
          st1 = removeFromInventory can st
          st2 = removeTask "fuel_request" $ addTask "awaiting_supply_choice" st1
       in ( st2,
            [ "You: \"I have it!\"",
              "Clara: \"Nice, hand it over - our bird's thirsty.\"",
              "*starts fueling the plane*",
              "Clara: \"Why don't you gather some supplies while I finish fueling?\"",
              "Your choices:",
              "1. \"On my way\"",
              "2. \"Are you sure you can handle it by yourself?\""
            ]
          )
pureDialogWithClara st
  | "collect_supplies" `elem` tasks st
      && countSupplies (inventory st) > 0
      && "explain_stage" `notElem` tasks st =
      let st1 =
            addTask "awaiting_explain_choice" $
              addTask "explain_stage" st
       in ( st1,
            [ "You: \"Thank you!\"",
              "*a moment of silence*",
              "Clara: \"So, tell me again why we're risking our necks for this?",
              "A diary from some explorer doesn't scream 'top priority' to me.\"",
              "Your choices:",
              "1. \"Because it could be the discovery of the century.\"",
              "2. \"Orders are orders. The government wants answers.\"",
              "3. \"I've got a feeling there's something big waiting for us.\""
            ]
          )
pureDialogWithClara st
  | "clara_fuel_request" `notElem` talked st =
      let st1 = markTalked "clara" "fuel_request" st
          st2 = addTask "awaiting_clara_choice" st1
       in ( st2,
            [ "Clara: \"Morning, doc. We need to get this bird fueled up. Could you check the TANKS for me?\"",
              "Your choices:",
              "1. \"Okay, I'll handle it now.\"",
              "2. \"I think it's good enough, but I could double-check.\"",
              "3. \"Why don't you take care of it?\""
            ]
          )
pureDialogWithClara st = (st, ["Clara is busy working on the plane."])

pureClaraChoice :: String -> GameState -> (GameState, Lines)
pureClaraChoice choice st
  | choice == "1" =
      let st1 = removeTask "awaiting_clara_choice" st
          st2 = addTask "fuel_request" st1
       in (st2, ["You: \"Okay, I'll handle it now.\""])
  | choice == "2" =
      let st1 = removeTask "awaiting_clara_choice" st
          st2 = addTask "fuel_request" st1
       in ( st2,
            [ "You: \"I think it's good enough, but I could double-check.\"",
              "Clara: \"Good enough doesn't cut it out here. Antarctica doesn't forgive mistakes. Check it properly.\""
            ]
          )
  | choice == "3" =
      let st1 = removeTask "awaiting_clara_choice" st
          st2 = addTask "collect_supplies" st1
       in ( st2,
            [ "You: \"Why don't you take care of it?\"",
              "Clara (frowning): \"Oh, you're lazy, aren't you? Fine, I'll handle it after I check the oil,",
              "but you're not off the hook, doc. Go gather mandatory supplies and drop them near the plane.\""
            ]
          )
  | otherwise = (st, ["Invalid choice - enter 1, 2, or 3."])

pureExplainChoice :: String -> GameState -> (GameState, Lines)
pureExplainChoice choice st
  | choice == "1" =
      let st1 = removeTask "awaiting_explain_choice" st
          st2 = addTask "awaiting_further_choice" st1
       in ( st2,
            [ "You: \"Because it could be the discovery of the century.\"",
              "Clara: \"Discovery of the century? I hope it's not just a pile of ice and a frostbite bill.\"",
              "Your choice:",
              "1. \"Byrd wasn't a dreamer. Those coordinates mean something.\"",
              "2. \"Even if it's nothing, the science alone is worth it.\""
            ]
          )
  | choice == "2" =
      let st1 = removeTask "awaiting_explain_choice" st
       in actEpilog
            [ "You: \"Orders are orders. The government wants answers.\"",
              "Clara: \"Yeah, and Uncle Sam loves sending us into the freezer for kicks. What's their angle?\"",
              "You: \"Cold War jitters, probably. They don't want the Soviets sniffing around first.\""
            ]
            st1
  | choice == "3" =
      let st1 = removeTask "awaiting_explain_choice" st
       in actEpilog
            [ "You: \"I've got a feeling there's something big waiting for us.\"",
              "Clara: \"Feelings don't keep us warm, doc. What's in that diary that's got you hooked?\"",
              "You: \"Hints of a hidden land - geological oddities, maybe more.\""
            ]
            st1
  | otherwise = (st, ["Invalid choice - enter 1, 2, or 3."])

pureFurtherChoice :: String -> GameState -> (GameState, Lines)
pureFurtherChoice choice st
  | choice == "1" =
      let st1 = removeTask "awaiting_further_choice" st
       in actEpilog
            [ "You: \"Byrd wasn't a dreamer. Those coordinates mean something.\"",
              "Clara: \"Maybe. But I'd rather not die proving him right.\""
            ]
            st1
  | choice == "2" =
      let st1 = removeTask "awaiting_further_choice" st
       in actEpilog
            [ "You: \"Even if it's nothing, the science alone is worth it.\"",
              "Clara: \"Maybe. But I'd rather not die proving him right.\""
            ]
            st1
  | otherwise = (st, ["Invalid choice - enter 1 or 2."])

pureSupplyChoice :: String -> GameState -> (GameState, Lines)
pureSupplyChoice choice st
  | choice == "1" =
      let st1 = removeTask "awaiting_supply_choice" st
          st2 = addTask "collect_supplies" st1
       in ( st2,
            [ "You: \"On my way.\"",
              ""
            ]
          )
  | choice == "2" =
      let st1 = removeTask "awaiting_supply_choice" st
          st2 = addTask "collect_supplies" st1
       in ( st2,
            [ "You: \"Are you sure you can handle it by yourself?\"",
              "Clara: \"Don't worry, doc - I'm not a kid. Go grab those supplies.\"",
              ""
            ]
          )
  | otherwise = (st, ["Invalid choice - enter 1 or 2.", ""])

actEpilog :: Lines -> GameState -> (GameState, Lines)
actEpilog pre st =
  let st1 = addTask "act_finished" st
   in ( st1,
        pre
          ++ [ "",
               "You: \"What do you think we'll find out there?\"",
               "Clara: \"Best case? A rock formation worth naming. Worst case? A grave with our names on it.",
               "I don't buy the unearthly land garbage.\"",
               "You: \"Neither do I, but the government does.\"",
               "Clara: \"I think it's time we have a good weather.\"",
               "",
               "Preparations complete, you and Clara climb into the plane's hatch.",
               "Clara starts the engines, ready to challenge the icy wilderness.",
               "The plane roars to life, cutting through swirling snow as it lifts off.",
               "",
               "Inside, you study the diary while Clara grips the yoke.",
               "The horizon swallows the base camp, leaving you with a mix of anticipation -",
               "and a hint of lurking danger.",
               "",
               "----------------------------ACT 1 OVER----------------------------",
               "",
               "Type \"next\" to continue, or \"quit\" to leave."
             ]
      )

examineSpecial :: String -> GameState -> Maybe (GameState, Lines)
examineSpecial key st = case key of
  "tanks"
    | currentLocation st == Runway ->
        let msg =
              [ "You crouch beside the aircraft and open the fuel hatch.",
                "We're running low. We need at least one more drum of fuel.",
                "Clara: \"Told you. Go grab one from the DEPOT.\""
              ]
            st' = addTask "can_take_canister" $ markExamined "tanks" st
         in Just (st', msg ++ [""])
  "list"
    | currentLocation st == Tent ->
        let msg =
              [ "- FOOD rations",
                "- WATER",
                "- GEIGER Counter",
                "- MEDKIT",
                "- RADIO",
                "- Climbing GEAR",
                "- Navigation TOOLS",
                "The plane has a capacity of only 5 items; choose wisely."
              ]
            st' = markExamined "list" st
         in Just (st', msg ++ [""])
  "photo"
    | currentLocation st == Barrack ->
        Just
          ( markExamined "photo" st,
            ["I'll never forget you, my love.", ""]
          )
  "lighter"
    | currentLocation st == Barrack ->
        Just
          ( markExamined "lighter" st,
            ["I really should quit smoking.", ""]
          )
  "food"
    | currentLocation st == Tent ->
        Just
          ( markExamined "food" st,
            [ "Canned goods and dried meals.",
              "Enough to last two weeks, but not exactly gourmet",
              ""
            ]
          )
  "water"
    | currentLocation st == Tent ->
        Just
          ( markExamined "water" st,
            [ "Fresh, sealed water cans.",
              "A week's worth if we ration.",
              ""
            ]
          )
  "geiger"
    | currentLocation st == Tent ->
        Just
          ( markExamined "geiger" st,
            [ "A standard radiation detector.",
              "If we stumble upon something unnatural, this could be crucial.",
              ""
            ]
          )
  "medkit"
    | currentLocation st == Tent ->
        Just
          ( markExamined "medkit" st,
            [ "Bandages, antiseptic, morphine...",
              "Everything needed for basic field medical care.",
              ""
            ]
          )
  "radio"
    | currentLocation st == Tent ->
        Just
          ( markExamined "radio" st,
            [ "A shortwave field radio.",
              "Not the best range, but it should work if we're within contact distance of the base.",
              ""
            ]
          )
  "gear"
    | currentLocation st == Tent ->
        Just
          ( markExamined "gear" st,
            [ "Ropes, pitons, carabiners.",
              "If we need to descend into something deep or climb out of trouble, this will help.",
              ""
            ]
          )
  "tools"
    | currentLocation st == Tent ->
        Just
          ( markExamined "tools" st,
            [ "A compass, maps, and a sextant.",
              "Old-school but reliable.",
              ""
            ]
          )
  "calendar"
    | currentLocation st == Barrack ->
        Just
          ( markExamined "calendar" st,
            ["August 26, 1946", ""]
          )
  "plane"
    | currentLocation st == Runway ->
        Just
          ( markExamined "plane" st,
            ["Your type served well in the war.", ""]
          )
  "canister"
    | currentLocation st == Depot
        && "can_take_canister" `notElem` tasks st ->
        Just
          ( st,
            ["I should check the fuel tanks first.", ""]
          )
    | currentLocation st == Depot ->
        Just
          ( markExamined "canister" st,
            ["Heavy, but necessary.", ""]
          )
  "clara"
    | currentLocation st == Runway ->
        Just
          ( st,
            [ "Clara stands near the plane, wearing a military pilot's uniform with rolled-up sleeves.",
              "Her dark hair is tied back, with a few strands escaping to frame her sharp eyes.",
              ""
            ]
          )
  "clara" ->
    Just
      ( st,
        ["I can't see her clearly from here.", ""]
      )
  "runway"
    | currentLocation st `elem` [Runway, Yard] ->
        Just
          ( st,
            ["The runway is a makeshift strip of concrete slabs, cleared of snow.", ""]
          )
  "depot"
    | currentLocation st `elem` [Depot, Yard] ->
        Just
          ( st,
            ["The depot is a simple structure, but it keeps the fuel canisters safe from the cold.", ""]
          )
  "tent"
    | currentLocation st `elem` [Tent, Yard] ->
        Just
          ( st,
            ["The tent is cramped but well-stocked with supplies.", ""]
          )
  "barrack"
    | currentLocation st `elem` [Barrack, Yard] ->
        Just
          ( st,
            ["Your resting place during the mission.", ""]
          )
  "yard" ->
    Just
      ( st,
        ["The yard is covered in snow.", ""]
      )
  _ -> Nothing

step :: GameState -> Command -> IO (GameState, Lines)
step st _
  | "act_finished" `elem` tasks st =
      return (st, ["You've already finished this act. Type \"quit\" to exit.", ""])
step st CmdQuit = return (st, ["Goodbye."])
step st CmdLook
  | currentLocation st == Runway =
      return (st, [describeRunway st, ""])
  | otherwise =
      return (st, [describeLocation (currentLocation st), ""])
step st CmdInventory =
  let inv = inventory st
   in if null inv
        then return (st, ["You are not carrying anything.", ""])
        else return (st, "You are carrying:" : map entityName inv ++ [""])
step st CmdHint = return (st, [getHint st, ""])
step st CmdInstructions = return (st, instructionsText)
step st (CmdGo p) =
  let loc = parseLocation p
   in case loc of
        Unknown -> return (st, ["Unknown place: " ++ p, ""])
        _
          | canMove (currentLocation st) loc ->
              let st' = st {currentLocation = loc}
                  out = if loc == Runway then describeRunway st' else describeLocation loc
               in return (st', [out, ""])
          | otherwise -> return (st, ["You can't go to " ++ p ++ " from here.", ""])
step st (CmdExamine x) =
  case examineSpecial (map toLower x) st of
    Just res -> return res
    Nothing -> case findEntity x st of
      Just e ->
        return
          ( markExamined (entityName e) st,
            [entityDescription e, ""]
          )
      Nothing ->
        return
          ( st,
            ["I can't see " ++ x ++ " here or there's nothing special about it.", ""]
          )
step st (CmdTalk p)
  | map toLower p == "clara" = return (pureDialogWithClara st)
  | otherwise = return (st, ["There's no one here to talk to by that name.", ""])
step st (CmdTake o) =
  let name = map toLower o
      inv = isInInventory name st
      here = findHere name st
      notHere = return (st, ["I don't see " ++ o ++ " here.", ""])
      limitFull = return (st, ["You cannot take this - you've reached the limit (5 items).", ""])
   in if inv
        then return (st, ["You're already holding it!", ""])
        else
          if isNothing here
            then notHere
            else case name of
              "photo" -> return (st, ["Sorry, my love, but I can't take you with me.", ""])
              "calendar" -> return (st, ["I doubt this will be useful.", ""])
              "lighter" -> return (st, ["I'm not going to smoke now.", ""])
              "canister"
                | "can_take_canister" `notElem` tasks st ->
                    return (st, ["I should check the fuel TANKS first.", ""])
                | otherwise ->
                    let Just e = here
                     in return (addToInventory e st, ["This should be enough.", ""])
              _
                | isSupply name ->
                    if "collect_supplies" `notElem` tasks st
                      then return (st, ["I don't need this right now. I should TALK to Clara first.", ""])
                      else
                        if countSupplies (inventory st) >= 5
                          then limitFull
                          else
                            let Just e = here
                             in return
                                  ( addToInventory e st,
                                    ["You take the " ++ entityName e ++ ".", ""]
                                  )
                | otherwise ->
                    let Just e = here
                     in if takeableByDefault e
                          then
                            return
                              ( addToInventory e st,
                                ["You take the " ++ entityName e ++ ".", ""]
                              )
                          else return (st, ["You can't take that.", ""])
step st (CmdDrop o) =
  let name = map toLower o
   in if not (isInInventory name st)
        then return (st, ["You aren't carrying that!", ""])
        else case name of
          "lighter" ->
            let Just e = findEntity name st
                st' = removeFromInventory e st
             in return (st', ["I might need something else instead.", ""])
          "canister" ->
            return (st, ["I should get it to Clara. I'll TALK to her.", ""])
          _
            | isSupply name ->
                let Just e = findEntity name st
                    st' = removeFromInventory e st
                 in return (st', ["You drop the " ++ entityName e ++ ".", ""])
            | otherwise ->
                let Just e = findEntity name st
                    st' = removeFromInventory e st
                 in return (st', ["OK.", ""])
-- use
step st (CmdUse x)
  | map toLower x == "canister"
      && isInInventory "canister" st
      && currentLocation st == Runway =
      return (st, ["I should give it to Clara. I'll TALK to her.", ""])
  | isInInventory (map toLower x) st =
      return (st, ["I can't use that right now.", ""])
  | otherwise =
      return (st, ["I don't have it or I can't use it.", ""])
step st CmdUnknown = return (st, ["Unknown command.", ""])
-- next
step st CmdNext
  | "act_finished" `elem` tasks st =
      return (st, ["Preparing Act 2..."])
step st CmdNext =
  return (st, ["You need to finish this act first."])

gameLoop :: GameState -> IO PlayerState
gameLoop st = do
  line <- readCommand
  let cmdLine = map toLower line
      args = words cmdLine
      cmd = parseCommand args

  (st', out) <-
    if "awaiting_clara_choice" `elem` tasks st
      then return (pureClaraChoice cmdLine st)
      else
        if "awaiting_explain_choice" `elem` tasks st
          then return (pureExplainChoice cmdLine st)
          else
            if "awaiting_further_choice" `elem` tasks st
              then return (pureFurtherChoice cmdLine st)
              else
                if "awaiting_supply_choice" `elem` tasks st
                  then return (pureSupplyChoice cmdLine st)
                  else case cmd of
                    CmdTalk "clara" -> return (pureDialogWithClara st)
                    _ -> step st cmd

  printLines out
  case cmd of
    CmdQuit -> pure (extractPlayerState st')
    CmdNext | "act_finished" `elem` tasks st' -> return (extractPlayerState st')
    _ -> gameLoop st'

startAct1 :: PlayerState -> IO PlayerState
startAct1 _ = do
  printLines instructionsText
  printLines act1Prolog
  printLines [describeLocation (currentLocation initialState), ""]
  gameLoop initialState
