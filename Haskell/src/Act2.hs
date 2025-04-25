module Act2 (startAct2) where

import Control.Applicative ((<|>))
import Data.Bool (Bool (False, True))
import Data.Char (toLower)
import Data.Function ((&))
import Data.List (delete, find, partition)
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
    (Compartment, []),
    ( Wreck,
      [ Entity Item "wreck object" "A wrecked plane" False,
        Entity Item "pistol" "A pistol, probably from the wreck." True
      ]
    )
  ]

removeFromLocation :: Location -> Entity -> GameState -> GameState
removeFromLocation loc ent st =
  let remaining = delete ent (entitiesAt loc st)
      newLocs =
        map
          (\(l, es) -> if l == loc then (l, remaining) else (l, es))
          (locationEntities st)
   in st {locationEntities = newLocs}

removeFromLocationByName :: Location -> String -> GameState -> GameState
removeFromLocationByName loc n gs =
  let remaining = filter ((/= map toLower n) . map toLower . entityName) (entitiesAt loc gs)
      newLocs =
        map
          (\(l, es) -> if l == loc then (l, remaining) else (l, es))
          (locationEntities gs)
   in gs {locationEntities = newLocs}

moveSuppliesToCompartment :: PlayerState -> PlayerState
moveSuppliesToCompartment ps =
  let compartment =
        Entity
          Item
          "compartment"
          "The plane compartment for supplies."
          False
   in ps {inventory_ = compartment : inventory_ ps}

setLocation :: Location -> GameState -> GameState
setLocation loc gs = gs {currentLocation = loc}

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

act2Epilog :: Lines
act2Epilog =
  [ "----------------------------ACT 2 OVER----------------------------"
  ]

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
  | currentLocation st == CrashSite,
    "crash_site_described" `notElem` tasks st =
      "I should first LOOK around to get my bearings."
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
    hasTask "injured_clara" st,
    not (hasTask "plane_examined" st) =
      "Clara needs help fast, and the wreckage of the PLANE might have something useful."
  | currentLocation st == CrashSite,
    hasTask "injured_clara" st,
    hasTask "plane_examined" st =
      "Clara needs help fast, and a MEDKIT should be in the luggage COMPARTMENT."
  | currentLocation st == CrashSite,
    not (hasTask "injured_clara" st),
    not (hasTask "compartment_checked" st) =
      "I should check the luggage COMPARTMENT for the rest of the supplies."
  | currentLocation st == CrashSite,
    not (hasTask "injured_clara" st) =
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

describeCrashSite :: GameState -> String
describeCrashSite st
  | "crash_site_described" `notElem` tasks st =
      "You wake amid the wreckage, cold seeping into your bones.\n"
        ++ "The PLANE is a ruin, and CLARA lies injured nearby.\n\
           \Twisted metal juts from the snow, half-burying the fuselage; engine debris still smoulders.\n\
           \Wind howls, stinging your face with ice.\n\
           \CLARA slumps a few feet away, blood staining the snow beneath her head."
  | "injured_clara" `elem` tasks st =
      "The PLANE is a ruin, and CLARA lies injured nearby.\n\
      \Twisted metal juts from the snow, half-burying the fuselage; engine debris still smoulders.\n\
      \Wind howls, stinging your face with ice.\n\
      \CLARA slumps a few feet away, blood staining the snow beneath her head."
  | otherwise =
      "The wreckage of your plane lies scattered across the frozen landscape.\n\
      \Clara stands nearby, looking shaken but determined.\n\
      \This place offers no shelter from the biting Antarctic cold.\n\
      \The crash site aligns with Byrd's coordinates."

describeCave :: GameState -> String
describeCave st
  | "entered_wreck" `elem` tasks st =
      "Slowly and carefully, you emerge from the wreckage.\n\
      \The dark cave corridor stretches before you."
  | "wreck_discovery" `notElem` tasks st =
      "The cave twists downward, its walls polished and warm. A low hum vibrates the air.\n\
      \Smooth, spiralling walls funnel you DEEPER; faint lights pulse below."
  | otherwise =
      "The entrance to the ice cave stretches before you.\n\
      \The unnatural smoothness of the walls suggests intelligent design.\n\
      \To the right, the Nazi flying-saucer WRECK remains embedded in the ice.\n\
      \A path leads DEEPER into the tunnel, where faint blue light pulses.\n\
      \A massive disk-shaped craft protrudes from the wall, its metallic surface scarred and dented—\n\
      \futuristic yet ancient."

dialogClaraA2 :: GameState -> (GameState, Lines)
dialogClaraA2 st
  | atLocation Cockpit st,
    hasTask "radio_used" st,
    not (hasTask "crashed" st) =
      let st1 = addTask "crashed" st
          st2 = addTask "injured_clara" st1
          st' = setLocation CrashSite st2
       in ( st',
            [ "You: \"Is everything okay?\"",
              "Clara: \"I don't know; the compass and the altimeter suddenly started going crazy,",
              "but we're close to our destination, so it shouldn't be a probl-\"",
              "",
              "*Suddenly, turbulence slams the plane. Lights flicker, instruments fail, the engines choke.*",
              "Clara (shouting): \"Brace yourself! Everything's shutting down!\"",
              "You (screaming): \"Ahh, what's happening!?\"",
              "*The plane spirals down, crashing into the ice. Darkness falls.*"
            ]
          )
  | atLocation Cockpit st,
    not (hasTask "crashed" st),
    not (hasTask "cockpit_intro_done" st) =
      let st' = addTask "awaiting_cockpit_choice" st
       in ( st',
            [ "Clara glances over: \"So, doc, what's your take? Are we on a wild goose chase,",
              "or is there really something out here in this frozen wasteland?\"",
              "Your choices:",
              "1. \"Byrd's diary points to 70S, 10E. The coordinates are too specific to be nothing.\"",
              "2. \"I don't know, but the weather's turning ugly. We need to stay sharp.\"",
              "3. \"Whether it's real or not, the mission's worth it for the discovery alone.\""
            ]
          )
  | atLocation Cockpit st,
    not (hasTask "crashed" st) =
      ( st,
        [ "You: \"We're bound to find something there—-I can feel it in my bones.\"",
          "Clara: \"Hopefully, or all our efforts will be for nothing.\""
        ]
      )
  | atLocation CrashSite st,
    hasTask "injured_clara" st,
    not (inventoryHas "medkit" st) =
      (st, ["She's unconscious and needs medical attention urgently."])
  | atLocation CrashSite st,
    not (hasTask "injured_clara" st),
    not (hasTask "cave_advice" st) =
      ( addTask "cave_advice" st,
        [ "Clara: \"We can't stay exposed out here. That CAVE might be our only shot,",
          "but it's giving me a bad feeling. We must GO now, before it gets dark.\""
        ]
      )
  | atLocation Cave st,
    not (hasTask "wreck_discovery" st) =
      ( addTask "wreck_discovery" st,
        [ "Clara: \"Hey, what's that? Do you see it?\"",
          "",
          "On the right side of the tunnel you see a disk-shaped WRECK –",
          "a massive saucer-like craft embedded in the ice, its metallic surface scarred and dented.",
          "It looks futuristic yet ancient."
        ]
      )
  | atLocation Cave st,
    hasTask "wreck_examined" st,
    not (hasTask "entered_wreck" st),
    not (hasTask "wreck_discovery2" st) =
      ( addTask "wreck_discovery2" st,
        [ "You: \"This is incredible. A Nazi flying saucer?\"",
          "Clara: \"Looks like it. But how did it get here? And why?\"",
          "You: \"Maybe they were experimenting with advanced technology in Antarctica.\"",
          "Clara: \"Or maybe they found something here. Either way, it's creepy.\"",
          "",
          "Clara: \"Do you think we should try to get INSIDE it or don't risk and GO DEEPER?\""
        ]
      )
  | atLocation Wreck st =
      ( st,
        [ "Clara: \"This place gives me the creeps. Look at these controls — they're way ahead of their time.\"",
          "You: \"Yeah, it's like something out of science-fiction. But it's real.\"",
          "Clara: \"I think we've seen enough. Let's keep moving; there might be more ahead. We should GO DEEPER.\""
        ]
      )
  | otherwise = (st, ["There's no one here to talk to."])

injectSupplies ::
  PlayerState ->
  [(Location, [Entity])] ->
  ([(Location, [Entity])], [Entity])
injectSupplies ps locs =
  let (supplies, restInv) = partition (isSupply . entityName) (inventory_ ps)
      locs' = map addSupplies locs
      addSupplies (Compartment, es) = (Compartment, es ++ supplies)
      addSupplies pair = pair
   in (locs', restInv)

processCockpitChoice :: String -> GameState -> (GameState, Lines)
processCockpitChoice choice st
  | choice == "1" =
      ( stDone,
        [ "You: \"Byrd's diary points to 70S, 10E. The coordinates are too specific to be nothing.\"",
          "Clara: \"Specific or not, Antarctica's a maze. Let's hope those coordinates don't lead us straight into trouble.\""
        ]
      )
  | choice == "2" =
      ( stDone,
        [ "You: \"I don't know, but the weather's turning ugly. We need to stay sharp.\"",
          "Clara: \"Yeah, I feel it too. This storm's got teeth. Keep your eyes peeled.\""
        ]
      )
  | choice == "3" =
      ( stDone,
        [ "You: \"Whether it's real or not, the mission's worth it for the discovery alone.\"",
          "Clara: \"Discovery's great until the ice swallows us whole. Still, I like your optimism.\""
        ]
      )
  | otherwise =
      (st, ["Invalid choice - enter 1, 2, or 3."])
  where
    stDone =
      addTask "cockpit_intro_done"
        . removeTask "awaiting_cockpit_choice"
        $ st

processRadioChoice :: String -> GameState -> (GameState, Lines)
processRadioChoice choice st
  | choice == "1" =
      ( stDoneChoice1,
        [ "You: \"Nah, you're freaking out; that's just some usual anomalies. Focus on piloting.\"",
          "Clara: \"Yeah, you're right; there''s no time for that.\""
        ]
      )
  | choice == "2" =
      ( stDoneChoice2,
        [ "You: \"Oh, you know German? I should have guessed from your surname.\"",
          "Clara: \"Yes, my father was a German immigrant. He went to the USA when WWI started.\"",
          "Your choices:",
          "1. \"Byrd's diary doesn't mention Germans, but hey, we're in what Nazi Germany claimed as their territory in Antarctica.\"",
          "2. \"Ah, Uncle Sam, a shelter for all the world's people in need.\""
        ]
      )
  | otherwise =
      (st, ["Invalid choice - enter 1 or 2."])
  where
    stDoneChoice1 =
      addTask "radio_used"
        . removeTask "awaiting_radio_choice"
        $ st

    stDoneChoice2 =
      addTask "awaiting_radio_background"
        . removeTask "awaiting_radio_choice"
        $ st

processRadioBackgroundChoice :: String -> GameState -> (GameState, Lines)
processRadioBackgroundChoice choice st
  | choice == "1" =
      ( stDoneChoice1,
        [ "You: \"Byrd's diary doesn't mention Germans, but hey, we're in what Nazi Germany claimed as their territory in Antarctica.\"",
          "Clara: \"I'm sure the last thing we want is for my German to come in handy. This whole mission feels unreal and ridiculous.\""
        ]
      )
  | choice == "2" =
      ( stDoneChoice2,
        [ "You: \"Ah, Uncle Sam, a shelter for all the world''s people in need.\"",
          "Clara: \"Until he sends you on a mission like this, haha.\"",
          ""
        ]
      )
  | otherwise =
      (st, ["Invalid choice - enter 1 or 2."])
  where
    stDoneChoice1 =
      addTask "radio_used"
        . removeTask "awaiting_radio_background"
        $ st

    stDoneChoice2 =
      addTask "radio_used"
        . removeTask "awaiting_radio_background"
        $ st

atLocation :: Location -> GameState -> Bool
atLocation loc gs = currentLocation gs == loc

inventoryHas :: String -> GameState -> Bool
inventoryHas n gs =
  any ((== map toLower n) . map toLower . entityName) (inventory gs)

hasTask :: String -> GameState -> Bool
hasTask t gs = t `elem` tasks gs

examineSpecialA2 :: String -> GameState -> Maybe (GameState, Lines)
examineSpecialA2 key st = case map toLower key of
  "diary"
    | atLocation Cockpit st ->
        Just
          ( markExamined "diary" st,
            [ "*It is open on the coordinates*",
              "There must be some truth in it.",
              ""
            ]
          )
  "radio"
    | atLocation Cockpit st ->
        Just
          ( addTask "radio_examined" $
              markExamined "radio" st,
            [ "*The radio has a frequency adjuster*",
              "Maybe I could run into something interesting by switching frequencies.",
              ""
            ]
          )
  "clara"
    | atLocation Cockpit st,
      not (hasTask "crashed" st) ->
        Just
          ( st,
            [ "Clara pilots beside you, focused on the controls.",
              ""
            ]
          )
    | atLocation CrashSite st,
      hasTask "crashed" st ->
        Just
          ( st,
            [ "She's unconscious, her forehead gashed, her breathing shallow.",
              "Blood soaks the snow around her leg.",
              ""
            ]
          )
  "plane"
    | atLocation CrashSite st ->
        let st' =
              addTask "plane_examined" $
                markExamined "plane" st
         in Just
              ( st',
                [ "The plane's a lost cause, but the luggage COMPARTMENT is intact.",
                  "The supplies you took are probably still there.",
                  ""
                ]
              )
  "compartment"
    | atLocation CrashSite st
        && hasTask "injured_clara" st
        && not (inventoryHas "medkit" st) ->
        let stExamined =
              addTask "compartment_checked" $
                markExamined "compartment" $
                  markExamined "plane" st

            supplies = entitiesAt Compartment stExamined
            hasMedkit = any ((== "medkit") . map toLower . entityName) supplies
         in if hasMedkit
              then
                Just
                  ( stExamined,
                    [ "You check the plane compartment for supplies.",
                      "Inside you find:"
                    ]
                      ++ map (("- " ++) . entityName) supplies
                      ++ [""]
                  )
              else
                let stGameOver =
                      addTask "game_over"
                        . addTask "act2_finished"
                        $ stExamined
                 in Just
                      ( stGameOver,
                        [ "You check the plane compartment for supplies.",
                          "You realize you forgot to take the MEDKIT before departure.",
                          "You: \"How could I forget it? What can I do now?\"",
                          "You start to panic, gasping heavily.",
                          "",
                          "You: \"Clara, Clara, wake up!\"",
                          "You try to rouse Clara, but it's futile.",
                          "You: (crying) \"Clara, please, I can't do this alone.\"",
                          "",
                          "With her wound untreated, Clara continues to bleed.",
                          "Suddenly, her heart stops beating.",
                          "As the cold overwhelms you, the Antarctic claims you both.",
                          "GAME OVER",
                          ""
                        ]
                      )
    | atLocation CrashSite st ->
        let st' = addTask "compartment_checked" st
         in Just
              ( markExamined "compartment" st',
                [ "You check the plane compartment for supplies.",
                  "In the compartment you find:"
                ]
                  ++ map (("- " ++) . entityName) (entitiesAt Compartment st')
                  ++ [""]
              )
  "wreck"
    | atLocation Cave st ->
        let st' = addTask "wreck_examined" $ markExamined "wreck" st
         in Just
              ( st',
                [ "A disk-shaped craft protrudes from the ice, marked with a Nazi Balkenkreuz,",
                  "\"Hergestellt in Deutschland. 1944. Danzig\". Machine-gun nests bristle from",
                  "its surface.",
                  "",
                  "Clara: \"Made in Germany. 1944. Danzig. I think that's Nazi tech-what's it doing here?\"",
                  "",
                  "There appears to be an entrance. You could GO inside to investigate further.",
                  ""
                ]
              )
  "pistol"
    | atLocation Wreck st ->
        Just
          ( markExamined "pistol" st,
            [ "An old German Mauser C96 pistol. Still looks functional.",
              ""
            ]
          )
  "cave"
    | atLocation Cave st,
      hasTask "cave_advice" st ->
        Just
          ( st,
            [ "This isn't natural — someone or something shaped it.",
              ""
            ]
          )
  _ -> Nothing

goDeeper :: GameState -> IO (GameState, Lines)
goDeeper st
  | atLocation Cave st,
    not (hasTask "wreck_discovery" st) =
      pure
        ( addTask "wreck_discovery" st,
          ["Clara: \"Hey, what's that? Do you see it?\"", ""]
        )
  | atLocation Cave st || atLocation Wreck st,
    not (hasTask "wreck_examined" st),
    not (hasTask "entered_wreck" st) =
      pure
        ( st,
          [ "Clara: \"Hold up, doc. We can't ignore this—it's too weird.\"",
            "You should EXAMINE the WRECK.",
            ""
          ]
        )
  | atLocation Cave st || atLocation Wreck st =
      let st1 =
            if atLocation Wreck st
              then setLocation Cave st
              else st
          st2 = setLocation Tunnel st1
          st3 = addTask "entered_cave" st2
          st4 = addTask "act2_finished" st3
       in pure
            ( st4,
              [ "Slowly and carefully, you emerge from the wreckage.",
                "The dark cave corridor stretches before you.",
                "As you descend deeper into the tunnel, a roar shakes the walls as a",
                "bat-winged aircraft rockets past, vanishing toward the outside world.",
                "",
                "Clara: \"That's Nazi design—straight out of the war!\"",
                "You: \"I'm freaking out; let's get out of here. I think I see light ahead.\"",
                "",
                "A steady glow blooms from the tunnel's depths, pulling you forward.",
                ""
              ]
                ++ act2Epilog
                ++ [""]
            )
  | otherwise =
      pure (st, ["You can't go deeper from here.", ""])

stepA2 :: GameState -> Command -> IO (GameState, Lines)
stepA2 st _
  | hasTask "act2_finished" st =
      pure (st, ["You've already finished this act. Type \"quit\" to exit.", ""])
stepA2 st CmdQuit = pure (st, ["Good-bye."])
stepA2 st CmdLook =
  case currentLocation st of
    CrashSite
      | "crash_site_described" `notElem` tasks st ->
          let st1 = addTask "crash_site_described" st
              out =
                "You wake amid the wreckage, cold seeping into your bones.\n"
                  ++ describeCrashSite st1
           in pure (st1, [out, ""])
    _ ->
      let out = case currentLocation st of
            Cockpit -> cockpitDesc
            CrashSite -> describeCrashSite st
            Cave -> describeCave st
            Wreck -> wreckDesc
            Tunnel -> describeCave st
            _ -> describeLocation (currentLocation st)
       in pure (st, [out, ""])
stepA2 st CmdInventory =
  let inv = inventory st
   in if null inv
        then pure (st, ["You are not carrying anything.", ""])
        else pure (st, "You are carrying:" : map entityName inv ++ [""])
stepA2 st CmdHint = pure (st, [getHint st, ""])
stepA2 st CmdInstructions = pure (st, instructionsText)
stepA2 st (CmdGo p)
  | map toLower p == "deeper" = goDeeper st
  | otherwise =
      case parseLocation p of
        Unknown -> pure (st, ["Unknown place: " ++ p, ""])
        loc
          | atLocation CrashSite st,
            hasTask "injured_clara" st ->
              pure (st, ["I need to help Clara first.", ""])
          | atLocation CrashSite st,
            not (null (entitiesAt Compartment st)) ->
              pure (st, ["I should check the luggage COMPARTMENT for the rest of the supplies first.", ""])
          | canMove (currentLocation st) loc ->
              let st' = st {currentLocation = loc}
                  out = case loc of
                    CrashSite -> "You step toward the entrance, driven by cold and curiosity." ++ describeCrashSite st'
                    Cave -> describeCave st'
                    Wreck -> "You and Clara enter the wreck through its hatch." ++ describeLocation loc
                    _ -> describeLocation loc
               in pure (st', [out, ""])
          | otherwise ->
              pure (st, ["You can't go to " ++ p ++ " from here.", ""])
stepA2 st (CmdExamine obj) =
  case examineSpecialA2 (map toLower obj) st of
    Just res -> pure res
    Nothing ->
      case findEntity obj st of
        Just e ->
          pure
            ( markExamined (entityName e) st,
              [entityDescription e, ""]
            )
        Nothing ->
          pure
            ( st,
              [ "I can't see " ++ obj ++ " here or there's nothing special about it.",
                ""
              ]
            )
stepA2 st (CmdTalk who)
  | map toLower who == "clara" = pure (dialogClaraA2 st)
  | otherwise = pure (st, ["There's no one here by that name.", ""])
stepA2 st (CmdTake raw)
  | atLocation CrashSite st,
    hasTask "injured_clara" st =
      pure (st, ["I need to help Clara first.", ""])
  | atLocation CrashSite st,
    not (hasTask "plane_examined" st) =
      pure (st, ["I don't know where the supplies are.", ""])
  | atLocation CrashSite st,
    not (hasTask "compartment_checked" st) =
      pure (st, ["I should check the luggage COMPARTMENT.", ""])
  | otherwise =
      let name = map toLower raw
          invAlready = isInInventory name st
          here =
            findHere name st
              <|> find
                ((== name) . map toLower . entityName)
                (entitiesAt Compartment st)

          limitFull = pure (st, ["You cannot take this - you've reached the limit (5 items).", ""])
          notHere = pure (st, ["I don't see " ++ raw ++ " here.", ""])

          removeFromLocation :: Location -> Entity -> GameState -> GameState
          removeFromLocation loc ent gs =
            let remaining = delete ent (entitiesAt loc gs)
                newLocs =
                  map
                    (\(l, es) -> if l == loc then (l, remaining) else (l, es))
                    (locationEntities gs)
             in gs {locationEntities = newLocs}

          takeEntity :: Entity -> String -> IO (GameState, Lines)
          takeEntity ent msg = do
            let st1 = addToInventory ent st
                st2 =
                  if ent `elem` entitiesAt Compartment st
                    then removeFromLocation Compartment ent st1
                    else st1
            pure (st2, [msg, ""])
       in if invAlready
            then pure (st, ["You're already holding it!", ""])
            else case here of
              Nothing -> notHere
              Just e
                | name == "pistol" ->
                    takeEntity e "You take the PISTOL - hopefully it won't be needed."
                | name == "medkit" ->
                    takeEntity e "Thank God I took it."
                | isSupply name ->
                    if length (filter (isSupply . entityName) (inventory st)) >= 5
                      then limitFull
                      else takeEntity e ("You take the " ++ entityName e ++ ".")
                | otherwise ->
                    pure (st, ["You can't take that.", ""])
stepA2 st (CmdDrop raw) =
  let name = map toLower raw
   in if not (isInInventory name st)
        then pure (st, ["You aren't carrying that!", ""])
        else do
          let Just e = findEntity name st
              st' = removeFromInventory e st
              msg =
                if isSupply name
                  then "You drop the " ++ entityName e ++ "."
                  else "OK."
          pure (st', [msg, ""])
stepA2 st (CmdUse raw)
  | name == "radio",
    atLocation Cockpit st,
    not (hasTask "radio_used" st) =
      let st' =
            addTask "radio_used"
              . addTask "awaiting_radio_choice"
              $ st
       in pure
            ( st',
              [ "You playfully switch frequencies.",
                "Clara: \"What is it, doc? Are you bored?\"",
                "You: \"Kind of.\"",
                "",
                "After a while, you run into something. The radio spits static until a garbled voice breaks through.",
                "",
                "Clara: \"Wait, what? I think I hear German, but the audio is too distorted; I can't make out the words.\"",
                "Your choices:",
                "1. \"Nah, you're freaking out; that's just some usual anomalies. Focus on piloting.\"",
                "2. \"Oh, you know German? I should have guessed from your surname.\"",
                ""
              ]
            )
  | name == "radio",
    inventoryHas "radio" st,
    not (hasTask "entered_cave" st),
    not (hasTask "injured_clara" st) =
      pure
        ( st,
          [ "No signal to base, but switching channels catches German again:",
            "\"Herr [distortion], wann ist die Glocke fertig? Ich denke, wir [distortion] die Arbeit nachste Woche been...\"",
            "can be heard between the static noise.",
            ""
          ]
        )
  | name == "medkit",
    atLocation CrashSite st,
    hasTask "injured_clara" st =
      let st1 = removeTask "injured_clara" st
          st2 = case findEntity "medkit" st1 of
            Just medkit -> removeFromInventory medkit st1
            Nothing -> st1
          st3 = removeFromLocationByName Compartment "medkit" st2
       in pure
            ( st3,
              [ "You bandage Clara's wounds; she stirs awake.",
                "Clara (mumbling): \"...what happened? Where are we?\"",
                "You: \"Thank God, you're alive. We crashed, and you're injured, but I think you'll be okay.\"",
                "Clara: \"Thanks, doc… I thought I was a goner.\"",
                ""
              ]
            )
  | isInInventory name st =
      pure (st, ["I can't use that right now.", ""])
  | otherwise =
      pure (st, ["I don't have it or I can't use it.", ""])
  where
    name = map toLower raw
stepA2 st CmdUnknown = pure (st, ["Unknown command.", ""])
stepA2 st CmdNext
  | hasTask "act2_finished" st = pure (st, ["Preparing Act 3…"])
  | otherwise = pure (st, ["You need to finish this act first."])

gameLoopA2 :: GameState -> IO PlayerState
gameLoopA2 st = do
  line <- readCommand
  let cmdLine = map toLower line
      args = words cmdLine
      cmd = parseCommand args

  (st', out) <-
    if hasTask "awaiting_cockpit_choice" st
      then return (processCockpitChoice cmdLine st)
      else
        if hasTask "awaiting_radio_choice" st
          then return (processRadioChoice cmdLine st)
          else
            if hasTask "awaiting_radio_background" st
              then return (processRadioBackgroundChoice cmdLine st)
              else case cmd of
                CmdTalk "clara" -> return (dialogClaraA2 st)
                _ -> stepA2 st cmd

  printLines out

  case cmd of
    CmdQuit -> pure (extractPlayerState st')
    CmdNext
      | hasTask "act2_finished" st' -> pure (extractPlayerState st')
    _ -> gameLoopA2 st'

startAct2 :: PlayerState -> IO PlayerState
startAct2 ps0 = do
  let (locWithSupplies, invAfter) = injectSupplies ps0 initialEntities
      gs0 =
        initialState
          { locationEntities = locWithSupplies,
            inventory = invAfter
          }
  printLines act2Prolog
  printLines [cockpitDesc, ""]
  gameLoopA2 gs0