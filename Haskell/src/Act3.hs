module Act3 (startAct3) where

import Control.Applicative ((<|>))
import Data.Char (toLower)
import Data.List (delete, find, partition)
import Data.Maybe (fromJust, fromMaybe, isJust, isNothing)
import System.Exit (exitSuccess)
import System.IO (hFlush, stdout)
import System.Random (randomRIO)
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
    "The valley pulses with life-chirping insects fill the air, leaves rustle in a gentle breeze, and the distant roar of an unseen beast sends a shiver down your spine.",
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
  "You both stand on a rocky ledge overlooking a hidden realm-an expansive, verdant valley cradled beneath Antarctica's icy crust.\n\
  \Bioluminescent plants emit a soft, ethereal glow, casting light across towering ferns and crystalline rivers that shimmer like liquid glass.\n\
  \The air hangs warm and humid, thick with the scent of exotic blooms, a jarring contrast to the frozen desolation above.\n\
  \Flying saucers, eerily similar to the wreck you stumbled upon, glide silently through the skies, their presence a quiet warning of something watchful and alive down here."
treeDesc =
  "From the tree's upper branches, the valley sprawls before you in breathtaking detail.\n\
  \To the east, ancient-looking RUINS emerge from the foliage-crumbling pyramids and temples etched with cryptic symbols, remnants of a lost civilization.\n\
  \To the west, the stark silhouette of a CITY cuts through the greenery, its dark gray buildings festooned with swastika flags fluttering ominously in the breeze, their bold red and black stark against the muted stone.\n\
  \Behind you, the TUNNEL exit gapes like a dark maw, leading back to the frozen surface-a lifeline or a trap, depending on your next move."
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
              "Its skin seems to shimmer faintly, and as you look closer, you realize it's communicating directly into your mind-a melodic hum that bypasses your ears.",
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

tuneRadio :: GameState -> Int -> Int -> Int -> (GameState, Lines)
tuneRadio st a b c
  | wrongCount a b c == 0 =
      let st1 = removeTask "radio" st
          st2 = removeFromInventory (fromJust $ findEntity "radio" st) st1
          st3 = addTask "after_radio" st2
          st4 = st3 {currentLocation = Cockpit}
       in ( st4,
            [ "The RADIO hums as it locks onto a strong signal. A clear voice cuts through:",
              "",
              "\"Mission 334, this is the 32nd Marine Corps. Coordinates received. Extraction team inbound. Over.\"",
              "Clara: \"Copy that. We'll hold tight. Over.\"",
              "Marine: \"Copy that. We're tracking your signal. Hold tight, over.\"",
              "",
              "The steady signal brings a flicker of relief amidst the chaos.",
              "",
              "After a tense wait, the roar of engines fills the air. A Marine transport plane descends through the snow, its lights cutting through the gloom.",
              "You and CLARA board, the warmth of the cabin a stark contrast to the biting cold.",
              "As the plane lifts off, a Marine hands you a stack of nondisclosure agreements.",
              "",
              "Marine: \"Sign these. What you saw down there stays buried. Understood?\"",
              "",
              "You nod, a heavy, unspoken weight settling over you.",
              "The valley's mysteries fade into the distance, shrouded in silence, as the plane carries you away.",
              ""
            ]
          )
  | wrongCount a b c == 1 =
      ( st,
        [ "The RADIO picks up a faint Marine transmission, but it's garbled:",
          "",
          "\"Mission... [static]... coordinates... [static]... hold...\"",
          "Clara: \"Almost there, but it's too weak. They won't get our position like this.\"",
          "",
          "HINT: \"The plaque mentions 'A=Even, B=Prime, C=Square.' And the note says 'Four's the square, Seven's luck, Two's pair'-could A be 2?\"",
          ""
        ]
      )
  | wrongCount a b c == 2 =
      ( st,
        [ "A sharp burst of static erupts from the RADIO, followed by a chilling German voice:",
          "",
          "\"Achtung! Feindliche Ubertragung entdeckt!\"",
          "Clara: \"That's the Germans-they've intercepted us. We've got to fix this now!\"",
          "",
          "HINT: \"The plaque mentions 'A=Even, B=Prime, C=Square.' And the note says 'Four's the square, Seven's luck, Two's pair'-could A be 2?\"",
          ""
        ]
      )
  | otherwise =
      ( st,
        [ "The RADIO hisses with static, a grating buzz drowning out any signal.",
          "You: \"Just noise. This isn't the right frequency.\"",
          "",
          "HINT: \"Think it through: 2 is even, 7 is prime, and 4 ties to the square of 2. That matches all the clues.\"",
          ""
        ]
      )
  where
    wrongCount :: Int -> Int -> Int -> Int
    wrongCount a' b' c' =
      (if a' == 2 then 0 else 1)
        + (if b' == 7 then 0 else 1)
        + (if c' == 4 then 0 else 1)

processLedgeChoice :: String -> GameState -> (GameState, Lines)
processLedgeChoice choice st
  | choice == "1" =
      ( removeTask "ledge_talk" $ removeTask "awaiting_ledge_choice" $ addTask "tree" st,
        [ "You: \"Maybe they found it during their Antarctic expeditions in the '30s.\"",
          "Clara: \"That could explain those flying saucers. They've had decades to dig in, hidden from the rest of the world.\"",
          "",
          "You stand together, awestruck by the valley's haunting beauty.",
          "The bioluminescent flora bathes the landscape in a shimmering, otherworldly hue, while the faint hum of the valley's life-chirps, rustles, and distant cries-wraps around you like a living tapestry.",
          "It's a paradise untouched by time, yet the shadow of danger looms just out of sight.",
          "",
          "Clara: *gasps* \"Okay, enough gawking. If we want to survive this, we need a better lay of the land. Let's find a high spot for reconnaissance.\"",
          ""
        ]
      )
  | choice == "2" =
      ( removeTask "awaiting_ledge_choice" $ addTask "tree" st,
        [ "You: \"Or they stumbled across it after the war, looking for a place to hide.\"",
          "Clara: \"Hide? More like regroup. This could be their secret fortress, waiting for the right moment to strike back.\"",
          "",
          "You stand together, awestruck by the valley's haunting beauty.",
          "The bioluminescent flora bathes the landscape in a shimmering, otherworldly hue, while the faint hum of the valley's life-chirps, rustles, and distant cries-wraps around you like a living tapestry.",
          "It's a paradise untouched by time, yet the shadow of danger looms just out of sight.",
          "",
          "Clara: *gasps* \"Okay, enough gawking. If we want to survive this, we need a better lay of the land. Let's find a high spot for reconnaissance.\"",
          ""
        ]
      )
  | otherwise =
      (st, ["Invalid choice - enter 1 or 2.", ""])

talkClara :: GameState -> (GameState, Lines)
talkClara st
  | "after_radio" `elem` tasks st =
      let st1 = addTask "act_finished" st
       in ( st1,
            [ "You turn to Clara, her face illuminated by the dim cabin lights.",
              "",
              "You: \"So, what-this is how it ends?\"",
              "Clara: \"Wake up!\"",
              "You: \"What?!\"",
              "Clara: \"WAKE UP!\"",
              "",
              "Suddenly, a sharper voice breaks through the haze.",
              "Your wife: \"Damn it, wake up! You'll be late for your lectures!\"",
              "You: \"What? What lectures?\"",
              "Your wife: \"You were up late watching TV again. You've got to stop with those",
              "ridiculous pseudo-historical documentaries on FOCUS TV or TV4-they're frying your brain.\"",
              "",
              "The Antarctic adventure dissolves like mist. You blink, disoriented, as the soft ",
              "glow of your bedside lamp replaces the plane's harsh lights. The hum of Warsaw's ",
              "morning traffic seeps through the window, a mundane rhythm far removed from the  ",
              "valley's eerie pulse. It was all a dream-a vivid fantasy spun from late-night ",
              "television and a restless mind. You're not an adventurer escaping a hidden",
              "world; you're an ordinary professor at the Warsaw University of Technology, with",
              "lectures to deliver and papers to grade. Reality sinks in, familiar and unrelenting.",
              "",
              "You sit up, rubbing your eyes as the dream's vivid details-Clara's determined ",
              "gaze, the snow-swept valley, the roar of the plane-slip away like sand through ",
              "your fingers. Your wife moves about the room, muttering about your late-night ",
              "habits, oblivious to the epic journey you've just imagined.",
              "",
              "Your wife: \"Honestly, those conspiracy channels will be the death of you. Go to bed on time for once.\"",
              "",
              "You muster a faint smile, the last echoes of the dream fading into nothingness.",
              "The adventure is over, and the real world beckons.",
              "",
              "THE END",
              ""
            ]
          )
  | "hide" `elem` tasks st && (currentLocation st == Ruins || currentLocation st == City) =
      (st, ["Clara: \"Here, give me the pistol and get behind that ROCK-now!\"", ""])
  | "tunnel" `elem` tasks st && (currentLocation st == Ruins || currentLocation st == City) =
      (st, ["Clara: \"It's a long shot, but let's GO to the TUNNEL now!\"", ""])
  | "tunnel" `elem` tasks st && currentLocation st == Tunnel && not (isInInventory "radio" st) =
      (st, ["Clara: \"You should USE the RADIO\"", ""])
  | "radio" `elem` tasks st && currentLocation st == Tunnel && not (isInInventory "radio" st) =
      (st, ["Clara: \"You should USE the RADIO\"", ""])
  | "woods" `elem` tasks st && (currentLocation st == Ruins || currentLocation st == City) =
      (st, ["Clara: \"It's a long shot, but let's GO to the WOODS now!\"", ""])
  | "after_fight" `elem` tasks st =
      let st1 = addTask "act_finished" $ removeTask "after_fight" st
       in ( st1,
            [ "You: \"What did he say?\"",
              "Clara: *breathing heavily* \"Nothing good. I don't know if we can get out of this alive.\"",
              "Clara: *shouting in fright* \"Wir kapitulieren! Halt!\"",
              "",
              "The soldiers cease fire, their eyes still burning with rage.",
              "They swarm closer, boots pounding the earth like war drums, and you're wrestled to the ground, wrists bound tight with rough cord.",
              "Their treatment is brutal - fists and threats of execution, though they spare you for now, muttering darkly about your potential value.",
              "They march you toward the CITY, their motorcycles roaring triumphantly.",
              "",
              "The CITY looms ahead, its dark spires piercing the bioluminescent sky like ",
              "jagged teeth. Clara stumbles beside you, her face pale but defiant, though her eyes betray a flicker of fear.",
              "",
              "You steal a glance at the leader, his scar twisting as he smirks, satisfied with ",
              "his prize. What awaits in the CITY? Interrogation? Imprisonment? Or something ",
              "far worse, tied to the secrets buried in this impossible valley? The questions ",
              "gnaw at you, but answers remain elusive, shrouded in the same mystery that",
              "cloaks this hidden world.",
              "",
              "As the CITY gates creak open, swallowing you into its shadowed maw, one thought",
              "lingers: this is not the end, but a dark new beginning. Your fate hangs in the ",
              "balance, and the next chapter of your journey waits just beyond the horizon.",
              "",
              "TO BE CONTINUED...",
              ""
            ]
          )
  | "ambush_beginning" `elem` tasks st =
      let st1 = addTask "awaiting_ambush_choice" $ removeTask "ambush_beginning" st
       in ( st1,
            [ "You: \"What's our move? They're closing in fast.\"",
              "Clara: \"We're outgunned and outmanned. Fight, run, or surrender-you decide, but make it quick!\"",
              "Your choices:",
              if isInInventory "pistol" st then "1. \"Let's fight! I'll hand you the PISTOL!\"" else "",
              "2. \"Run for the TUNNEL." ++ if isInInventory "radio" st then " We can try the RADIO one more time!\"" else "\"",
              "3. \"We surrender. Maybe we can talk our way out.\"",
              "4. \"Into the WOODS-lose them in the trees!\"",
              ""
            ]
          )
  | currentLocation st == Ledge && "clara_ledge" `notElem` talked st =
      let st1 = addTask "awaiting_ledge_choice" $ markTalked "clara" "ledge" st
       in ( st1,
            [ "Clara: \"This place... it's like stepping into a dream. Or maybe a nightmare-I can't decide.\"",
              "You: \"It's incredible-Byrd wasn't exaggerating in that diary.\"",
              "Clara: \"Sure, but that diary was written before the war. No mention of Nazis anywhere in it. Do you think they beat us to this discovery?\"",
              "Your choices:",
              "1. \"Maybe they found it during their Antarctic expeditions in the '30s.\"",
              "2. \"Or they stumbled across it after the war, looking for a place to hide.\"",
              ""
            ]
          )
  | currentLocation st == Ruins && any ((== "creature") . entityName) (entitiesAt Ruins st) =
      (st, ["Clara: \"Do you think we should TALK to it?\"", ""])
  | currentLocation st == Tree =
      (st, ["Clara: Do you see anything interesting?", ""])
  | otherwise =
      (st, ["There's no one here to talk to.", ""])

processAmbushChoice :: String -> GameState -> (GameState, Lines)
processAmbushChoice choice st
  | choice == "1" && isInInventory "pistol" st =
      ( removeTask "awaiting_ambush_choice" $ addTask "hide" st,
        [ "You: \"Let's fight! I'll hand you the PISTOL!\"",
          "Clara: \"Here, give me the pistol and get behind that ROCK-now!\"",
          ""
        ]
      )
  | choice == "2" =
      ( removeTask "awaiting_ambush_choice" $ addTask "tunnel" st,
        [ "You: \"To the TUNNEL - move. " ++ if isInInventory "radio" st then "We can try the RADIO one more time!" else "",
          if isInInventory "radio" st
            then "Clara: \"It's a long shot, but let's go!\""
            else "Clara: \"Without the RADIO, we'll just freeze out there. Terrible plan, but I'm with you.\"",
          ""
        ]
      )
  | choice == "3" =
      let st1 = addTask "act_finished" $ removeTask "awaiting_ambush_choice" st
       in ( st1,
            [ "You: \"We surrender. Maybe we can talk our way out.\"",
              "You raise your hands slowly. Clara mirrors your movement and calls out to the soldiers:",
              "Clara: \"Wir kapitulieren! Kein Problem.\"",
              "",
              "The Nazis lower their rifles slightly, though their glares remain sharp as knives. The leader smirks, holstering his Luger with a flourish.",
              "",
              "Nazi Leader: \"Kluger Schachzug, Amerikaner. Unser Kommandant mochte Sie unbedingt sehen.\"",
              "They bind your hands with coarse rope, the knots biting into your wrists, and march you toward the CITY, their motorcycles roaring triumphantly.",
              "",
              "The CITY looms ahead, its dark spires piercing the bioluminescent sky like ",
              "jagged teeth. Clara stumbles beside you, her face pale but defiant, though her eyes betray a flicker of fear.",
              "",
              "You steal a glance at the leader, his scar twisting as he smirks, satisfied with ",
              "his prize. What awaits in the CITY? Interrogation? Imprisonment? Or something ",
              "far worse, tied to the secrets buried in this impossible valley? The questions ",
              "gnaw at you, but answers remain elusive, shrouded in the same mystery that",
              "cloaks this hidden world.",
              "",
              "As the CITY gates creak open, swallowing you into its shadowed maw, one thought",
              "lingers: this is not the end, but a dark new beginning. Your fate hangs in the ",
              "balance, and the next chapter of your journey waits just beyond the horizon.",
              "",
              "TO BE CONTINUED...",
              ""
            ]
          )
  | choice == "4" =
      ( removeTask "awaiting_ambush_choice" $ addTask "woods" st,
        [ "You: \"Come on, to the WOODS-GO!\"",
          ""
        ]
      )
  | otherwise =
      (st, ["Invalid choice - enter 1, 2, 3, or 4.", ""])

talkCreature :: GameState -> (GameState, Lines)
talkCreature st
  | currentLocation st == Ruins && any ((== "creature") . entityName) (entitiesAt Ruins st) =
      let st1 = addTask "awaiting_creature_choice" st
       in ( st1,
            [ "*The creature's voice resonates in your mind, a melodic hum that bypasses your ears entirely.*",
              "",
              "Creature: \"Wanderers, greetings. Sentinel of this realm, I am, keeper of wisdom older than your civilization, hmm.",
              "Answers you seek, yes? Give them to you, I shall.",
              "Tied to what you call 'Atlantis,' our kin are, though lost to your tongue, our true name is.",
              "Arrived, the ones you call 'Germans' did, speaking of a great calamity they fled. Stewards of peace we are, granted them refuge, we did. Yet, tidings of their shadow you bear, hmm?\"",
              "",
              "Your choices: ",
              "1. \"Those Germans-the Nazis-are monsters. They've waged war and killed millions.\"",
              "2. \"They're exploiting you. They'll strip this valley bare and leave nothing behind.\"",
              ""
            ]
          )
  | otherwise =
      (st, ["There's no one here to talk to.", ""])

processCreatureChoice :: String -> GameState -> (GameState, Lines)
processCreatureChoice choice st
  | choice == "1" || choice == "2" =
      let newEntities = filter ((/= "creature") . entityName) (entitiesAt Ruins st)
          updatedLocEntities = map (\(loc, ents) -> if loc == Ruins then (loc, newEntities) else (loc, ents)) (locationEntities st)
          st1 = st {locationEntities = updatedLocEntities}
          st2 = removeTask "awaiting_creature_choice" st1
          st3 = addTask "ambush_beginning" st2
          responseLine =
            if choice == "1"
              then "You: \"Those Germans-the Nazis-are monsters. They've waged war and killed millions.\"\nCreature: \"Malice such, perceived it not, we did. Blinded us, our hospitality has, to their stain.\""
              else "You: \"They're exploiting you. They'll strip this valley bare and leave nothing behind.\"\nCreature: \"Cloaked in deception, they are, then. Harmony we cherish, yet stirred by this threat, we are. Counsel, what offer you?\""
       in ( st3,
            [ responseLine,
              "*Before you can respond, the air splits with the roar of engines and sharp, guttural shouts. The ground trembles faintly-a prelude to chaos.*",
              "Creature: \"True, if what you say is, run immediately, you must. Farewell, my friends.\"",
              "*The creature dissolves into the air, leaving you confused and on edge.*",
              "",
              "The Nazis lock eyes on you, their motorcycles skidding to a halt in a crescent of dust and menace.",
              "Their leader, a wiry man with a scar slashing across his cheek, leaps off his bike, his black uniform pristine despite the grime of the valley.",
              "He levels a Luger at you, his voice a guttural snarl that cuts through the humid air.",
              "",
              "Nazi Leader: \"Halt! Amerikanische Spione! Werft die Waffen nieder!\"",
              "Clara (whispering): \"They think we are spies. They've got us wrong, but I doubt they'll listen to reason.\"",
              "",
              "The air thickens with tension as the Nazis fan out, their boots crunching on the gravel, rifles glinting in the bioluminescent glow.",
              "Above, a flying saucer hums into view, its searchlight slicing through the foliage like a predator's gaze.",
              "Time slows-your heart pounds, and the valley's beauty fades behind the cold reality of danger.",
              ""
            ]
          )
  | otherwise =
      (st, ["Invalid choice - enter 1 or 2.", ""])

getRandomPistolOutcome :: GameState -> IO (GameState, Lines)
getRandomPistolOutcome st = do
  randomSuccess <- randomRIO (0.0, 1.0) :: IO Double
  let st1 = removeFromInventory (fromJust $ findEntity "pistol" st) st
      st2 = removeTask "fight" st1
      st3 = addTask "after_fight" st2

  if randomSuccess > 0.5
    then
      pure
        ( st3,
          [ "You hand the PISTOL to Clara.",
            "She aims the old Mauser and fires-a sharp crack echoes through the valley, the shot strikes one of the soldiers, who collapses with a cry.",
            "The Nazis roar in fury, their rifles spitting fire in response.",
            "Bullets chip the rock, showering you with dust and shards.",
            "The leader bellows, his voice thick with venom:",
            "",
            "Nazi Leader: \"Ihr wagt es, uns herauszufordern? Euer Blut wird dieses Tal beflecken!\"",
            ""
          ]
        )
    else
      pure
        ( st3,
          [ "You hand the PISTOL to Clara.",
            "She aims the old Mauser and pulls the trigger, but it jams with a dull click, refusing to fire.",
            "The Nazis burst into mocking laughter, their sneers cutting through the air.",
            "The leader steps forward, his voice dripping with scorn:",
            "",
            "Nazi Leader: \"Ihr erbarmlichen Narren! Dafur werdet ihr sterben!\"",
            ""
          ]
        )

use :: String -> GameState -> Maybe (GameState, Lines)
use item st = case map toLower item of
  "pistol"
    | isInInventory "pistol" st && "fight" `elem` tasks st ->
        Just (st, ["Clara: \"Hide behind the ROCK, now!\"", ""])
  "radio"
    | isInInventory "radio" st && "tunnel" `elem` tasks st ->
        Just
          ( removeTask "tunnel" $ addTask "radio" st,
            [ "The RADIO crackles in your hands, its three dials labeled A, B, and C glinting faintly in the dim light of the crash site.",
              "",
              "Each dial can be set to a number between 1 and 9.",
              "A faded, crumpled NOTE taped to the side reads: \"Marine Corps Frequency: Alpha-Bravo-Charlie.\"",
              "The wind howls outside, urging you to hurry.",
              ""
            ]
          )
  "radio"
    | isInInventory "radio" st && "radio" `elem` tasks st ->
        Just
          ( st,
            [ "You grip the RADIO and start adjusting the dials to tune the frequency.",
              "What do you set dial A to? (Enter a number)",
              ""
            ]
          )
  "radio"
    | isInInventory "radio" st ->
        Just
          ( st,
            [ "The radio has no use here, this place blocks the signal.",
              ""
            ]
          )
  "pistol"
    | isInInventory "pistol" st ->
        Just
          ( st,
            [ "Who do you want me to shoot, you psycho?",
              ""
            ]
          )
  "geiger"
    | isInInventory "geiger" st ->
        Just
          ( st,
            [ "Radiation levels - normal.",
              ""
            ]
          )
  _ -> Nothing

goSpecial :: String -> GameState -> Maybe (GameState, Lines)
goSpecial place st = case map toLower place of
  "woods"
    | (currentLocation st == Ruins || currentLocation st == City) && "woods" `elem` tasks st ->
        let st1 = addTask "act_finished" $ removeTask "woods" st
         in Just
              ( st1,
                [ "You sprint into the dense forest, branches snapping underfoot as you weave through the shadows.",
                  "The Nazis' shouts fade briefly-you dare to hope-until the sky hums with menace.",
                  "A flying saucer descends, its beam of light slashing through the canopy like a blade, pinning you in its merciless glare.",
                  "",
                  "Nazi Pilot (over loudspeaker): \"Kein Entkommen, ihr Narren! Das Reich sieht alles!\"",
                  "Riflemen emerge from the trees, their grips iron as they drag you back to the group. They bind your hands with coarse rope and march you toward the CITY, their motorcycles roaring triumphantly.",
                  "",
                  "The CITY looms ahead, its dark spires piercing the bioluminescent sky like ",
                  "jagged teeth. Clara stumbles beside you, her face pale but defiant, though her eyes betray a flicker of fear.",
                  "",
                  "You steal a glance at the leader, his scar twisting as he smirks, satisfied with ",
                  "his prize. What awaits in the CITY? Interrogation? Imprisonment? Or something ",
                  "far worse, tied to the secrets buried in this impossible valley? The questions ",
                  "gnaw at you, but answers remain elusive, shrouded in the same mystery that",
                  "cloaks this hidden world.",
                  "",
                  "As the CITY gates creak open, swallowing you into its shadowed maw, one thought",
                  "lingers: this is not the end, but a dark new beginning. Your fate hangs in the ",
                  "balance, and the next chapter of your journey waits just beyond the horizon.",
                  "",
                  "TO BE CONTINUED...",
                  ""
                ]
              )
  "rock"
    | (currentLocation st == Ruins || currentLocation st == City) && isInInventory "pistol" st && "hide" `elem` tasks st ->
        let st1 = removeTask "hide" $ addTask "fight" st
            st2 = st1 {currentLocation = Rock}
         in Just
              ( st2,
                [ "You dive behind a jagged boulder, its surface slick with glowing moss, your breath ragged as you press against the cold stone.",
                  ""
                ]
              )
  "tree"
    | currentLocation st == Ledge && "tree" `elem` tasks st ->
        let st1 = st {currentLocation = Tree}
            st2 = removeTask "tree" st1
         in Just
              ( st2,
                [ "You approach the towering TREE, its presence both majestic and unsettling.",
                  "Thick vines and sturdy branches form a natural ladder, inviting you to climb into its heights.",
                  ""
                ]
              )
  "ruins"
    | currentLocation st == Tree ->
        let st1 = st {currentLocation = Ruins}
         in Just
              ( st1,
                [ "You weave through the dense undergrowth toward the RUINS, their stone facades echoing the grandeur of Egypt's pyramids or the jungle temples of South America, yet twisted with an alien flair.",
                  "Intricate carvings of starships and celestial beings adorn the walls, hinting at a history far beyond human understanding.",
                  "As you step deeper, a tall, slender figure emerges, its luminous eyes studying you with quiet intrigue. The CREATURE gestures gracefully, inviting conversation.",
                  ""
                ]
              )
  "city"
    | currentLocation st == Tree ->
        let st1 = st {currentLocation = City}
            st2 = addTask "ambush_beginning" st1
         in Just
              ( st2,
                [ "You set off toward the CITY, its ominous skyline growing sharper with each step. Before you reach its perimeter, the growl of engines cuts through the stillness.",
                  "A division of Nazis on motorcycles bursts into view, their dust trails rising like storm clouds. Clara mutters under her breath, \"Looks like we've got company-and they don't seem friendly.\"",
                  "",
                  "The Nazis lock eyes on you, their motorcycles skidding to a halt in a crescent of dust and menace.",
                  "Their leader, a wiry man with a scar slashing across his cheek, leaps off his bike, his black uniform pristine despite the grime of the valley.",
                  "He levels a Luger at you, his voice a guttural snarl that cuts through the humid air.",
                  "",
                  "Nazi Leader: \"Halt! Amerikanische Spione! Werft die Waffen nieder!\"",
                  "Clara (whispering): \"They think we are spies. They've got us wrong, but I doubt they'll listen to reason.\"",
                  "",
                  "The air thickens with tension as the Nazis fan out, their boots crunching on the gravel, rifles glinting in the bioluminescent glow.",
                  "Above, a flying saucer hums into view, its searchlight slicing through the foliage like a predator's gaze.",
                  "Time slows-your heart pounds, and the valley's beauty fades behind the cold reality of danger.",
                  ""
                ]
              )
  "tunnel"
    | (currentLocation st == Ruins || currentLocation st == City) && "tunnel" `elem` tasks st ->
        let st1 =
              if isInInventory "radio" st
                then addTask "tunnel" $ st {currentLocation = Tunnel}
                else addTask "act_finished" $ st {currentLocation = Tunnel}
            gameOverMsg =
              if not (isInInventory "radio" st)
                then
                  [ "You bolt through the undergrowth, the Nazis' shouts and revving engines hot on your heels.",
                    "Thorns snag your clothes, tearing at your skin as you burst through the TUNNEL exit and emerge at the crash site, winded and desperate.",
                    "The icy wind bites at your face, a cruel reminder of the surface's hostility.",
                    "",
                    "You and Clara huddle in the wreckage, the valley's secrets slipping away as the cold closes in.",
                    "Survival hangs by a thread, your fate uncertain.",
                    "GAME OVER.",
                    ""
                  ]
                else
                  [ "You bolt through the undergrowth, the Nazis' shouts and revving engines hot on your heels.",
                    "Thorns snag your clothes, tearing at your skin as you burst through the TUNNEL exit and emerge at the crash site, winded and desperate.",
                    "The icy wind bites at your face, a cruel reminder of the surface's hostility.",
                    ""
                  ]
         in Just (st1, gameOverMsg)
  "ledge"
    | currentLocation st == Tree -> Just (st {currentLocation = Ledge}, ["You climb back down to the ledge.", ""])
  _ -> Nothing

processRadioInput :: GameState -> IO (GameState, Lines)
processRadioInput st = do
  putStrLn "Set dial A to: "
  aStr <- getLine
  putStrLn "Set dial B to: "
  bStr <- getLine
  putStrLn "Set dial C to: "
  cStr <- getLine

  let a = read aStr :: Int
      b = read bStr :: Int
      c = read cStr :: Int

  return (tuneRadio st a b c)

stepA3 :: GameState -> Command -> IO (GameState, Lines)
stepA3 st _
  | st `hasTask` "act_finished" =
      pure (st, ["You've already finished this act. Type \"quit\" to exit.", ""])
stepA3 st CmdQuit = exitSuccess
stepA3 st CmdLook =
  let out = case currentLocation st of
        Ledge ->
          if st `hasTask` "tree" && not (st `hasExamined` "ledge")
            then "Ahead looms a massive TREE, its gnarled trunk wider than a barn, its branches clawing toward the cavern's glowing ceiling.\nBioluminescent moss clings to its bark, pulsing faintly, while its leaves shimmer with an unearthly light, swaying as if whispering secrets to the wind."
            else ledgeDesc
        Tree -> treeDesc
        Ruins -> ruinsDesc
        City -> cityDesc
        Rock -> rockDesc
        Tunnel -> tunnelDesc
        _ -> "You see nothing special."
   in do
        let st' =
              if currentLocation st == Ledge && st `hasTask` "tree"
                then markExamined "ledge" st
                else st
        pure (st', [out, ""])
stepA3 st CmdInventory =
  let inv = inventory st
   in if null inv
        then pure (st, ["You are not carrying anything.", ""])
        else pure (st, "You are carrying:" : map entityName inv ++ [""])
stepA3 st CmdHint = pure (st, [getHint st, ""])
stepA3 st CmdInstructions = pure (st, instructionsText)
stepA3 st (CmdGo place) =
  case goSpecial (map toLower place) st of
    Just result -> pure result
    Nothing ->
      case parseLocation place of
        Unknown -> pure (st, ["Unknown place: " ++ place, ""])
        loc ->
          if canMove (currentLocation st) loc
            then
              let st' = st {currentLocation = loc}
                  out = describeLocation loc
               in pure (st', [out, ""])
            else pure (st, ["You can't go to " ++ place ++ " from here.", ""])
stepA3 st (CmdExamine obj) =
  case examine (map toLower obj) st of
    Just result -> pure result
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
              ["I can't see " ++ obj ++ " here or there's nothing special about it.", ""]
            )
stepA3 st (CmdTalk person)
  | map toLower person == "clara" = pure (talkClara st)
  | map toLower person == "creature" = pure (talkCreature st)
  | otherwise = pure (st, ["There's no one here to talk to by that name.", ""])
stepA3 st (CmdTake obj) =
  let name = map toLower obj
      inv = isInInventory name st
      here = findHere name st
      notHere = pure (st, ["I don't see " ++ obj ++ " here.", ""])
   in if inv
        then pure (st, ["You're already holding it!", ""])
        else case here of
          Nothing -> notHere
          Just e -> pure (addToInventory e st, ["You take the " ++ entityName e ++ ".", ""])
-- Separate step for droping pistol because of random number generation
stepA3 st (CmdDrop "pistol")
  | st `hasItem` "pistol" && st `hasTask` "fight" && currentLocation st == Rock =
      getRandomPistolOutcome st
stepA3 st (CmdDrop obj) =
  let name = map toLower obj
   in if not (isInInventory name st)
        then pure (st, ["You aren't carrying that!", ""])
        else
          let Just e = findEntity name st
              st' = removeFromInventory e st
           in pure (st', ["OK.", ""])
-- Separate step for using pistol because of random number generation
stepA3 st (CmdUse "pistol")
  | isInInventory "pistol" st && "fight" `elem` tasks st && currentLocation st == Rock =
      getRandomPistolOutcome st
stepA3 st (CmdUse obj) =
  case use (map toLower obj) st of
    Just result ->
      if map toLower obj == "radio" && st `hasTask` "radio"
        then processRadioInput st
        else pure result
    Nothing ->
      if isInInventory (map toLower obj) st
        then pure (st, ["I can't use that right now.", ""])
        else pure (st, ["I don't have it or I can't use it.", ""])
stepA3 st CmdUnknown = pure (st, ["Unknown command.", ""])
stepA3 st CmdNext
  | st `hasTask` "act_finished" =
      pure (st, ["Act 3 complete! The end of the game.", ""])
  | otherwise =
      pure (st, ["You need to finish this act first."])

gameLoop :: GameState -> IO PlayerState
gameLoop st = do
  line <- readCommand
  let cmdLine = map toLower line
      args = words cmdLine
      cmd = parseCommand args

  (st', out) <-
    if "awaiting_ledge_choice" `elem` tasks st
      then return (processLedgeChoice cmdLine st)
      else
        if "awaiting_creature_choice" `elem` tasks st
          then return (processCreatureChoice cmdLine st)
          else
            if "awaiting_ambush_choice" `elem` tasks st
              then return (processAmbushChoice cmdLine st)
              else stepA3 st cmd

  printLines out

  case cmd of
    CmdQuit -> pure (extractPlayerState st')
    CmdNext | "act_finished" `elem` tasks st' -> pure (extractPlayerState st')
    _ -> gameLoop st'

startAct3 :: PlayerState -> IO PlayerState
startAct3 ps = do
  let invItems = inventory_ ps
      st = initialState {inventory = invItems}

  printLines act3Prolog
  printLines [ledgeDesc, ""]

  gameLoop st
