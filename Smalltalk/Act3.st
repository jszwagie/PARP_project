"Act 3: Into the Heart of the Unknown"

Object subclass: #Act3
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    category: 'Game-Acts'!

!Act3 class methodsFor: 'game content'!
prologText
    ^#(
        'ACT 3: INTO THE HEART OF THE UNKNOWN'
        ''
        'You and Clara carefully climb down from the ledge, your boots sinking into the soft, mossy ground.'
        'The valley pulses with life-chirping insects fill the air, leaves rustle in a gentle breeze, and the distant roar of an unseen beast sends a shiver down your spine.'
        'The memory of your crash-landed supplies lingers, a heavy burden as you take your first cautious steps into this strange, uncharted world.'
        ''
    )!

!Act3 class methodsFor: 'initialization'!
createInitialState
    | state entities |
    state := GameState new.
    state currentLocation: Location ledge.
    state addTask: 'ledge_talk'.

    entities := Dictionary new.

    entities at: Location ledge put: (OrderedCollection with:
        (Entity type: EntityType person name: 'clara' description: 'Clara stands beside you, looking out over the hidden valley.' takeable: false)).

    entities at: Location tree put: OrderedCollection new.

    entities at: Location ruins put: (OrderedCollection with:
        (Entity type: EntityType person name: 'creature' description: 'A tall, slender figure with luminous eyes studying you with quiet intrigue.' takeable: false)).

    entities at: Location tunnel put: OrderedCollection new.
    entities at: Location city put: OrderedCollection new.
    entities at: Location rock put: OrderedCollection new.

    state locationEntities: entities.
    ^state!

!Act3 class methodsFor: 'movement'!
canMove: fromLocation to: toLocation
    fromLocation = Location ledge ifTrue: [^toLocation = Location tree].
    fromLocation = Location tree ifTrue: [
        ^toLocation = Location ledge or: [
            toLocation = Location ruins or: [
                toLocation = Location tunnel or: [
                    toLocation = Location city]]]].
    fromLocation = Location ruins ifTrue: [^toLocation = Location tree].
    fromLocation = Location tunnel ifTrue: [^toLocation = Location tree].
    fromLocation = Location city ifTrue: [^toLocation = Location tree].

    fromLocation = Location city   ifTrue: [ ^toLocation = Location rock or: [ toLocation = Location tree ] ].
    fromLocation = Location ruins  ifTrue: [ ^toLocation = Location rock or: [ toLocation = Location tree ] ].

    fromLocation = Location rock   ifTrue: [
        ^toLocation = Location city
            or: [ toLocation = Location ruins ] ].
    ^false!

!Act3 class methodsFor: 'descriptions'!
describeLocation: location
    location = Location ledge ifTrue: [
        ^'You both stand on a rocky ledge overlooking a hidden realm-an expansive, verdant valley cradled beneath Antarctica''s icy crust.
Bioluminescent plants emit a soft, ethereal glow, casting light across towering ferns and crystalline rivers that shimmer like liquid glass.
The air hangs warm and humid, thick with the scent of exotic blooms, a jarring contrast to the frozen desolation above.
Flying saucers, eerily similar to the wreck you stumbled upon, glide silently through the skies, their presence a quiet warning of something watchful and alive down here.'].

    location = Location tree ifTrue: [
        ^'From the tree''s upper branches, the valley sprawls before you in breathtaking detail.
To the east, ancient-looking RUINS emerge from the foliage-crumbling pyramids and temples etched with cryptic symbols, remnants of a lost civilization.
To the west, the stark silhouette of a CITY cuts through the greenery, its dark gray buildings festooned with swastika flags fluttering ominously in the breeze, their bold red and black stark against the muted stone.
Behind you, the TUNNEL exit gapes like a dark maw, leading back to the frozen surface-a lifeline or a trap, depending on your next move.'].

    location = Location ruins ifTrue: [
        ^'The ruins before you are a marvel of ancient architecture, reminiscent of Egypt''s pyramids or the jungle temples of South America, yet distinctly alien.
Crumbling stone facades are adorned with intricate carvings of starships and celestial beings, hinting at a civilization far beyond human comprehension.
The air here feels thick with history and unspoken secrets.'].

    location = Location tunnel ifTrue: [
        ^'The crash site lies in ruins, the plane''s twisted metal half-buried in snow.
The wind howls mercilessly, and the sky above is a bleak, unforgiving gray.
Your breath fogs in the frigid air, a stark contrast to the warmth of the hidden valley below.'].

    location = Location city ifTrue: [
        ^'The city cuts a stark silhouette against the valley''s greenery, its dark gray buildings rising like monolithic sentinels.
Swastika flags flutter ominously from every structure, their bold red and black stark against the muted stone.
The atmosphere is heavy with foreboding, as if the very walls are watching your every move.'].

    location = Location rock ifTrue: [
        ^'The massive boulder provides a makeshift shelter, its surface slick with glowing moss.
You press against the cold stone, your breath ragged as the Nazi patrol draws closer.
From this vantage point, you can see several Nazis, their faces twisted in determination as they search the area.'].

    ^'You see nothing special.'!

!Act3 class methodsFor: 'hints'!
getHint: gameState
    (gameState hasTask: 'act_finished') ifTrue: [
        ^'You''ve already finished this act. Type "quit" to exit the game.'].

    (gameState hasTask: 'woods') ifTrue: [
        ^'We should GO to the WOODS.'].

    (gameState currentLocation = Location tunnel and: [(gameState hasItem: 'radio') and: [gameState hasTask: 'tunnel']] ) ifTrue: [
        ^'I should USE the RADIO, as I said.'].

    (gameState currentLocation = Location rock and: [gameState hasTask: 'tunnel']) ifTrue: [
        ^'We should go to TUNNEL.'].

    (gameState currentLocation = Location rock and: [gameState hasTask: 'after_fight']) ifTrue: [
        ^'I should talk to Clara.'].

    (gameState currentLocation = Location rock and: [(gameState hasItem: 'pistol') and: [gameState hasTask: 'fight']] ) ifTrue: [
        ^'I should hand the PISTOL to Clara.'].

    (gameState hasTask: 'ambush_beginning') ifTrue: [
        ^'Maybe Clara knows what to do in this situation.'].

    (gameState hasTask: 'after_radio') ifTrue: [
        ^'I should talk to Clara.'].

    ((gameState hasTask: 'radio') and: [(gameState hasItem: 'radio') and: [gameState hasExamined: 'note']] ) ifTrue: [
        ^'The note says ''Four''s the square, Seven''s luck, Two''s pair.'' That could point to the settings for A, B, and C. The plaque might help confirm it.'].

    ((gameState hasTask: 'radio') and: [gameState hasItem: 'radio']) ifTrue: [
        ^'I need to tune the dials to the right numbers to reach the Marines. The NOTE or the RADIO might hold the key.'].

    ((gameState hasTask: 'hide') and: [gameState hasItem: 'pistol']) ifTrue: [
        ^'I should GO behind that ROCK.'].

    (gameState currentLocation = Location ledge and: [gameState hasTask: 'ledge_talk']) ifTrue: [
        ^'I should talk to Clara.'].

    (gameState currentLocation = Location ledge and: [(gameState hasTask: 'tree') and: [gameState hasExamined: 'ledge']] ) ifTrue: [
        ^'I think I could GO up on that TREE.'].

    (gameState currentLocation = Location ledge and: [gameState hasTask: 'tree']) ifTrue: [
        ^'I should find a high place for recon.'].

    (gameState currentLocation = Location tree) ifTrue: [
        ^'I should LOOK around and decide where to GO.'].

    (gameState currentLocation = Location ruins and: [(gameState entitiesAt: Location ruins) anySatisfy: [:e | e entityName = 'creature']]) ifTrue: [
        ^'Is the TALK the answer?'].

    (gameState currentLocation = Location ruins and: [gameState hasTask: 'hide']) ifTrue: [
        ^'I should hide behind that ROCK'].

    (gameState currentLocation = Location city and: [gameState hasTask: 'hide']) ifTrue: [
        ^'I should hide behind that ROCK'].

    (gameState currentLocation = Location ruins and: [gameState hasTask: 'tunnel']) ifTrue: [
        ^'I should GO to the TUNNEL'].

    (gameState currentLocation = Location city and: [gameState hasTask: 'tunnel']) ifTrue: [
        ^'I should GO to the TUNNEL'].

    (gameState currentLocation = Location ruins and: [gameState hasTask: 'woods']) ifTrue: [
        ^'I should GO to the WOODS'].

    (gameState currentLocation = Location city and: [gameState hasTask: 'woods']) ifTrue: [
        ^'I should GO to the WOODS'].

    ^'I should try to LOOK around to get my bearings.'!

!Act3 class methodsFor: 'special examine'!
examineSpecial: itemName gameState: gameState
    | newState response |
    newState := gameState deepCopy.
    response := OrderedCollection new.

    (itemName = 'creature' and: [(gameState currentLocation = Location ruins) and: [(gameState entitiesAt: Location ruins) anySatisfy: [:e | e entityName = 'creature']]] ) ifTrue: [
        newState markExamined: 'creature'.
        response addAll: #(
            'The creature stands tall and slender, its luminous eyes studying you with an intelligence that feels ancient.'
            'Its skin seems to shimmer faintly, and as you look closer, you realize it''s communicating directly into your mind-a melodic hum that bypasses your ears.'
            'It exudes an aura of wisdom and otherworldliness, as if it holds secrets older than time itself.'
            ''
        ).
        ^Array with: newState with: response].

    ((itemName = 'tree') and: [(gameState currentLocation = Location tree) or: [gameState currentLocation = Location ledge]]) ifTrue: [
        newState markExamined: 'tree'.
        response addAll: #(
            'The tree stands ancient and imposing, its roots plunging into the earth like the veins of the valley itself.'
            ''
        ).
        ^Array with: newState with: response].

    (itemName = 'radio' and: [(gameState hasTask: 'radio') and: [gameState isInInventory: 'radio']] ) ifTrue: [
        newState markExamined: 'radio'.
        response addAll: #(
            'The RADIO is a rugged military device, scratched and dented but still working.'
            'Each dial can be set to a number between 1 and 9.'
            'The dials click stiffly as you turn them. A small plaque beneath them reads: "Standard Marine Corps Protocol: A=Even, B=Prime, C=Square."'
            ''
        ).
        ^Array with: newState with: response].

    (itemName = 'note' and: [(gameState hasTask: 'radio') and: [gameState isInInventory: 'radio']] ) ifTrue: [
        newState markExamined: 'note'.
        response addAll: #(
            'The note is weathered, its ink blurred but readable: "Marine Corps Frequency: Alpha-Bravo-Charlie. Remember the code: Four''s the square, Seven''s luck, Two''s pair."'
            ''
        ).
        ^Array with: newState with: response].

    ^nil!

!Act3 class methodsFor: 'radio puzzle'!
tuneRadio: gameState a: a b: b c: c
    | newState response wrongCount |
    newState := gameState deepCopy.
    response := OrderedCollection new.

    wrongCount := (a = 2 ifTrue: [0] ifFalse: [1]) +
                  (b = 7 ifTrue: [0] ifFalse: [1]) +
                  (c = 4 ifTrue: [0] ifFalse: [1]).

    wrongCount = 0 ifTrue: [
        | radio |
        newState removeTask: 'radio'.
        radio := gameState findEntity: 'radio'.
        radio ifNotNil: [newState removeFromInventory: radio].
        newState addTask: 'after_radio'.
        newState currentLocation: Location cockpit.

        response addAll: #(
            'The RADIO hums as it locks onto a strong signal. A clear voice cuts through:'
            ''
            '"Mission 334, this is the 32nd Marine Corps. Coordinates received. Extraction team inbound. Over."'
            'Clara: "Copy that. We''ll hold tight. Over."'
            'Marine: "Copy that. We''re tracking your signal. Hold tight, over."'
            ''
            'The steady signal brings a flicker of relief amidst the chaos.'
            ''
            'After a tense wait, the roar of engines fills the air. A Marine transport plane descends through the snow, its lights cutting through the gloom.'
            'You and CLARA board, the warmth of the cabin a stark contrast to the biting cold.'
            'As the plane lifts off, a Marine hands you a stack of nondisclosure agreements.'
            ''
            'Marine: "Sign these. What you saw down there stays buried. Understood?"'
            ''
            'You nod, a heavy, unspoken weight settling over you.'
            'The valley''s mysteries fade into the distance, shrouded in silence, as the plane carries you away.'
            ''
        ).
        ^Array with: newState with: response].

    wrongCount = 1 ifTrue: [
        response addAll: #(
            'The RADIO picks up a faint Marine transmission, but it''s garbled:'
            ''
            '"Mission... [static]... coordinates... [static]... hold..."'
            'Clara: "Almost there, but it''s too weak. They won''t get our position like this."'
            ''
            'HINT: "The plaque mentions ''A=Even, B=Prime, C=Square.'' And the note says ''Four''s the square, Seven''s luck, Two''s pair''-could A be 2?"'
            ''
        ).
        ^Array with: newState with: response].

    wrongCount = 2 ifTrue: [
        response addAll: #(
            'A sharp burst of static erupts from the RADIO, followed by a chilling German voice:'
            ''
            '"Achtung! Feindliche Ubertragung entdeckt!"'
            'Clara: "That''s the Germans-they''ve intercepted us. We''ve got to fix this now!"'
            ''
            'HINT: "The plaque mentions ''A=Even, B=Prime, C=Square.'' And the note says ''Four''s the square, Seven''s luck, Two''s pair''-could A be 2?"'
            ''
        ).
        ^Array with: newState with: response].

    response addAll: #(
        'The RADIO hisses with static, a grating buzz drowning out any signal.'
        'You: "Just noise. This isn''t the right frequency."'
        ''
        'HINT: "Think it through: 2 is even, 7 is prime, and 4 ties to the square of 2. That matches all the clues."'
        ''
    ).
    ^Array with: newState with: response!

!Act3 class methodsFor: 'dialog'!
talkClara: gameState
    | newState response |
    newState := gameState deepCopy.
    response := OrderedCollection new.

    (gameState hasTask: 'after_radio') ifTrue: [
        newState addTask: 'act_finished'.
        response addAll: #(
            'You turn to Clara, her face illuminated by the dim cabin lights.'
            ''
            'You: "So, what-this is how it ends?"'
            'Clara: "Wake up!"'
            'You: "What?!"'
            'Clara: "WAKE UP!"'
            ''
            'Suddenly, a sharper voice breaks through the haze.'
            'Your wife: "Damn it, wake up! You''ll be late for your lectures!"'
            'You: "What? What lectures?"'
            'Your wife: "You were up late watching TV again. You''ve got to stop with those'
            'ridiculous pseudo-historical documentaries on FOCUS TV or TV4-they''re frying your brain."'
            ''
            'The Antarctic adventure dissolves like mist. You blink, disoriented, as the soft '
            'glow of your bedside lamp replaces the plane''s harsh lights. The hum of Warsaw''s '
            'morning traffic seeps through the window, a mundane rhythm far removed from the  '
            'valley''s eerie pulse. It was all a dream-a vivid fantasy spun from late-night '
            'television and a restless mind. You''re not an adventurer escaping a hidden'
            'world; you''re an ordinary professor at the Warsaw University of Technology, with'
            'lectures to deliver and papers to grade. Reality sinks in, familiar and unrelenting.'
            ''
            'You sit up, rubbing your eyes as the dream''s vivid details-Clara''s determined '
            'gaze, the snow-swept valley, the roar of the plane-slip away like sand through '
            'your fingers. Your wife moves about the room, muttering about your late-night '
            'habits, oblivious to the epic journey you''ve just imagined.'
            ''
            'Your wife: "Honestly, those conspiracy channels will be the death of you. Go to bed on time for once."'
            ''
            'You muster a faint smile, the last echoes of the dream fading into nothingness.'
            'The adventure is over, and the real world beckons.'
            ''
            'THE END'
            ''
        ).
        ^Array with: newState with: response].

    ((gameState hasTask: 'hide') and: [gameState currentLocation = Location ruins or: [gameState currentLocation = Location city]]) ifTrue: [
        response add: 'Clara: "Here, give me the pistol and get behind that ROCK-now!"'.
        response add: ''.
        ^Array with: newState with: response].

    ((gameState hasTask: 'tunnel') and: [gameState currentLocation = Location ruins or: [gameState currentLocation = Location city]]) ifTrue: [
        response add: 'Clara: "It''s a long shot, but let''s GO to the TUNNEL now!"'.
        response add: ''.
        ^Array with: newState with: response].

    ((gameState hasTask: 'tunnel') and: [(gameState currentLocation = Location tunnel) and: [(gameState isInInventory: 'radio') not]] ) ifTrue: [
        response add: 'Clara: "You should USE the RADIO"'.
        response add: ''.
        ^Array with: newState with: response].

    ((gameState hasTask: 'radio') and: [(gameState currentLocation = Location tunnel) and: [(gameState isInInventory: 'radio') not]] ) ifTrue: [
        response add: 'Clara: "You should USE the RADIO"'.
        response add: ''.
        ^Array with: newState with: response].

    ((gameState hasTask: 'woods') and: [gameState currentLocation = Location ruins or: [gameState currentLocation = Location city]]) ifTrue: [
        response add: 'Clara: "It''s a long shot, but let''s GO to the WOODS now!"'.
        response add: ''.
        ^Array with: newState with: response].

    (gameState hasTask: 'after_fight') ifTrue: [
        newState addTask: 'act_finished'.
        newState removeTask: 'after_fight'.
        response addAll: #(
            'You: "What did he say?"'
            'Clara: *breathing heavily* "Nothing good. I don''t know if we can get out of this alive."'
            'Clara: *shouting in fright* "Wir kapitulieren! Halt!"'
            ''
            'The soldiers cease fire, their eyes still burning with rage.'
            'They swarm closer, boots pounding the earth like war drums, and you''re wrestled to the ground, wrists bound tight with rough cord.'
            'Their treatment is brutal - fists and threats of execution, though they spare you for now, muttering darkly about your potential value.'
            'They march you toward the CITY, their motorcycles roaring triumphantly.'
            ''
            'The CITY looms ahead, its dark spires piercing the bioluminescent sky like '
            'jagged teeth. Clara stumbles beside you, her face pale but defiant, though her eyes betray a flicker of fear.'
            ''
            'You steal a glance at the leader, his scar twisting as he smirks, satisfied with '
            'his prize. What awaits in the CITY? Interrogation? Imprisonment? Or something '
            'far worse, tied to the secrets buried in this impossible valley? The questions '
            'gnaw at you, but answers remain elusive, shrouded in the same mystery that'
            'cloaks this hidden world.'
            ''
            'As the CITY gates creak open, swallowing you into its shadowed maw, one thought'
            'lingers: this is not the end, but a dark new beginning. Your fate hangs in the '
            'balance, and the next chapter of your journey waits just beyond the horizon.'
            ''
            'TO BE CONTINUED...'
            ''
        ).
        ^Array with: newState with: response].

    (gameState hasTask: 'ambush_beginning') ifTrue: [
        newState addTask: 'awaiting_ambush_choice'.
        newState removeTask: 'ambush_beginning'.
        response addAll: #(
            'You: "What''s our move? They''re closing in fast."'
            'Clara: "We''re outgunned and outmanned. Fight, run, or surrender-you decide, but make it quick!"'
            'Your choices:'
        ).
        (gameState hasItem: 'pistol') ifTrue: [
            response add: '1. "Let''s fight! I''ll hand you the PISTOL!"'].
        response add: '2. "Run for the TUNNEL.', (gameState isInInventory: 'radio' ifTrue: [' We can try the RADIO one more time!"'] ifFalse: ['"']).
        response add: '3. "We surrender. Maybe we can talk our way out."'.
        response add: '4. "Into the WOODS-lose them in the trees!"'.
        response add: ''.
        ^Array with: newState with: response].

    ((gameState currentLocation = Location ledge) and: [(gameState hasTalked: 'clara' event: 'ledge') not]) ifTrue: [
        newState addTask: 'awaiting_ledge_choice'.
        newState markTalked: 'clara' event: 'ledge'.
        response addAll: #(
            'Clara: "This place... it''s like stepping into a dream. Or maybe a nightmare-I can''t decide."'
            'You: "It''s incredible-Byrd wasn''t exaggerating in that diary."'
            'Clara: "Sure, but that diary was written before the war. No mention of Nazis anywhere in it. Do you think they beat us to this discovery?"'
            'Your choices:'
            '1. "Maybe they found it during their Antarctic expeditions in the ''30s."'
            '2. "Or they stumbled across it after the war, looking for a place to hide."'
            ''
        ).
        ^Array with: newState with: response].

    (gameState currentLocation = Location ruins and: [(gameState entitiesAt: Location ruins) anySatisfy: [:e | e entityName = 'creature']]) ifTrue: [
        response add: 'Clara: "Do you think we should TALK to it?"'.
        response add: ''.
        ^Array with: newState with: response].

    (gameState currentLocation = Location tree) ifTrue: [
        response add: 'Clara: Do you see anything interesting?'.
        response add: ''.
        ^Array with: newState with: response].

    response add: 'There''s no one here to talk to.'.
    response add: ''.
    ^Array with: newState with: response!

talkCreature: gameState
    | newState response |
    newState := gameState deepCopy.
    response := OrderedCollection new.

    (gameState currentLocation = Location ruins and: [(gameState entitiesAt: Location ruins) anySatisfy: [:e | e entityName = 'creature']]) ifTrue: [
        newState addTask: 'awaiting_creature_choice'.
        response addAll: #(
            '*The creature''s voice resonates in your mind, a melodic hum that bypasses your ears entirely.*'
            ''
            'Creature: "Wanderers, greetings. Sentinel of this realm, I am, keeper of wisdom older than your civilization, hmm.'
            'Answers you seek, yes? Give them to you, I shall.'
            'Tied to what you call ''Atlantis,'' our kin are, though lost to your tongue, our true name is.'
            'Arrived, the ones you call ''Germans'' did, speaking of a great calamity they fled. Stewards of peace we are, granted them refuge, we did. Yet, tidings of their shadow you bear, hmm?"'
            ''
            'Your choices: '
            '1. "Those Germans-the Nazis-are monsters. They''ve waged war and killed millions."'
            '2. "They''re exploiting you. They''ll strip this valley bare and leave nothing behind."'
            ''
        ).
        ^Array with: newState with: response].

    response add: 'There''s no one here to talk to.'.
    response add: ''.
    ^Array with: newState with: response!

!Act3 class methodsFor: 'choice processing'!
processChoice: choice gameState: gameState choiceType: choiceType
    | newState response |
    newState := gameState deepCopy.
    response := OrderedCollection new.

    choiceType = 'ledge' ifTrue: [
        newState removeTask: 'ledge_talk'.
        newState removeTask: 'awaiting_ledge_choice'.
        newState addTask: 'tree'.

        choice = '1' ifTrue: [
            response addAll: #(
                'You: "Maybe they found it during their Antarctic expeditions in the ''30s."'
                'Clara: "That could explain those flying saucers. They''ve had decades to dig in, hidden from the rest of the world."'
            )]
        ifFalse: [
            response addAll: #(
                'You: "Or they stumbled across it after the war, looking for a place to hide."'
                'Clara: "Hide? More like regroup. This could be their secret fortress, waiting for the right moment to strike back."'
            )].

        response addAll: #(
            ''
            'You stand together, awestruck by the valley''s haunting beauty.'
            'The bioluminescent flora bathes the landscape in a shimmering, otherworldly hue, while the faint hum of the valley''s life-chirps, rustles, and distant cries-wraps around you like a living tapestry.'
            'It''s a paradise untouched by time, yet the shadow of danger looms just out of sight.'
            ''
            'Clara: *gasps* "Okay, enough gawking. If we want to survive this, we need a better lay of the land. Let''s find a high spot for reconnaissance."'
            ''
        ).

        choice = '1' ifFalse: [choice = '2' ifFalse: [
            response := OrderedCollection with: 'Invalid choice - enter 1 or 2.' with: ''.
            ^Array with: gameState with: response]].

        ^Array with: newState with: response].

    choiceType = 'creature' ifTrue: [
        | ruinsEntities updatedEntities |
        ruinsEntities := gameState entitiesAt: Location ruins.
        updatedEntities := ruinsEntities reject: [:e | e entityName = 'creature'].
        newState locationEntities at: Location ruins put: updatedEntities.
        newState removeTask: 'awaiting_creature_choice'.
        newState addTask: 'ambush_beginning'.

        choice = '1' ifTrue: [
            response add: 'You: "Those Germans-the Nazis-are monsters. They''ve waged war and killed millions."
Creature: "Malice such, perceived it not, we did. Blinded us, our hospitality has, to their stain."']
        ifFalse: [choice = '2' ifTrue: [
            response add: 'You: "They''re exploiting you. They''ll strip this valley bare and leave nothing behind."
Creature: "Cloaked in deception, they are, then. Harmony we cherish, yet stirred by this threat, we are. Counsel, what offer you?"']
        ifFalse: [
            response add: 'Invalid choice - enter 1 or 2.'.
            ^Array with: gameState with: response]].

        response addAll: #(
            '*Before you can respond, the air splits with the roar of engines and sharp, guttural shouts. The ground trembles faintly-a prelude to chaos.*'
            'Creature: "True, if what you say is, run immediately, you must. Farewell, my friends."'
            '*The creature dissolves into the air, leaving you confused and on edge.*'
            ''
            'The Nazis lock eyes on you, their motorcycles skidding to a halt in a crescent of dust and menace.'
            'Their leader, a wiry man with a scar slashing across his cheek, leaps off his bike, his black uniform pristine despite the grime of the valley.'
            'He levels a Luger at you, his voice a guttural snarl that cuts through the humid air.'
            ''
            'Nazi Leader: "Halt! Amerikanische Spione! Werft die Waffen nieder!"'
            'Clara (whispering): "They think we are spies. They''ve got us wrong, but I doubt they''ll listen to reason."'
            ''
            'The air thickens with tension as the Nazis fan out, their boots crunching on the gravel, rifles glinting in the bioluminescent glow.'
            'Above, a flying saucer hums into view, its searchlight slicing through the foliage like a predator''s gaze.'
            'Time slows-your heart pounds, and the valley''s beauty fades behind the cold reality of danger.'
            ''
        ).
        ^Array with: newState with: response].

    choiceType = 'ambush' ifTrue: [
        newState removeTask: 'awaiting_ambush_choice'.

        choice = '1' ifTrue: [
            (gameState isInInventory: 'pistol') ifTrue: [
                newState addTask: 'hide'.
                response addAll: #(
                    'You: "Let''s fight! I''ll hand you the PISTOL!"'
                    'Clara: "Here, give me the pistol and get behind that ROCK-now!"'
                    ''
                ).
                ^Array with: newState with: response]
            ifFalse: [
                response add: 'You don''t have a pistol to fight with!'.
                ^Array with: gameState with: response]].

        choice = '2' ifTrue: [
            newState addTask: 'tunnel'.
            gameState isInInventory: 'radio'
                ifTrue: [response add: 'You: "To the TUNNEL - move. We can try the RADIO one more time!"']
                ifFalse: [response add: 'You: "To the TUNNEL - move."'].
            gameState isInInventory: 'radio'
                ifTrue: [response add: 'Clara: "It''s a long shot, but let''s go!"']
                ifFalse: [response add: 'Clara: "Without the RADIO, we''ll just freeze out there. Terrible plan, but I''m with you."'].
            response add: ''.
            ^Array with: newState with: response].

        choice = '3' ifTrue: [
            newState addTask: 'act_finished'.
            response addAll: #(
                'You: "We surrender. Maybe we can talk our way out."'
                'You raise your hands slowly. Clara mirrors your movement and calls out to the soldiers:'
                'Clara: "Wir kapitulieren! Kein Problem."'
                ''
                'The Nazis lower their rifles slightly, though their glares remain sharp as knives. The leader smirks, holstering his Luger with a flourish.'
                ''
                'Nazi Leader: "Kluger Schachzug, Amerikaner. Unser Kommandant mochte Sie unbedingt sehen."'
                'They bind your hands with coarse rope, the knots biting into your wrists, and march you toward the CITY, their motorcycles roaring triumphantly.'
                ''
                'The CITY looms ahead, its dark spires piercing the bioluminescent sky like '
                'jagged teeth. Clara stumbles beside you, her face pale but defiant, though her eyes betray a flicker of fear.'
                ''
                'You steal a glance at the leader, his scar twisting as he smirks, satisfied with '
                'his prize. What awaits in the CITY? Interrogation? Imprisonment? Or something '
                'far worse, tied to the secrets buried in this impossible valley? The questions '
                'gnaw at you, but answers remain elusive, shrouded in the same mystery that'
                'cloaks this hidden world.'
                ''
                'As the CITY gates creak open, swallowing you into its shadowed maw, one thought'
                'lingers: this is not the end, but a dark new beginning. Your fate hangs in the '
                'balance, and the next chapter of your journey waits just beyond the horizon.'
                ''
                'TO BE CONTINUED...'
                ''
            ).
            ^Array with: newState with: response].

        choice = '4' ifTrue: [
            newState addTask: 'woods'.
            response addAll: #(
                'You: "Come on, to the WOODS-GO!"'
                ''
            ).
            ^Array with: newState with: response].

        response add: 'Invalid choice - enter 1, 2, 3, or 4.'.
        response add: ''.
        ^Array with: gameState with: response].

    ^Array with: gameState with: response!

!Act3 class methodsFor: 'special actions'!
useRadio: gameState
    | newState response a b c |
    newState := gameState deepCopy.
    response := OrderedCollection new.

((gameState isInInventory: 'radio') and: [gameState hasTask: 'tunnel']) ifTrue: [
        newState removeTask: 'tunnel'.
        newState addTask: 'radio'.
        response addAll: #(
            'The RADIO crackles in your hands, its three dials labeled A, B, and C glinting faintly in the dim light of the crash site.'
            ''
            'Each dial can be set to a number between 1 and 9.'
            'A faded, crumpled NOTE taped to the side reads: "Marine Corps Frequency: Alpha-Bravo-Charlie."'
            'The wind howls outside, urging you to hurry.'
            ''
        ).
        ^Array with: newState with: response].

    ((gameState isInInventory: 'radio') and: [gameState hasTask: 'radio']) ifTrue: [
        Transcript show: 'Set dial A to: '; cr.
        a := (FileStream stdin nextLine) asInteger.
        Transcript show: 'Set dial B to: '; cr.
        b := (FileStream stdin nextLine) asInteger.
        Transcript show: 'Set dial C to: '; cr.
        c := (FileStream stdin nextLine) asInteger.

        ^self tuneRadio: gameState a: a b: b c: c].

    (gameState isInInventory: 'radio') ifTrue: [
        response addAll: #(
            'The radio has no use here, this place blocks the signal.'
            ''
        ).
        ^Array with: newState with: response].

    ^nil!

goSpecial: destination gameState: gameState
    | newState response |
    newState := gameState deepCopy.
    response := OrderedCollection new.

    destination = 'woods' ifTrue: [
        ((gameState currentLocation = Location ruins or: [gameState currentLocation = Location city]) and: [gameState hasTask: 'woods']) ifTrue: [
            newState addTask: 'act_finished'.
            newState removeTask: 'woods'.
            response addAll: #(
                'You sprint into the dense forest, branches snapping underfoot as you weave through the shadows.'
                'The Nazis'' shouts fade briefly-you dare to hope-until the sky hums with menace.'
                'A flying saucer descends, its beam of light slashing through the canopy like a blade, pinning you in its merciless glare.'
                ''
                'Nazi Pilot (over loudspeaker): "Kein Entkommen, ihr Narren! Das Reich sieht alles!"'
                'Riflemen emerge from the trees, their grips iron as they drag you back to the group. They bind your hands with coarse rope and march you toward the CITY, their motorcycles roaring triumphantly.'
                ''
                'The CITY looms ahead, its dark spires piercing the bioluminescent sky like '
                'jagged teeth. Clara stumbles beside you, her face pale but defiant, though her eyes betray a flicker of fear.'
                ''
                'You steal a glance at the leader, his scar twisting as he smirks, satisfied with '
                'his prize. What awaits in the CITY? Interrogation? Imprisonment? Or something '
                'far worse, tied to the secrets buried in this impossible valley? The questions '
                'gnaw at you, but answers remain elusive, shrouded in the same mystery that'
                'cloaks this hidden world.'
                ''
                'As the CITY gates creak open, swallowing you into its shadowed maw, one thought'
                'lingers: this is not the end, but a dark new beginning. Your fate hangs in the '
                'balance, and the next chapter of your journey waits just beyond the horizon.'
                ''
                'TO BE CONTINUED...'
                ''
            ).
            ^Array with: newState with: response]].

    destination = 'rock' ifTrue: [
        ((gameState currentLocation = Location ruins or: [gameState currentLocation = Location city]) and: [gameState hasTask: 'hide']) ifTrue: [
            newState removeTask: 'hide'.
            newState addTask: 'fight'.
            newState currentLocation: Location rock.
            response addAll: #(
                'You dive behind a jagged boulder, its surface slick with glowing moss, your breath ragged as you press against the cold stone.'
                ''
            ).
            ^Array with: newState with: response]].

    destination = 'tree' ifTrue: [
        (gameState currentLocation = Location ledge and: [gameState hasTask: 'tree']) ifTrue: [
            newState currentLocation: Location tree.
            newState removeTask: 'tree'.
            response addAll: #(
                'You approach the towering TREE, its presence both majestic and unsettling.'
                'Thick vines and sturdy branches form a natural ladder, inviting you to climb into its heights.'
                ''
            ).
            ^Array with: newState with: response]].

    destination = 'ruins' ifTrue: [
        (gameState currentLocation = Location tree) ifTrue: [
            newState currentLocation: Location ruins.
            response addAll: #(
                'You weave through the dense undergrowth toward the RUINS, their stone facades echoing the grandeur of Egypt''s pyramids or the jungle temples of South America, yet twisted with an alien flair.'
                'Intricate carvings of starships and celestial beings adorn the walls, hinting at a history far beyond human understanding.'
                'As you step deeper, a tall, slender figure emerges, its luminous eyes studying you with quiet intrigue. The CREATURE gestures gracefully, inviting conversation.'
                ''
            ).
            ^Array with: newState with: response]].

    destination = 'city' ifTrue: [
        (gameState currentLocation = Location tree) ifTrue: [
            newState currentLocation: Location city.
            newState addTask: 'ambush_beginning'.
            response addAll: #(
                'You set off toward the CITY, its ominous skyline growing sharper with each step. Before you reach its perimeter, the growl of engines cuts through the stillness.'
                'A division of Nazis on motorcycles bursts into view, their dust trails rising like storm clouds. Clara mutters under her breath, "Looks like we''ve got company-and they don''t seem friendly."'
                ''
                'The Nazis lock eyes on you, their motorcycles skidding to a halt in a crescent of dust and menace.'
                'Their leader, a wiry man with a scar slashing across his cheek, leaps off his bike, his black uniform pristine despite the grime of the valley.'
                'He levels a Luger at you, his voice a guttural snarl that cuts through the humid air.'
                ''
                'Nazi Leader: "Halt! Amerikanische Spione! Werft die Waffen nieder!"'
                'Clara (whispering): "They think we are spies. They''ve got us wrong, but I doubt they''ll listen to reason."'
                ''
                'The air thickens with tension as the Nazis fan out, their boots crunching on the gravel, rifles glinting in the bioluminescent glow.'
                'Above, a flying saucer hums into view, its searchlight slicing through the foliage like a predator''s gaze.'
                'Time slows-your heart pounds, and the valley''s beauty fades behind the cold reality of danger.'
                ''
            ).
            ^Array with: newState with: response]].

    destination = 'tunnel' ifTrue: [
        (gameState currentLocation = Location tree) ifTrue: [
            (gameState hasTalked: 'clara' event: 'tunnel') ifTrue: [
                response add: 'We can''t GO to TUNNEL. We went too far.'.
                response add: ''.
                ^Array with: newState with: response]
            ifFalse: [
                newState addTask: 'awaiting_tunnel_choice'.
                newState markTalked: 'clara' event: 'tunnel'.
                response addAll: #(
                    'Clara grabs your sleeve, her grip tight.'
                    'Clara: "Hold on! We can''t just run back now-there''s too much we don''t understand."'
                    'Your choices: '
                    '1. "You''re right. We need to explore and figure this out."'
                    '2. "No, it''s too risky. Let''s head back while we can."'
                    ''
                ).
                ^Array with: newState with: response]].

        ((gameState currentLocation = Location ruins or: [gameState currentLocation = Location city]) and: [gameState hasTask: 'tunnel']) ifTrue: [
            gameState isInInventory: 'radio'
                ifTrue: [newState addTask: 'tunnel']
                ifFalse: [newState addTask: 'act_finished'].
            newState currentLocation: Location tunnel.

            gameState isInInventory: 'radio' ifTrue: [
                response addAll: #(
                    'You bolt through the undergrowth, the Nazis'' shouts and revving engines hot on your heels.'
                    'Thorns snag your clothes, tearing at your skin as you burst through the TUNNEL exit and emerge at the crash site, winded and desperate.'
                    'The icy wind bites at your face, a cruel reminder of the surface''s hostility.'
                    ''
                )]
            ifFalse: [
                response addAll: #(
                    'You bolt through the undergrowth, the Nazis'' shouts and revving engines hot on your heels.'
                    'Thorns snag your clothes, tearing at your skin as you burst through the TUNNEL exit and emerge at the crash site, winded and desperate.'
                    'The icy wind bites at your face, a cruel reminder of the surface''s hostility.'
                    ''
                    'You and Clara huddle in the wreckage, the valley''s secrets slipping away as the cold closes in.'
                    'Survival hangs by a thread, your fate uncertain.'
                    'GAME OVER.'
                    ''
                )].
            ^Array with: newState with: response]].

    destination = 'ledge' ifTrue: [
        (gameState currentLocation = Location tree) ifTrue: [
            newState currentLocation: Location ledge.
            response add: 'You climb back down to the ledge.'.
            response add: ''.
            ^Array with: newState with: response]].

    ^nil!

getRandomPistolOutcome: gameState
    | newState response success pistol |
    newState := gameState deepCopy.
    response := OrderedCollection new.

    pistol := gameState findEntity: 'pistol'.
    pistol ifNotNil: [newState removeFromInventory: pistol].
    newState removeTask: 'fight'.
    newState addTask: 'after_fight'.

    "Random success (50% chance)"
    success := (1 to: 2) atRandom = 1.

    success ifTrue: [
        response addAll: #(
            'You hand the PISTOL to Clara.'
            'She aims the old Mauser and fires-a sharp crack echoes through the valley, the shot strikes one of the soldiers, who collapses with a cry.'
            'The Nazis roar in fury, their rifles spitting fire in response.'
            'Bullets chip the rock, showering you with dust and shards.'
            'The leader bellows, his voice thick with venom:'
            ''
            'Nazi Leader: "Ihr wagt es, uns herauszufordern? Euer Blut wird dieses Tal beflecken!"'
            ''
        )]
    ifFalse: [
        response addAll: #(
            'You hand the PISTOL to Clara.'
            'She aims the old Mauser and pulls the trigger, but it jams with a dull click, refusing to fire.'
            'The Nazis burst into mocking laughter, their sneers cutting through the air.'
            'The leader steps forward, his voice dripping with scorn:'
            ''
            'Nazi Leader: "Ihr erbarmlichen Narren! Dafur werdet ihr sterben!"'
            ''
        )].

    ^Array with: newState with: response!

!Act3 class methodsFor: 'game step'!
step: command gameState: gameState
    | newState response result entity |
    newState := gameState deepCopy.
    response := OrderedCollection new.

    (gameState hasTask: 'act_finished') ifTrue: [
        response add: 'You''ve already finished this act. Type "quit" to exit.'.
        response add: ''.
        ^Array with: newState with: response].

    command isQuit ifTrue: [
        Smalltalk quitPrimitive.
        ^Array with: newState with: response].

    command isLook ifTrue: [
        ^self handleLook: gameState].

    command isInventory ifTrue: [
        gameState inventory isEmpty
            ifTrue: [response add: 'You are not carrying anything.']
            ifFalse: [
                response add: 'You are carrying:'.
                gameState inventory do: [:item | response add: item entityName]].
        response add: ''.
        ^Array with: newState with: response].

    command isHint ifTrue: [
        response add: (self getHint: gameState).
        response add: ''.
        ^Array with: newState with: response].

    command isInstructions ifTrue: [
        response addAll: GameUtils instructionsText.
        ^Array with: newState with: response].

    command isGo ifTrue: [
        result := self goSpecial: command parameter asLowercase gameState: gameState.
        result ifNotNil: [^result].
        ^self handleGoNormal: command parameter gameState: gameState].

    command isExamine ifTrue: [
        result := self examineSpecial: command parameter asLowercase gameState: gameState.
        result ifNotNil: [^result].

        entity := gameState findEntity: command parameter.
        entity ifNotNil: [
            newState markExamined: entity entityName.
            response add: entity entityDescription.
            response add: ''.
            ^Array with: newState with: response]
        ifNil: [
            response add: 'I can''t see ', command parameter, ' here or there''s nothing special about it.'.
            response add: ''.
            ^Array with: newState with: response]].

    command isTalk ifTrue: [
        command parameter asLowercase = 'clara' ifTrue: [
            ^self talkClara: gameState].
        command parameter asLowercase = 'creature' ifTrue: [
            ^self talkCreature: gameState].
        response add: 'There''s no one here to talk to by that name.'.
        response add: ''.
        ^Array with: newState with: response].

    command isTake ifTrue: [
        ^self handleTake: command parameter gameState: gameState].

    command isDrop ifTrue: [
        "Special case for pistol when hide task is active"
        (command parameter asLowercase = 'pistol' and: [(gameState isInInventory: 'pistol') and: [gameState hasTask: 'hide']]) ifTrue: [
            ^self handleUse: 'pistol' gameState: gameState].

        "Special case for pistol with random outcome"
        (command parameter asLowercase = 'pistol' and: [(gameState isInInventory: 'pistol') and: [(gameState hasTask: 'fight') and: [gameState currentLocation = Location rock]]]  ) ifTrue: [
            ^self getRandomPistolOutcome: gameState].
        ^self handleDrop: command parameter gameState: gameState].

    command isUse ifTrue: [
        "Special case for pistol with random outcome"
        (command parameter asLowercase = 'pistol' and: [(gameState isInInventory: 'pistol') and: [(gameState hasTask: 'fight') and: [gameState currentLocation = Location rock]]]  ) ifTrue: [
            ^self getRandomPistolOutcome: gameState].

        command parameter asLowercase = 'radio' ifTrue: [
            result := self useRadio: gameState.
            result ifNotNil: [^result]].

        ^self handleUse: command parameter gameState: gameState].

    command isNext ifTrue: [
        (gameState hasTask: 'act_finished') ifTrue: [
            response add: 'Act 3 complete! The end of the game.'.
            response add: ''.
            ^Array with: newState with: response].
        response add: 'You need to finish this act first.'.
        ^Array with: newState with: response].

    response add: 'Unknown command.'.
    response add: ''.
    ^Array with: newState with: response!

handleLook: gameState
    | newState response out |
    newState := gameState deepCopy.
    response := OrderedCollection new.

    out := gameState currentLocation = Location ledge
        ifTrue: [
            ((gameState hasTask: 'tree') and: [(gameState hasExamined: 'ledge') not])
                ifTrue: [
                    newState markExamined: 'ledge'.
                    'Ahead looms a massive TREE, its gnarled trunk wider than a barn, its branches clawing toward the cavern''s glowing ceiling.
Bioluminescent moss clings to its bark, pulsing faintly, while its leaves shimmer with an unearthly light, swaying as if whispering secrets to the wind.']
                ifFalse: [self describeLocation: Location ledge]]
        ifFalse: [self describeLocation: gameState currentLocation].

    response add: out.
    response add: ''.
    ^Array with: newState with: response!

handleGoNormal: destination gameState: gameState
    | newState response targetLocation |
    newState := gameState deepCopy.
    response := OrderedCollection new.

    targetLocation := GameUtils parseLocation: destination.
    targetLocation = Location unknown ifTrue: [
        response add: 'Unknown place: ', destination.
        response add: ''.
        ^Array with: newState with: response].

    (self canMove: gameState currentLocation to: targetLocation) ifTrue: [
        newState currentLocation: targetLocation.
        response add: (self describeLocation: targetLocation).
        response add: ''.
        ^Array with: newState with: response]
    ifFalse: [
        response add: 'You can''t go to ', destination, ' from here.'.
        response add: ''.
        ^Array with: newState with: response]!

handleTake: itemName gameState: gameState
    | newState response entity lowerName |
    newState := gameState deepCopy.
    response := OrderedCollection new.
    lowerName := itemName asLowercase.

    (gameState isInInventory: lowerName) ifTrue: [
        response add: 'You''re already holding it!'.
        response add: ''.
        ^Array with: newState with: response].

    entity := gameState findHere: lowerName.
    entity ifNil: [
        response add: 'I don''t see ', itemName, ' here.'.
        response add: ''.
        ^Array with: newState with: response].

    newState addToInventory: entity.
    response add: 'You take the ', entity entityName, '.'.
    response add: ''.
    ^Array with: newState with: response!

handleDrop: itemName gameState: gameState
    | newState response entity lowerName |
    newState := gameState deepCopy.
    response := OrderedCollection new.
    lowerName := itemName asLowercase.

    (gameState isInInventory: lowerName) ifFalse: [
        response add: 'You aren''t carrying that!'.
        response add: ''.
        ^Array with: newState with: response].

    entity := gameState findEntity: lowerName.
    newState removeFromInventory: entity.
    response add: 'OK.'.
    response add: ''.
    ^Array with: newState with: response!

handleUse: itemName gameState: gameState
    | newState response lowerName |
    newState := gameState deepCopy.
    response := OrderedCollection new.
    lowerName := itemName asLowercase.

    lowerName = 'pistol' ifTrue: [
        (gameState isInInventory: 'pistol') ifTrue: [
            response add: 'Who do you want me to shoot, you psycho?'.
            response add: ''.
            ^Array with: newState with: response]].

    lowerName = 'geiger' ifTrue: [
        (gameState isInInventory: 'geiger') ifTrue: [
            response add: 'Radiation levels - normal.'.
            response add: ''.
            ^Array with: newState with: response]].

    (gameState isInInventory: lowerName) ifTrue: [
        response add: 'I can''t use that right now.'.
        response add: ''.
        ^Array with: newState with: response].

    response add: 'I don''t have it or I can''t use it.'.
    response add: ''.
    ^Array with: newState with: response!

!Act3 class methodsFor: 'main game loop'!
gameLoop: gameState
    | state command result newState output line choice |
    state := gameState.
    [true] whileTrue: [
        line := GameUtils readCommand.

        "Handle choice inputs"
        (state hasTask: 'awaiting_ledge_choice') ifTrue: [
            result := self processChoice: line gameState: state choiceType: 'ledge'.
            newState := result first.
            output := result second.
            GameUtils printLines: output.
            state := newState.
            ] ifFalse: [
        (state hasTask: 'awaiting_creature_choice') ifTrue: [
            result := self processChoice: line gameState: state choiceType: 'creature'.
            newState := result first.
            output := result second.
            GameUtils printLines: output.
            state := newState.
            ] ifFalse: [
        (state hasTask: 'awaiting_ambush_choice') ifTrue: [
            result := self processChoice: line gameState: state choiceType: 'ambush'.
            newState := result first.
            output := result second.
            GameUtils printLines: output.
            state := newState.
            ] ifFalse: [
        (state hasTask: 'awaiting_tunnel_choice') ifTrue: [
            "Handle tunnel choice processing here"
            choice := line.
            newState := state deepCopy.
            newState removeTask: 'awaiting_tunnel_choice'.
            choice = '1' ifTrue: [
                output := #('You: "You''re right. We need to explore and figure this out."' '*You stay in the valley, your resolve hardening.*' '')]
            ifFalse: [choice = '2' ifTrue: [
                output := #('You: "No, it''s too risky. Let''s head back while we can."' 'Clara: "And what? Freeze out there with no plan? We''re in too deep to turn tail now."' '')]
            ifFalse: [
                output := #('Invalid choice - enter 1 or 2.' '').
                newState := state]].
            GameUtils printLines: output.
            state := newState.
            ] ifFalse: [

        "Handle regular commands"
        result := self step: (GameUtils parseCommand: line) gameState: state.
        newState := result first.
        output := result second.
        GameUtils printLines: output.
        state := newState.

        ((GameUtils parseCommand: line) isQuit) ifTrue: [^PlayerState extractFrom: newState].
        (((GameUtils parseCommand: line) isNext) and: [newState hasTask: 'act_finished']) ifTrue: [
            ^PlayerState extractFrom: newState].
        ]]]].
    ]!

start: playerState
    | gameState |
    gameState := self createInitialState.
    gameState inventory: playerState inventory.
    GameUtils printLines: self prologText.
    GameUtils printLines: (Array with: (self describeLocation: Location ledge) with: '').
    ^self gameLoop: gameState! !
