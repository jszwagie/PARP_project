BaseGame subclass: Act3Game [

    setupLocations [
        | oc |
        locations := Dictionary new.

        " Ledge "
        oc := OrderedCollection new.
        oc add: (Entity personNamed: 'clara' description: 'Clara stands beside you, looking out over the hidden valley.').
        locations at: #'ledge' put: oc.

        " Tree "
        locations at: #'tree' put: OrderedCollection new.

        " Ruins "
        oc := OrderedCollection new.
        oc add: (Entity personNamed: 'creature' description: 'A tall, slender figure with luminous eyes studying you with quiet intrigue.').
        locations at: #'ruins' put: oc.

        " City "
        locations at: #'city' put: OrderedCollection new.

        " Rock "
        locations at: #'rock' put: OrderedCollection new.

        " Tunnel "
        locations at: #'tunnel' put: OrderedCollection new.

        currentLocation := #'ledge'.
        self addTask: 'ledge_talk'
    ]

    canMoveFrom: fromLoc to: toLoc [
        (fromLoc = #'ledge' and: [toLoc = #'tree']) ifTrue: [ ^ true ].
        (fromLoc = #'tree' and: [toLoc = #'ledge']) ifTrue: [ ^ true ].
        (fromLoc = #'tree' and: [toLoc = #'ruins']) ifTrue: [ ^ true ].
        (fromLoc = #'ruins' and: [toLoc = #'tree']) ifTrue: [ ^ true ].
        (fromLoc = #'tunnel' and: [toLoc = #'tree']) ifTrue: [ ^ true ].
        (fromLoc = #'tree' and: [toLoc = #'tunnel']) ifTrue: [ ^ true ].
        (fromLoc = #'tree' and: [toLoc = #'city']) ifTrue: [ ^ true ].
        (fromLoc = #'city' and: [toLoc = #'tree']) ifTrue: [ ^ true ].
        ^ false
    ]

    parseLocation: aString [
        | s |
        s := aString asLowercase.
        s = 'ledge'   ifTrue: [ ^ #'ledge' ].
        s = 'tree'    ifTrue: [ ^ #'tree' ].
        s = 'ruins'   ifTrue: [ ^ #'ruins' ].
        s = 'woods'   ifTrue: [ ^ #'ruins' ].
        s = 'tunnel'  ifTrue: [ ^ #'tunnel' ].
        s = 'city'    ifTrue: [ ^ #'city' ].
        s = 'rock'    ifTrue: [ ^ #'rock' ].
        ^ #'unknown'
    ]

    descriptionOf: location [
        location = #'ledge' ifTrue: [ ^ self ledgeDescription ].
        location = #'tree' ifTrue: [ ^ self treeDescription ].
        location = #'ruins' ifTrue: [ ^ self ruinsDescription ].
        location = #'tunnel' ifTrue: [ ^ self tunnelDescription ].
        location = #'city' ifTrue: [ ^ self cityDescription ].
        location = #'rock' ifTrue: [ ^ self rockDescription ].
        ^ 'You see nothing special.'
    ]

    doLook [
        | out |
        out := ''.
        (currentLocation = #'ledge' and: [self hasTask: 'tree'] and: [(self hasExamined: 'ledge') not]) ifTrue: [
            self markExamined: 'ledge'.
            out := 'Ahead looms a massive TREE, its gnarled trunk wider than a barn, its branches clawing toward the cavern''s glowing ceiling.' , Character lf asString,
                   'Bioluminescent moss clings to its bark, pulsing faintly, while its leaves shimmer with an unearthly light, swaying as if whispering secrets to the wind.'
        ] ifFalse: [
            out := self descriptionOf: currentLocation
        ].
        self printLine: out; printLine: ''
    ]

    ledgeDescription [
        ^ 'You both stand on a rocky ledge overlooking a hidden realm-an expansive, verdant valley cradled beneath Antarctica''s icy crust.' , Character lf asString,
          'Bioluminescent plants emit a soft, ethereal glow, casting light across towering ferns and crystalline rivers that shimmer like liquid glass.' , Character lf asString,
          'The air hangs warm and humid, thick with the scent of exotic blooms, a jarring contrast to the frozen desolation above.' , Character lf asString,
          'Flying saucers, eerily similar to the wreck you stumbled upon, glide silently through the skies, their presence a quiet warning of something watchful and alive down here.'
    ]

    treeDescription [
        ^ 'From the tree''s upper branches, the valley sprawls before you in breathtaking detail.' , Character lf asString,
          'To the east, ancient-looking RUINS emerge from the foliage-crumbling pyramids and temples etched with cryptic symbols, remnants of a lost civilization.' , Character lf asString,
          'To the west, the stark silhouette of a CITY cuts through the greenery, its dark gray buildings festooned with swastika flags fluttering ominously in the breeze, their bold red and black stark against the muted stone.' , Character lf asString,
          'Behind you, the TUNNEL exit gapes like a dark maw, leading back to the frozen surface-a lifeline or a trap, depending on your next move.'
    ]

    ruinsDescription [
        ^ 'The ruins before you are a marvel of ancient architecture, reminiscent of Egypt''s pyramids or the jungle temples of South America, yet distinctly alien.' , Character lf asString,
          'Crumbling stone facades are adorned with intricate carvings of starships and celestial beings, hinting at a civilization far beyond human comprehension.' , Character lf asString,
          'The air here feels thick with history and unspoken secrets.'
    ]

    tunnelDescription [
        ^ 'The crash site lies in ruins, the plane''s twisted metal half-buried in snow.' , Character lf asString,
          'The wind howls mercilessly, and the sky above is a bleak, unforgiving gray.' , Character lf asString,
          'Your breath fogs in the frigid air, a stark contrast to the warmth of the hidden valley below.'
    ]

    cityDescription [
        ^ 'The city cuts a stark silhouette against the valley''s greenery, its dark gray buildings rising like monolithic sentinels.' , Character lf asString,
          'Swastika flags flutter ominously from every structure, their bold red and black stark against the muted stone.' , Character lf asString,
          'The atmosphere is heavy with foreboding, as if the very walls are watching your every move.'
    ]

    rockDescription [
        ^ 'The massive boulder provides a makeshift shelter, its surface slick with glowing moss.' , Character lf asString,
          'You press against the cold stone, your breath ragged as the Nazi patrol draws closer.' , Character lf asString,
          'From this vantage point, you can see several Nazis, their faces twisted in determination as they search the area.'
    ]

    getHint [
        (self hasTask: 'act_finished') ifTrue: [
            ^ 'You''ve already finished this act. Type "quit" to exit the game.'
        ].
        
        (self hasTask: 'woods') ifTrue: [
            ^ 'We should GO to the WOODS.'
        ].
        
        (currentLocation = #'tunnel' and: [self hasItem: 'radio'] and: [self hasTask: 'tunnel']) ifTrue: [
            ^ 'I should USE the RADIO, as I said.'
        ].
        
        (currentLocation = #'rock' and: [self hasTask: 'tunnel']) ifTrue: [
            ^ 'We should go to TUNNEL.'
        ].
        
        (currentLocation = #'rock' and: [self hasTask: 'after_fight']) ifTrue: [
            ^ 'I should talk to Clara.'
        ].
        
        (currentLocation = #'rock' and: [self hasItem: 'pistol'] and: [self hasTask: 'fight']) ifTrue: [
            ^ 'I should hand the PISTOL to Clara.'
        ].
        
        (self hasTask: 'ambush_beginning') ifTrue: [
            ^ 'Maybe Clara knows what to do in this situation.'
        ].
        
        (self hasTask: 'after_radio') ifTrue: [
            ^ 'I should talk to Clara.'
        ].
        
        (self hasTask: 'radio' and: [self hasItem: 'radio'] and: [self hasExamined: 'note']) ifTrue: [
            ^ 'The note says ''Four''s the square, Seven''s luck, Two''s pair.'' That could point to the settings for A, B, and C. The plaque might help confirm it.'
        ].
        
        (self hasTask: 'radio' and: [self hasItem: 'radio']) ifTrue: [
            ^ 'I need to tune the dials to the right numbers to reach the Marines. The NOTE or the RADIO might hold the key.'
        ].
        
        (self hasTask: 'hide' and: [self hasItem: 'pistol']) ifTrue: [
            ^ 'I should GO behind that ROCK.'
        ].
        
        (currentLocation = #'ledge' and: [self hasTask: 'ledge_talk']) ifTrue: [
            ^ 'I should talk to Clara.'
        ].
        
        (currentLocation = #'ledge' and: [self hasTask: 'tree'] and: [self hasExamined: 'ledge']) ifTrue: [
            ^ 'I think I could GO up on that TREE.'
        ].
        
        (currentLocation = #'ledge' and: [self hasTask: 'tree']) ifTrue: [
            ^ 'I should find a high place for recon.'
        ].
        
        (currentLocation = #'tree') ifTrue: [
            ^ 'I should LOOK around and decide where to GO.'
        ].
        
        (currentLocation = #'ruins' and: [(self entitiesAt: #'ruins') anySatisfy: [:e | e name = 'creature']]) ifTrue: [
            ^ 'Is the TALK the answer?'
        ].
        
        (currentLocation = #'ruins' and: [self hasTask: 'hide']) ifTrue: [
            ^ 'I should hide behind that ROCK'
        ].
        
        (currentLocation = #'city' and: [self hasTask: 'hide']) ifTrue: [
            ^ 'I should hide behind that ROCK'
        ].
        
        ^ 'I should try to LOOK around to get my bearings.'
    ]

    examineSpecial: key [
        (key = 'creature' and: [currentLocation = #'ruins'] and: [(self entitiesAt: #'ruins') anySatisfy: [:e | e name = 'creature']]) ifTrue: [
            self markExamined: 'creature'.
            self printLines: {
                'The creature stands tall and slender, its luminous eyes studying you with an intelligence that feels ancient.'.
                'Its skin seems to shimmer faintly, and as you look closer, you realize it''s communicating directly into your mind-a melodic hum that bypasses your ears.'.
                'It exudes an aura of wisdom and otherworldliness, as if it holds secrets older than time itself.'.
                ''
            }.
            ^ true
        ].
        
        (key = 'radio' and: [self hasTask: 'radio'] and: [self isInInventory: 'radio']) ifTrue: [
            self markExamined: 'radio'.
            self printLines: {
                'The RADIO is a rugged military device, scratched and dented but still working.'.
                'Each dial can be set to a number between 1 and 9.'.
                'The dials click stiffly as you turn them. A small plaque beneath them reads: "Standard Marine Corps Protocol: A=Even, B=Prime, C=Square."'.
                ''
            }.
            ^ true
        ].
        
        (key = 'note' and: [self hasTask: 'radio'] and: [self isInInventory: 'radio']) ifTrue: [
            self markExamined: 'note'.
            self printLines: {
                'The note is weathered, its ink blurred but readable: "Marine Corps Frequency: Alpha-Bravo-Charlie. Remember the code: Four''s the square, Seven''s luck, Two''s pair."'.
                ''
            }.
            ^ true
        ].
        
        ^ nil
    ]

    handleTalk: whoName [
        | name |
        name := whoName asLowercase.
        
        name = 'clara' ifTrue: [
            ^ self talkClara
        ].
        
        name = 'creature' ifTrue: [
            ^ self talkCreature
        ].
        
        self printLines: { 'There''s no one here to talk to by that name.' . '' }
    ]

    talkClara [
        (self hasTask: 'after_radio') ifTrue: [
            ^ self claraAfterRadio
        ].
        
        (self hasTask: 'hide' and: [(currentLocation = #'ruins' or: [currentLocation = #'city'])]) ifTrue: [
            self printLines: { 'Clara: "Here, give me the pistol and get behind that ROCK-now!"' . '' }.
            ^ nil
        ].
        
        (self hasTask: 'tunnel' and: [(currentLocation = #'ruins' or: [currentLocation = #'city'])]) ifTrue: [
            self printLines: { 'Clara: "It''s a long shot, but let''s GO to the TUNNEL now!"' . '' }.
            ^ nil
        ].
        
        (self hasTask: 'woods' and: [(currentLocation = #'ruins' or: [currentLocation = #'city'])]) ifTrue: [
            self printLines: { 'Clara: "It''s a long shot, but let''s GO to the WOODS now!"' . '' }.
            ^ nil
        ].
        
        (self hasTask: 'after_fight') ifTrue: [
            ^ self claraAfterFight
        ].
        
        (self hasTask: 'ambush_beginning') ifTrue: [
            ^ self claraAmbushDialog
        ].
        
        (currentLocation = #'ledge' and: [(self hasTalked: 'clara' topic: 'ledge') not]) ifTrue: [
            ^ self claraLedgeDialog
        ].
        
        (currentLocation = #'ruins' and: [(self entitiesAt: #'ruins') anySatisfy: [:e | e name = 'creature']]) ifTrue: [
            self printLines: { 'Clara: "Do you think we should TALK to it?"' . '' }.
            ^ nil
        ].
        
        (currentLocation = #'tree') ifTrue: [
            self printLines: { 'Clara: Do you see anything interesting?' . '' }.
            ^ nil
        ].
        
        self printLines: { 'There''s no one here to talk to.' . '' }
    ]

    claraAfterRadio [
        self addTask: 'act_finished'.
        self printLines: {
            'You turn to Clara, her face illuminated by the dim cabin lights.'.
            ''.
            'You: "So, what-this is how it ends?"'.
            'Clara: "Wake up!"'.
            'You: "What?!"'.
            'Clara: "WAKE UP!"'.
            ''.
            'Suddenly, a sharper voice breaks through the haze.'.
            'Your wife: "Damn it, wake up! You''ll be late for your lectures!"'.
            'You: "What? What lectures?"'.
            'Your wife: "You were up late watching TV again. You''ve got to stop with those'.
            'ridiculous pseudo-historical documentaries on FOCUS TV or TV4-they''re frying your brain."'.
            ''.
            'The Antarctic adventure dissolves like mist. You blink, disoriented, as the soft'.
            'glow of your bedside lamp replaces the plane''s harsh lights. The hum of Warsaw''s'.
            'morning traffic seeps through the window, a mundane rhythm far removed from the'.
            'valley''s eerie pulse. It was all a dream-a vivid fantasy spun from late-night'.
            'television and a restless mind. You''re not an adventurer escaping a hidden'.
            'world; you''re an ordinary professor at the Warsaw University of Technology, with'.
            'lectures to deliver and papers to grade. Reality sinks in, familiar and unrelenting.'.
            ''.
            'You sit up, rubbing your eyes as the dream''s vivid details-Clara''s determined'.
            'gaze, the snow-swept valley, the roar of the plane-slip away like sand through'.
            'your fingers. Your wife moves about the room, muttering about your late-night'.
            'habits, oblivious to the epic journey you''ve just imagined.'.
            ''.
            'Your wife: "Honestly, those conspiracy channels will be the death of you. Go to bed on time for once."'.
            ''.
            'You muster a faint smile, the last echoes of the dream fading into nothingness.'.
            'The adventure is over, and the real world beckons.'.
            ''.
            'THE END'.
            ''
        }.
        ^ nil
    ]

    claraAfterFight [
        self addTask: 'act_finished'.
        self removeTask: 'after_fight'.
        self printLines: {
            'You: "What did he say?"'.
            'Clara: *breathing heavily* "Nothing good. I don''t know if we can get out of this alive."'.
            'Clara: *shouting in fright* "Wir kapitulieren! Halt!"'.
            ''.
            'The soldiers cease fire, their eyes still burning with rage.'.
            'They swarm closer, boots pounding the earth like war drums, and you''re wrestled to the ground, wrists bound tight with rough cord.'.
            'Their treatment is brutal - fists and threats of execution, though they spare you for now, muttering darkly about your potential value.'.
            'They march you toward the CITY, their motorcycles roaring triumphantly.'.
            ''.
            'The CITY looms ahead, its dark spires piercing the bioluminescent sky like'.
            'jagged teeth. Clara stumbles beside you, her face pale but defiant, though her eyes betray a flicker of fear.'.
            ''.
            'You steal a glance at the leader, his scar twisting as he smirks, satisfied with'.
            'his prize. What awaits in the CITY? Interrogation? Imprisonment? Or something'.
            'far worse, tied to the secrets buried in this impossible valley? The questions'.
            'gnaw at you, but answers remain elusive, shrouded in the same mystery that'.
            'cloaks this hidden world.'.
            ''.
            'As the CITY gates creak open, swallowing you into its shadowed maw, one thought'.
            'lingers: this is not the end, but a dark new beginning. Your fate hangs in the'.
            'balance, and the next chapter of your journey waits just beyond the horizon.'.
            ''.
            'TO BE CONTINUED...'.
            ''
        }.
        ^ nil
    ]

    claraAmbushDialog [
        self addTask: 'awaiting_ambush_choice'.
        self removeTask: 'ambush_beginning'.
        self printLines: {
            'You: "What''s our move? They''re closing in fast."'.
            'Clara: "We''re outgunned and outmanned. Fight, run, or surrender-you decide, but make it quick!"'.
            'Your choices:'.
            (self isInInventory: 'pistol') ifTrue: ['1. "Let''s fight! I''ll hand you the PISTOL!"'] ifFalse: [''].
            '2. "Run for the TUNNEL.' , ((self isInInventory: 'radio') ifTrue: [' We can try the RADIO one more time!"'] ifFalse: ['"']).
            '3. "We surrender. Maybe we can talk our way out."'.
            '4. "Into the WOODS-lose them in the trees!"'.
            ''
        }.
        ^ self waitForChoice: #ambush
    ]

    claraLedgeDialog [
        self addTask: 'awaiting_ledge_choice'.
        self markTalked: 'clara' topic: 'ledge'.
        self printLines: {
            'Clara: "This place... it''s like stepping into a dream. Or maybe a nightmare-I can''t decide."'.
            'You: "It''s incredible-Byrd wasn''t exaggerating in that diary."'.
            'Clara: "Sure, but that diary was written before the war. No mention of Nazis anywhere in it. Do you think they beat us to this discovery?"'.
            'Your choices:'.
            '1. "Maybe they found it during their Antarctic expeditions in the ''30s."'.
            '2. "Or they stumbled across it after the war, looking for a place to hide."'.
            ''
        }.
        ^ self waitForChoice: #ledge
    ]

    talkCreature [
        (currentLocation = #'ruins' and: [(self entitiesAt: #'ruins') anySatisfy: [:e | e name = 'creature']]) ifTrue: [
            self addTask: 'awaiting_creature_choice'.
            self printLines: {
                '*The creature''s voice resonates in your mind, a melodic hum that bypasses your ears entirely.*'.
                ''.
                'Creature: "Wanderers, greetings. Sentinel of this realm, I am, keeper of wisdom older than your civilization, hmm.'.
                'Answers you seek, yes? Give them to you, I shall.'.
                'Tied to what you call ''Atlantis,'' our kin are, though lost to your tongue, our true name is.'.
                'Arrived, the ones you call ''Germans'' did, speaking of a great calamity they fled. Stewards of peace we are, granted them refuge, we did. Yet, tidings of their shadow you bear, hmm?"'.
                ''.
                'Your choices:'.
                '1. "Those Germans-the Nazis-are monsters. They''ve waged war and killed millions."'.
                '2. "They''re exploiting you. They''ll strip this valley bare and leave nothing behind."'.
                ''
            }.
            ^ self waitForChoice: #creature
        ].
        
        self printLines: { 'There''s no one here to talk to.' . '' }
    ]

    handleSpecialCommand: aCmdArray [
        | cmd arg |
        cmd := aCmdArray first.
        arg := aCmdArray size > 1 ifTrue: [ aCmdArray second ] ifFalse: [ nil ].
        
        " Handle choice inputs "
        (self hasTask: 'awaiting_ledge_choice') ifTrue: [
            ^ self handleChoiceInput: aCmdArray context: #ledge range: 2
        ].
        
        (self hasTask: 'awaiting_creature_choice') ifTrue: [
            ^ self handleChoiceInput: aCmdArray context: #creature range: 2
        ].
        
        (self hasTask: 'awaiting_ambush_choice') ifTrue: [
            ^ self handleChoiceInput: aCmdArray context: #ambush range: 4
        ].
        
        (self hasTask: 'awaiting_tunnel_choice') ifTrue: [
            ^ self handleChoiceInput: aCmdArray context: #tunnel range: 2
        ].
        
        " Handle special GO commands "
        (cmd = #go) ifTrue: [
            arg ifNotNil: [
                ^ self handleSpecialGo: arg
            ]
        ].
        
        ^ nil
    ]

    handleChoiceInput: aCmdArray context: context range: maxChoice [
        | input choice |
        input := aCmdArray first asString.
        choice := input asInteger.
        
        (choice between: 1 and: maxChoice) ifTrue: [
            ^ self processChoice: choice context: context
        ].
        self printLine: 'Invalid choice - enter 1-' , maxChoice asString , '.'.
        ^ nil
    ]

    processChoice: choice context: context [
        context = #ledge ifTrue: [ ^ self processLedgeChoice: choice ].
        context = #creature ifTrue: [ ^ self processCreatureChoice: choice ].
        context = #ambush ifTrue: [ ^ self processAmbushChoice: choice ].
        context = #tunnel ifTrue: [ ^ self processTunnelChoice: choice ].
        ^ self printLine: 'Invalid choice context.'
    ]

    processLedgeChoice: choice [
        choice = 1 ifTrue: [
            self removeTask: 'ledge_talk'.
            self removeTask: 'awaiting_ledge_choice'.
            self addTask: 'tree'.
            self printLines: {
                'You: "Maybe they found it during their Antarctic expeditions in the ''30s."'.
                'Clara: "That could explain those flying saucers. They''ve had decades to dig in, hidden from the rest of the world."'.
                ''.
                'You stand together, awestruck by the valley''s haunting beauty.'.
                'The bioluminescent flora bathes the landscape in a shimmering, otherworldly hue, while the faint hum of the valley''s life-chirps, rustles, and distant cries-wraps around you like a living tapestry.'.
                'It''s a paradise untouched by time, yet the shadow of danger looms just out of sight.'.
                ''.
                'Clara: *gasps* "Okay, enough gawking. If we want to survive this, we need a better lay of the land. Let''s find a high spot for reconnaissance."'.
                ''
            }
        ].
        
        choice = 2 ifTrue: [
            self removeTask: 'ledge_talk'.
            self removeTask: 'awaiting_ledge_choice'.
            self addTask: 'tree'.
            self printLines: {
                'You: "Or they stumbled across it after the war, looking for a place to hide."'.
                'Clara: "Hide? More like regroup. This could be their secret fortress, waiting for the right moment to strike back."'.
                ''.
                'You stand together, awestruck by the valley''s haunting beauty.'.
                'The bioluminescent flora bathes the landscape in a shimmering, otherworldly hue, while the faint hum of the valley''s life-chirps, rustles, and distant cries-wraps around you like a living tapestry.'.
                'It''s a paradise untouched by time, yet the shadow of danger looms just out of sight.'.
                ''.
                'Clara: *gasps* "Okay, enough gawking. If we want to survive this, we need a better lay of the land. Let''s find a high spot for reconnaissance."'.
                ''
            }
        ]
    ]

    processCreatureChoice: choice [
        (choice = 1 or: [choice = 2]) ifTrue: [
            | newEntities responseLine |
            newEntities := (self entitiesAt: #'ruins') reject: [:e | e name = 'creature'].
            locations at: #'ruins' put: newEntities.
            self removeTask: 'awaiting_creature_choice'.
            self addTask: 'ambush_beginning'.
            
            responseLine := choice = 1 
                ifTrue: ['You: "Those Germans-the Nazis-are monsters. They''ve waged war and killed millions."' , Character lf asString , 'Creature: "Malice such, perceived it not, we did. Blinded us, our hospitality has, to their stain."']
                ifFalse: ['You: "They''re exploiting you. They''ll strip this valley bare and leave nothing behind."' , Character lf asString , 'Creature: "Cloaked in deception, they are, then. Harmony we cherish, yet stirred by this threat, we are. Counsel, what offer you?"'].
            
            self printLines: {
                responseLine.
                '*Before you can respond, the air splits with the roar of engines and sharp, guttural shouts. The ground trembles faintly-a prelude to chaos.*'.
                'Creature: "True, if what you say is, run immediately, you must. Farewell, my friends."'.
                '*The creature dissolves into the air, leaving you confused and on edge.*'.
                ''.
                'The Nazis lock eyes on you, their motorcycles skidding to a halt in a crescent of dust and menace.'.
                'Their leader, a wiry man with a scar slashing across his cheek, leaps off his bike, his black uniform pristine despite the grime of the valley.'.
                'He levels a Luger at you, his voice a guttural snarl that cuts through the humid air.'.
                ''.
                'Nazi Leader: "Halt! Amerikanische Spione! Werft die Waffen nieder!"'.
                'Clara (whispering): "They think we are spies. They''ve got us wrong, but I doubt they''ll listen to reason."'.
                ''.
                'The air thickens with tension as the Nazis fan out, their boots crunching on the gravel, rifles glinting in the bioluminescent glow.'.
                'Above, a flying saucer hums into view, its searchlight slicing through the foliage like a predator''s gaze.'.
                'Time slows-your heart pounds, and the valley''s beauty fades behind the cold reality of danger.'.
                ''
            }
        ]
    ]

    processAmbushChoice: choice [
        choice = 1 ifTrue: [
            (self isInInventory: 'pistol') ifTrue: [
                self removeTask: 'awaiting_ambush_choice'.
                self addTask: 'hide'.
                self printLines: {
                    'You: "Let''s fight! I''ll hand you the PISTOL!"'.
                    'Clara: "Here, give me the pistol and get behind that ROCK-now!"'.
                    ''
                }
            ]
        ].
        
        choice = 2 ifTrue: [
            self removeTask: 'awaiting_ambush_choice'.
            self addTask: 'tunnel'.
            self printLines: {
                'You: "To the TUNNEL - move. ' , ((self isInInventory: 'radio') ifTrue: ['We can try the RADIO one more time!'] ifFalse: ['']).
                (self isInInventory: 'radio') 
                    ifTrue: ['Clara: "It''s a long shot, but let''s go!"']
                    ifFalse: ['Clara: "Without the RADIO, we''ll just freeze out there. Terrible plan, but I''m with you."'].
                ''
            }
        ].
        
        choice = 3 ifTrue: [
            self addTask: 'act_finished'.
            self removeTask: 'awaiting_ambush_choice'.
            self printLines: {
                'You: "We surrender. Maybe we can talk our way out."'.
                'You raise your hands slowly. Clara mirrors your movement and calls out to the soldiers:'.
                'Clara: "Wir kapitulieren! Kein Problem."'.
                ''.
                'The Nazis lower their rifles slightly, though their glares remain sharp as knives. The leader smirks, holstering his Luger with a flourish.'.
                ''.
                'Nazi Leader: "Kluger Schachzug, Amerikaner. Unser Kommandant mochte Sie unbedingt sehen."'.
                'They bind your hands with coarse rope, the knots biting into your wrists, and march you toward the CITY, their motorcycles roaring triumphantly.'.
                ''.
                'TO BE CONTINUED...'.
                ''
            }
        ].
        
        choice = 4 ifTrue: [
            self removeTask: 'awaiting_ambush_choice'.
            self addTask: 'woods'.
            self printLines: {
                'You: "Come on, to the WOODS-GO!"'.
                ''
            }
        ]
    ]

    processTunnelChoice: choice [
        choice = 1 ifTrue: [
            self removeTask: 'awaiting_tunnel_choice'.
            self printLines: {
                'You: "You''re right. We need to explore and figure this out."'.
                '*You stay in the valley, your resolve hardening.*'.
                ''
            }
        ].
        
        choice = 2 ifTrue: [
            | baseResponse radioResponse |
            baseResponse := {
                'You: "No, it''s too risky. Let''s head back while we can."'.
                'Clara: "And what? Freeze out there with no plan? We''re in too deep to turn tail now."'
            }.
            
            radioResponse := (self isInInventory: 'radio')
                ifTrue: [{
                    'You: "But we could try contacting the base again with the radio."'.
                    'Clara: "Don''t be naive, doc. That radio''s a piece of junk-half-static on a good day-and even if we got through, what then?'.
                    'Help''s days away at best, assuming they don''t think we''re delusional. We''d be stuck out there, freezing to death, praying for a miracle.'.
                    'No, we push forward, find answers, and make our own way out of this mess."'
                }]
                ifFalse: [{
                    'Clara: "And what? Freeze out there with no plan? We''re in too deep to turn tail now.'.
                    'Without a radio or supplies, we wouldn''t last a day on the surface. Our only chance is to keep moving, find shelter or someone who knows what''s going on-anything''s better than retreating empty-handed."'
                }].
            
            self removeTask: 'awaiting_tunnel_choice'.
            self printLines: (baseResponse , radioResponse , { '' })
        ]
    ]

    handleSpecialGo: destination [
        | dest |
        dest := destination asLowercase.
        
        " Handle special GO commands "
        dest = 'woods' ifTrue: [
            ((currentLocation = #'ruins' or: [currentLocation = #'city']) and: [self hasTask: 'woods']) ifTrue: [
                ^ self goWoods
            ]
        ].
        
        dest = 'rock' ifTrue: [
            ((currentLocation = #'ruins' or: [currentLocation = #'city']) and: [self isInInventory: 'pistol'] and: [self hasTask: 'hide']) ifTrue: [
                ^ self goRock
            ]
        ].
        
        dest = 'tree' ifTrue: [
            (currentLocation = #'ledge' and: [self hasTask: 'tree']) ifTrue: [
                ^ self goTree
            ]
        ].
        
        dest = 'ruins' ifTrue: [
            (currentLocation = #'tree') ifTrue: [
                ^ self goRuins
            ]
        ].
        
        dest = 'city' ifTrue: [
            (currentLocation = #'tree') ifTrue: [
                ^ self goCity
            ]
        ].
        
        dest = 'tunnel' ifTrue: [
            ^ self goTunnelSpecial
        ].
        
        ^ nil " Let normal GO handling take over "
    ]

    goWoods [
        self addTask: 'act_finished'.
        self removeTask: 'woods'.
        self printLines: {
            'You sprint into the dense forest, branches snapping underfoot as you weave through the shadows.'.
            'The Nazis'' shouts fade briefly-you dare to hope-until the sky hums with menace.'.
            'A flying saucer descends, its beam of light slashing through the canopy like a blade, pinning you in its merciless glare.'.
            ''.
            'Nazi Pilot (over loudspeaker): "Kein Entkommen, ihr Narren! Das Reich sieht alles!"'.
            'Riflemen emerge from the trees, their grips iron as they drag you back to the group. They bind your hands with coarse rope and march you toward the CITY, their motorcycles roaring triumphantly.'.
            ''.
            'TO BE CONTINUED...'.
            ''
        }.
        ^ nil
    ]

    goRock [
        self removeTask: 'hide'.
        self addTask: 'fight'.
        currentLocation := #'rock'.
        self printLines: {
            'You dive behind a jagged boulder, its surface slick with glowing moss, your breath ragged as you press against the cold stone.'.
            ''
        }.
        ^ nil
    ]

    goTree [
        currentLocation := #'tree'.
        self removeTask: 'tree'.
        self printLines: {
            'You approach the towering TREE, its presence both majestic and unsettling.'.
            'Thick vines and sturdy branches form a natural ladder, inviting you to climb into its heights.'.
            ''
        }.
        ^ nil
    ]

    goRuins [
        currentLocation := #'ruins'.
        self printLines: {
            'You weave through the dense undergrowth toward the RUINS, their stone facades echoing the grandeur of Egypt''s pyramids or the jungle temples of South America, yet twisted with an alien flair.'.
            'Intricate carvings of starships and celestial beings adorn the walls, hinting at a history far beyond human understanding.'.
            'As you step deeper, a tall, slender figure emerges, its luminous eyes studying you with quiet intrigue. The CREATURE gestures gracefully, inviting conversation.'.
            ''
        }.
        ^ nil
    ]

    goCity [
        currentLocation := #'city'.
        self addTask: 'ambush_beginning'.
        self printLines: {
            'You set off toward the CITY, its ominous skyline growing sharper with each step. Before you reach its perimeter, the growl of engines cuts through the stillness.'.
            'A division of Nazis on motorcycles bursts into view, their dust trails rising like storm clouds. Clara mutters under her breath, "Looks like we''ve got company-and they don''t seem friendly."'.
            ''.
            'The Nazis lock eyes on you, their motorcycles skidding to a halt in a crescent of dust and menace.'.
            'Their leader, a wiry man with a scar slashing across his cheek, leaps off his bike, his black uniform pristine despite the grime of the valley.'.
            'He levels a Luger at you, his voice a guttural snarl that cuts through the humid air.'.
            ''.
            'Nazi Leader: "Halt! Amerikanische Spione! Werft die Waffen nieder!"'.
            'Clara (whispering): "They think we are spies. They''ve got us wrong, but I doubt they''ll listen to reason."'.
            ''.
            'The air thickens with tension as the Nazis fan out, their boots crunching on the gravel, rifles glinting in the bioluminescent glow.'.
            'Above, a flying saucer hums into view, its searchlight slicing through the foliage like a predator''s gaze.'.
            'Time slows-your heart pounds, and the valley''s beauty fades behind the cold reality of danger.'.
            ''
        }.
        ^ nil
    ]

    goTunnelSpecial [
        (currentLocation = #'tree') ifTrue: [
            (self hasTalked: 'clara' topic: 'tunnel') ifTrue: [
                self printLines: { 'We can''t GO to TUNNEL. We went too far.' . '' }.
                ^ nil
            ].
            
            self addTask: 'awaiting_tunnel_choice'.
            self markTalked: 'clara' topic: 'tunnel'.
            self printLines: {
                'Clara grabs your sleeve, her grip tight.'.
                'Clara: "Hold on! We can''t just run back now-there''s too much we don''t understand."'.
                'Your choices:'.
                '1. "You''re right. We need to explore and figure this out."'.
                '2. "No, it''s too risky. Let''s head back while we can."'.
                ''
            }.
            ^ nil
        ].
        
        ((currentLocation = #'ruins' or: [currentLocation = #'city']) and: [self hasTask: 'tunnel']) ifTrue: [
            | gameOverMsg |
            currentLocation := #'tunnel'.
            
            (self isInInventory: 'radio') ifTrue: [
                self addTask: 'tunnel'.
                gameOverMsg := {
                    'You bolt through the undergrowth, the Nazis'' shouts and revving engines hot on your heels.'.
                    'Thorns snag your clothes, tearing at your skin as you burst through the TUNNEL exit and emerge at the crash site, winded and desperate.'.
                    'The icy wind bites at your face, a cruel reminder of the surface''s hostility.'.
                    ''
                }
            ] ifFalse: [
                self addTask: 'act_finished'.
                gameOverMsg := {
                    'You bolt through the undergrowth, the Nazis'' shouts and revving engines hot on your heels.'.
                    'Thorns snag your clothes, tearing at your skin as you burst through the TUNNEL exit and emerge at the crash site, winded and desperate.'.
                    'The icy wind bites at your face, a cruel reminder of the surface''s hostility.'.
                    ''.
                    'You and Clara huddle in the wreckage, the valley''s secrets slipping away as the cold closes in.'.
                    'Survival hangs by a thread, your fate uncertain.'.
                    'GAME OVER.'.
                    ''
                }
            ].
            
            self printLines: gameOverMsg.
            ^ nil
        ].
        
        ^ nil
    ]

    handleTake: objName [
        | name obj here |
        name := objName asLowercase.
        here := self findHere: name.
        
        here ifNil: [ ^ self printLines: { 'I don''t see ' , objName , ' here.' . '' } ].
        
        (self isInInventory: name) ifTrue: [ 
            ^ self printLines: { 'You''re already holding it!' . '' } 
        ].
        
        self addToInventory: here.
        self removeFromLocation: here.
        self printLines: { 'You take the ' , here name , '.' . '' }
    ]

    handleDrop: objName [
        | name obj |
        name := objName asLowercase.
        obj := inventory detect: [:e | e name = name] ifNone: [ nil ].
        obj ifNil: [ ^ self printLines: { 'You aren''t holding that!' . '' } ].
        
        " Special handling for pistol during fight "
        (name = 'pistol' and: [self hasTask: 'fight'] and: [currentLocation = #'rock']) ifTrue: [
            ^ self handlePistolFight
        ].
        
        self removeFromInventory: obj.
        self printLines: { 'OK.' . '' }
    ]

    handlePistolFight [
        | success pistol |
        pistol := self findEntity: 'pistol'.
        self removeFromInventory: pistol.
        self removeTask: 'fight'.
        self addTask: 'after_fight'.
        
        " Random success/failure "
        success := (1 to: 10) atRandom > 5.
        
        success ifTrue: [
            self printLines: {
                'You hand the PISTOL to Clara.'.
                'She aims the old Mauser and fires-a sharp crack echoes through the valley, the shot strikes one of the soldiers, who collapses with a cry.'.
                'The Nazis roar in fury, their rifles spitting fire in response.'.
                'Bullets chip the rock, showering you with dust and shards.'.
                'The leader bellows, his voice thick with venom:'.
                ''.
                'Nazi Leader: "Ihr wagt es, uns herauszufordern? Euer Blut wird dieses Tal beflecken!"'.
                ''
            }
        ] ifFalse: [
            self printLines: {
                'You hand the PISTOL to Clara.'.
                'She aims the old Mauser and pulls the trigger, but it jams with a dull click, refusing to fire.'.
                'The Nazis burst into mocking laughter, their sneers cutting through the air.'.
                'The leader steps forward, his voice dripping with scorn:'.
                ''.
                'Nazi Leader: "Ihr erbarmlichen Narren! Dafur werdet ihr sterben!"'.
                ''
            }
        ].
        ^ nil
    ]

    handleUse: objName [
        | name |
        name := objName asLowercase.
        
        (name = 'radio' and: [self isInInventory: 'radio'] and: [self hasTask: 'tunnel']) ifTrue: [
            self removeTask: 'tunnel'.
            self addTask: 'radio'.
            ^ self printLines: {
                'The RADIO crackles in your hands, its three dials labeled A, B, and C glinting faintly in the dim light of the crash site.'.
                ''.
                'Each dial can be set to a number between 1 and 9.'.
                'A faded, crumpled NOTE taped to the side reads: "Marine Corps Frequency: Alpha-Bravo-Charlie."'.
                'The wind howls outside, urging you to hurry.'.
                ''
            }
        ].
        
        (name = 'radio' and: [self isInInventory: 'radio'] and: [self hasTask: 'radio']) ifTrue: [
            ^ self tuneRadio
        ].
        
        (name = 'geiger' and: [self isInInventory: 'geiger']) ifTrue: [
            ^ self printLines: { 'Radiation levels - normal.' . '' }
        ].
        
        (self isInInventory: name) 
            ifTrue: [ self printLines: { 'I can''t use that right now.' . '' } ]
            ifFalse: [ self printLines: { 'I don''t have it or I can''t use it.' . '' } ]
    ]

    tuneRadio [
        | a b c result |
        Transcript show: 'Set dial A to: '.
        a := stdin nextLine asInteger.
        Transcript show: 'Set dial B to: '.
        b := stdin nextLine asInteger.
        Transcript show: 'Set dial C to: '.
        c := stdin nextLine asInteger.
        
        result := self processRadioTuning: a B: b C: c.
        self printLines: result.
        ^ nil
    ]

    processRadioTuning: a B: b C: c [
        | wrongCount |
        wrongCount := 0.
        (a ~= 2) ifTrue: [ wrongCount := wrongCount + 1 ].
        (b ~= 7) ifTrue: [ wrongCount := wrongCount + 1 ].
        (c ~= 4) ifTrue: [ wrongCount := wrongCount + 1 ].
        
        wrongCount = 0 ifTrue: [
            self removeTask: 'radio'.
            self removeFromInventory: (self findEntity: 'radio').
            self addTask: 'after_radio'.
            currentLocation := #'cockpit'.
            ^ {
                'The RADIO hums as it locks onto a strong signal. A clear voice cuts through:'.
                ''.
                '"Mission 334, this is the 32nd Marine Corps. Coordinates received. Extraction team inbound. Over."'.
                'Clara: "Copy that. We''ll hold tight. Over."'.
                'Marine: "Copy that. We''re tracking your signal. Hold tight, over."'.
                ''.
                'The steady signal brings a flicker of relief amidst the chaos.'.
                ''.
                'After a tense wait, the roar of engines fills the air. A Marine transport plane descends through the snow, its lights cutting through the gloom.'.
                'You and CLARA board, the warmth of the cabin a stark contrast to the biting cold.'.
                'As the plane lifts off, a Marine hands you a stack of nondisclosure agreements.'.
                ''.
                'Marine: "Sign these. What you saw down there stays buried. Understood?"'.
                ''.
                'You nod, a heavy, unspoken weight settling over you.'.
                'The valley''s mysteries fade into the distance, shrouded in silence, as the plane carries you away.'.
                ''
            }
        ].
        
        wrongCount = 1 ifTrue: [
            ^ {
                'The RADIO picks up a faint Marine transmission, but it''s garbled:'.
                ''.
                '"Mission... [static]... coordinates... [static]... hold..."'.
                'Clara: "Almost there, but it''s too weak. They won''t get our position like this."'.
                ''.
                'HINT: "The plaque mentions ''A=Even, B=Prime, C=Square.'' And the note says ''Four''s the square, Seven''s luck, Two''s pair''-could A be 2?"'.
                ''
            }
        ].
        
        wrongCount = 2 ifTrue: [
            ^ {
                'A sharp burst of static erupts from the RADIO, followed by a chilling German voice:'.
                ''.
                '"Achtung! Feindliche Ubertragung entdeckt!"'.
                'Clara: "That''s the Germans-they''ve intercepted us. We''ve got to fix this now!"'.
                ''.
                'HINT: "The plaque mentions ''A=Even, B=Prime, C=Square.'' And the note says ''Four''s the square, Seven''s luck, Two''s pair''-could A be 2?"'.
                ''
            }
        ].
        
        ^ {
            'The RADIO hisses with static, a grating buzz drowning out any signal.'.
            'You: "Just noise. This isn''t the right frequency."'.
            ''.
            'HINT: "Think it through: 2 is even, 7 is prime, and 4 ties to the square of 2. That matches all the clues."'.
            ''
        }
    ]

    printActProlog [
        self printLines: {
            'ACT 3: INTO THE HEART OF THE UNKNOWN'.
            ''.
            'You and Clara carefully climb down from the ledge, your boots sinking into the soft, mossy ground.'.
            'The valley pulses with life-chirping insects fill the air, leaves rustle in a gentle breeze, and the distant roar of an unseen beast sends a shiver down your spine.'.
            'The memory of your crash-landed supplies lingers, a heavy burden as you take your first cautious steps into this strange, uncharted world.'.
            ''
        }
    ]

    mainLoop [
        | input cmdArray res |
        self printActProlog.
        self doLook.
        
        [ true ] whileTrue: [
            res = #next ifTrue: [ ^ self extractPlayerState ].
            res = #quit ifTrue: [ ^ nil ].
            
            self prompt.
            input := stdin nextLine.
            cmdArray := self parseCommand: input.
            cmdArray first = #quit ifTrue: [ ^ nil ].
            res := self handleCommand: cmdArray.
        ]
    ]
] !