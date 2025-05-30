BaseGame subclass: Act1Game [

    setupLocations [
        | oc |
        locations := Dictionary new.

        locations at: #'yard' put: OrderedCollection new.

        oc := OrderedCollection new.
        oc add: (Entity itemNamed: 'photo' description: 'A photo of your late wife sits on the dresser.' takeable: false);
           add: (Entity itemNamed: 'lighter' description: 'A simple silver lighter. You should really quit smoking.' takeable: true);
           add: (Entity itemNamed: 'calendar' description: 'August 26, 1946.' takeable: false).
        locations at: #'barrack' put: oc.

        oc := OrderedCollection new.
        oc add: (Entity personNamed: 'clara' description: 'Clara stands near the plane, wearing a military pilot''s uniform with rolled-up sleeves.');
           add: (Entity itemNamed: 'plane' description: 'Your type served well in the war.' takeable: false);
           add: (Entity itemNamed: 'tanks' description: 'Fuel tanks for the plane. They''re running low.' takeable: false).
        locations at: #'runway' put: oc.

        locations at: #'depot' put: (OrderedCollection with: 
            (Entity itemNamed: 'canister' description: 'A heavy fuel canister. Necessary for the journey.' takeable: true)).

        oc := OrderedCollection new.
        oc add: (Entity itemNamed: 'food' description: 'Canned goods and dried meals.' takeable: true);
           add: (Entity itemNamed: 'water' description: 'Fresh water in sealed containers.' takeable: true);
           add: (Entity itemNamed: 'geiger' description: 'A standard radiation detector.' takeable: true);
           add: (Entity itemNamed: 'medkit' description: 'Bandages, antiseptic, morphine…' takeable: true);
           add: (Entity itemNamed: 'radio' description: 'A shortwave field radio.' takeable: true);
           add: (Entity itemNamed: 'gear' description: 'Ropes, pitons, carabiners.' takeable: true);
           add: (Entity itemNamed: 'tools' description: 'A compass, maps, and a sextant.' takeable: true);
           add: (Entity itemNamed: 'list' description: 'A supply list showing available items.' takeable: false).
        locations at: #'tent' put: oc.

        currentLocation := #'yard'
    ]

    canMoveFrom: fromLoc to: toLoc [
        (fromLoc = #'yard' and: [toLoc = #'barrack']) ifTrue: [ ^ true ].
        (fromLoc = #'yard' and: [toLoc = #'runway']) ifTrue: [ ^ true ].
        (fromLoc = #'yard' and: [toLoc = #'depot']) ifTrue: [ ^ true ].
        (fromLoc = #'yard' and: [toLoc = #'tent']) ifTrue: [ ^ true ].
        (fromLoc = #'barrack' and: [toLoc = #'yard']) ifTrue: [ ^ true ].
        (fromLoc = #'runway' and: [toLoc = #'yard']) ifTrue: [ ^ true ].
        (fromLoc = #'depot' and: [toLoc = #'yard']) ifTrue: [ ^ true ].
        (fromLoc = #'tent' and: [toLoc = #'yard']) ifTrue: [ ^ true ].
        ^ false
    ]

    parseLocation: aString [
        | s |
        s := aString asLowercase.
        s = 'yard'    ifTrue: [ ^ #'yard' ].
        s = 'barrack' ifTrue: [ ^ #'barrack' ].
        s = 'runway'  ifTrue: [ ^ #'runway' ].
        s = 'depot'   ifTrue: [ ^ #'depot' ].
        s = 'tent'    ifTrue: [ ^ #'tent' ].
        ^ #'unknown'
    ]

    descriptionOf: location [
        location = #'yard' ifTrue: [ ^ self yardDescription ].
        location = #'barrack' ifTrue: [ ^ self barrackDescription ].
        location = #'runway' ifTrue: [ ^ self describeRunway ].
        location = #'depot' ifTrue: [ ^ self depotDescription ].
        location = #'tent' ifTrue: [ ^ self tentDescription ].
        ^ 'You see nothing special.'
    ]

    doLook [
        currentLocation = #'runway' ifTrue: [
            self printLine: (self describeRunway).
            self printLine: ''.
            ^ self
        ].
        super doLook
    ]

    yardDescription [
        ^ 'You''re on the BARRACK yard. Nearby, a sturdy twin-engine plane rests' , Character lf asString,
          'on a makeshift RUNWAY, its metal hull glinting faintly in the weak sunlight.' , Character lf asString,
          'To the side, there''s a fuel DEPOT and a supply TENT. The air is frigid,' , Character lf asString,
          'the wind howls intermittently, and the isolation weighs heavily. By the plane,' , Character lf asString,
          'you spot your partner, Lt. CLARA Voss, a pragmatic military pilot assigned' , Character lf asString,
          'to join you on this mission.'
    ]

    barrackDescription [
        ^ 'This is your resting place during the mission - small but convenient.' , Character lf asString,
          'Your bed is neatly made, and a PHOTO of your late wife sits on the dresser beside it.' , Character lf asString,
          'Across the room, your working desk holds mission documents, a small lamp, and a LIGHTER.' , Character lf asString,
          'A CALENDAR hangs above the desk.' , Character lf asString,
          'Outside - the YARD, covered in snow.'
    ]

    depotDescription [
        ^ 'You step into the depot, a rough but functional structure shielding fuel CANISTERs from the Antarctic cold.' , Character lf asString,
          'Outside - the YARD, covered in snow.'
    ]

    tentDescription [
        ^ 'You enter the supply tent, a cramped space cluttered with gear.' , Character lf asString,
          'Boxes and crates are labeled with essentials: FOOD, WATER, scientific tools, and survival equipment.' , Character lf asString,
          'A LIST of stock hangs on the wall.' , Character lf asString,
          'Outside - the YARD, covered in snow.'
    ]

    describeRunway [
        (self countSupplies) > 0 ifTrue: [
            ^ 'Clara has finished fueling and has something waiting for you.' , Character lf asString,
              'You pack the supplies into the plane. The reason for your journey -' , Character lf asString,
              'Admiral Byrd''s diary - lies open on a box in front of you, its cryptic' , Character lf asString,
              'coordinates circled in red ink: 70S, 10E.' , Character lf asString,
              'Clara hands you a cup of lukewarm coffee.'
        ].
        
        (self isInInventory: 'canister') not ifTrue: [
            ^ 'The sunlight, reflected off the steel plates, blinds you as you approach' , Character lf asString,
              'the aircraft - a Douglas A-20 Havoc. It''s not the newest PLANE, but it''s' , Character lf asString,
              'reliable. CLARA is tinkering with one of the engines.' , Character lf asString,
              'Behind you - the YARD, covered in snow.'
        ].
        
        ^ 'The sunlight, reflected off the steel plates, blinds you as you approach' , Character lf asString,
          'the Douglas A-20 Havoc - a reliable, if not modern, PLANE.' , Character lf asString,
          'Clara is still tinkering with one of the engines.' , Character lf asString,
          'Behind you - the YARD, covered in snow.'
    ]

    getHint [
        (self hasTask: 'act_finished') ifTrue: [ 
            ^ 'You''ve already finished this act. Type "quit" to leave the game.' 
        ].
        
        (currentLocation = #'barrack' and: [(self isInInventory: 'lighter') not]) ifTrue: [
            ^ 'I should gather something useful.'
        ].
        
        ((self hasTask: 'fuel_request') and: [(self hasTask: 'can_take_canister') not]) ifTrue: [
            ^ 'I should check the fuel TANKS.'
        ].
        
        ((self hasTask: 'fuel_request') and: [(self isInInventory: 'canister') not]) ifTrue: [
            ^ 'I should gather some fuel from the DEPOT.'
        ].
        
        ((self hasTask: 'fuel_request') and: [self isInInventory: 'canister']) ifTrue: [
            ^ 'I should give the CANISTER to Clara.'
        ].
        
        ((self hasTask: 'collect_supplies') and: [currentLocation = #'runway'] and: [(self countSupplies) > 0]) ifTrue: [
            ^ 'I should thank her for the coffee.'
        ].
        
        ((self hasTask: 'collect_supplies') and: [currentLocation ~= #'tent']) ifTrue: [
            ^ 'I should gather supplies in the supply TENT.'
        ].
        
        ((self hasTask: 'collect_supplies') and: [currentLocation = #'tent']) ifTrue: [
            ^ 'I should take only the most necessary items for the mission.'
        ].
        
        ^ 'I think I should talk with Clara.'
    ]

    examineSpecial: key [
        (key = 'tanks' and: [currentLocation = #'runway']) ifTrue: [
            self markExamined: 'tanks'.
            self addTask: 'can_take_canister'.
            self printLines: {
                'You crouch beside the aircraft and open the fuel hatch.'.
                'We''re running low. We need at least one more drum of fuel.'.
                'Clara: "Told you. Go grab one from the DEPOT."'.
                ' '
            }.
            ^ true
        ].
        
        (key = 'list' and: [currentLocation = #'tent']) ifTrue: [
            self markExamined: 'list'.
            self printLines: {
                '- FOOD rations'.
                '- WATER'.
                '- GEIGER Counter'.
                '- MEDKIT'.
                '- RADIO'.
                '- Climbing GEAR'.
                '- Navigation TOOLS'.
                'The plane has a capacity of only 5 items; choose wisely.'.
                ''
            }.
            ^ true
        ].
        
        (key = 'photo' and: [currentLocation = #'barrack']) ifTrue: [
            self markExamined: 'photo'.
            self printLines: { 'I''ll never forget you, my love.' . '' }.
            ^ true
        ].
        
        (key = 'lighter' and: [currentLocation = #'barrack']) ifTrue: [
            self markExamined: 'lighter'.
            self printLines: { 'I really should quit smoking.' . '' }.
            ^ true
        ].
        
        (key = 'calendar' and: [currentLocation = #'barrack']) ifTrue: [
            self markExamined: 'calendar'.
            self printLines: { 'August 26, 1946' . '' }.
            ^ true
        ].
        
        " Tent items "
        ((#('food' 'water' 'geiger' 'medkit' 'radio' 'gear' 'tools') includes: key) and: [currentLocation = #'tent']) ifTrue: [
            self markExamined: key.
            self printTentItemDescription: key.
            ^ true
        ].
        
        (key = 'canister' and: [currentLocation = #'depot']) ifTrue: [
            (self hasTask: 'can_take_canister') ifFalse: [
                self printLines: { 'I should check the fuel tanks first.' . '' }.
                ^ true
            ].
            self markExamined: 'canister'.
            self printLines: { 'Heavy, but necessary.' . '' }.
            ^ true
        ].
        
        (key = 'clara' and: [currentLocation = #'runway']) ifTrue: [
            self printLines: {
                'Clara stands near the plane, wearing a military pilot''s uniform with rolled-up sleeves.'.
                'Her dark hair is tied back, with a few strands escaping to frame her sharp eyes.'.
                ''
            }.
            ^ true
        ].
        
        (key = 'clara') ifTrue: [
            self printLines: { 'I can''t see her clearly from here.' . '' }.
            ^ true
        ].
        
        (key = 'plane' and: [currentLocation = #'runway']) ifTrue: [
            self markExamined: 'plane'.
            self printLines: { 'Your type served well in the war.' . '' }.
            ^ true
        ].
        
        ^ nil
    ]

    printTentItemDescription: key [
        key = 'food' ifTrue: [
            self printLines: {
                'Canned goods and dried meals.'.
                'Enough to last two weeks, but not exactly gourmet'.
                ''
            }
        ].
        key = 'water' ifTrue: [
            self printLines: {
                'Fresh, sealed water cans.'.
                'A week''s worth if we ration.'.
                ''
            }
        ].
        key = 'geiger' ifTrue: [
            self printLines: {
                'A standard radiation detector.'.
                'If we stumble upon something unnatural, this could be crucial.'.
                ''
            }
        ].
        key = 'medkit' ifTrue: [
            self printLines: {
                'Bandages, antiseptic, morphine...'.
                'Everything needed for basic field medical care.'.
                ''
            }
        ].
        key = 'radio' ifTrue: [
            self printLines: {
                'A shortwave field radio.'.
                'Not the best range, but it should work if we''re within contact distance of the base.'.
                ''
            }
        ].
        key = 'gear' ifTrue: [
            self printLines: {
                'Ropes, pitons, carabiners.'.
                'If we need to descend into something deep or climb out of trouble, this will help.'.
                ''
            }
        ].
        key = 'tools' ifTrue: [
            self printLines: {
                'A compass, maps, and a sextant.'.
                'Old-school but reliable.'.
                ''
            }
        ]
    ]

    handleTalk: whoName [
        whoName asLowercase = 'clara' ifTrue: [
            currentLocation = #'runway' ifFalse: [
                self addTask: 'go_to_clara'.
                ^ self printLines: { 'I''m not going to shout; I should go to her at the RUNWAY.' . '' }
            ].
            ^ self dialogWithClara
        ].
        
        self printLines: { 'There''s no one here to talk to by that name.' . '' }
    ]

    dialogWithClara [
        " Check various dialog states "
        ((self hasTask: 'fuel_request') and: [self isInInventory: 'canister']) ifTrue: [
            ^ self claraFuelComplete
        ].
        
        (self hasTask: 'collect_supplies') ifTrue: [
            (self countSupplies) > 0 ifTrue: [
                (self hasTask: 'explain_stage') ifFalse: [
                    self addTask: 'awaiting_explain_choice'.
                    self addTask: 'explain_stage'.
                    ^ self showExplainDialog
                ].
            ].
        ].
        
        (self hasTalked: 'clara' topic: 'fuel_request') ifFalse: [
            self markTalked: 'clara' topic: 'fuel_request'.
            self addTask: 'awaiting_clara_choice'.
            ^ self showInitialClaraDialog
        ].
        
        self printLines: { 'Clara is busy working on the plane.' . '' }
    ]

    showInitialClaraDialog [
        self printLines: {
            'Clara: "Morning, doc. We need to get this bird fueled up. Could you check the TANKS for me?"'.
            'Your choices:'.
            '1. "Okay, I''ll handle it now."'.
            '2. "I think it''s good enough, but I could double-check."'.
            '3. "Why don''t you take care of it?"'
        }.
        ^ nil
    ]

    showExplainDialog [
        self printLines: {
            'You: "Thank you!"'.
            '*a moment of silence*'.
            'Clara: "So, tell me again why we''re risking our necks for this?'.
            'A diary from some explorer doesn''t scream ''top priority'' to me."'.
            'Your choices:'.
            '1. "Because it could be the discovery of the century."'.
            '2. "Orders are orders. The government wants answers."'.
            '3. "I''ve got a feeling there''s something big waiting for us."'.
            ''
        }.
        ^ nil
    ]

    processChoice: choice context: context [
        context = #clara ifTrue: [ ^ self processClaraChoice: choice ].
        context = #explain ifTrue: [ ^ self processExplainChoice: choice ].
        context = #further ifTrue: [ ^ self processFurtherChoice: choice ].
        context = #supply ifTrue: [ ^ self processSupplyChoice: choice ].
        ^ self printLine: 'Invalid choice context.'
    ]

    processClaraChoice: choice [
        self removeTask: 'awaiting_clara_choice'.
        
        choice = 1 ifTrue: [
            self addTask: 'fuel_request'.
            self printLines: { 'You: "Okay, I''ll handle it now."' . '' }
        ].
        
        choice = 2 ifTrue: [
            self addTask: 'fuel_request'.
            self printLines: {
                'You: "I think it''s good enough, but I could double-check."'.
                'Clara: "Good enough doesn''t cut it out here. Antarctica doesn''t forgive mistakes. Check it properly."'.
                ''
            }
        ].
        
        choice = 3 ifTrue: [
            self addTask: 'collect_supplies'.
            self printLines: {
                'You: "Why don''t you take care of it?"'.
                'Clara (frowning): "Oh, you''re lazy, aren''t you? Fine, I''ll handle it after I check the oil,'.
                'but you''re not off the hook, doc. Go gather mandatory supplies and drop them near the plane."'.
                ''
            }
        ]
    ]

    processExplainChoice: choice [
        self removeTask: 'awaiting_explain_choice'.
        
        choice = 1 ifTrue: [
            self addTask: 'awaiting_further_choice'.
            self printLines: {
                'You: "Because it could be the discovery of the century."'.
                'Clara: "Discovery of the century? I hope it''s not just a pile of ice and a frostbite bill."'.
                'Your choice:'.
                '1. "Byrd wasn''t a dreamer. Those coordinates mean something."'.
                '2. "Even if it''s nothing, the science alone is worth it."'
            }.
            ^ self waitForChoice: #further
        ].
        
        choice = 2 ifTrue: [
            self printLines: {
                'You: "Orders are orders. The government wants answers."'.
                'Clara: "Yeah, and Uncle Sam loves sending us into the freezer for kicks. What''s their angle?"'.
                'You: "Cold War jitters, probably. They don''t want the Soviets sniffing around first."'
            }.
            ^ self showActEpilog
        ].
        
        choice = 3 ifTrue: [
            self printLines: {
                'You: "I''ve got a feeling there''s something big waiting for us."'.
                'Clara: "Feelings don''t keep us warm, doc. What''s in that diary that''s got you hooked?"'.
                'You: "Hints of a hidden land - geological oddities, maybe more."'
            }.
            ^ self showActEpilog
        ]
    ]

    processFurtherChoice: choice [
        self removeTask: 'awaiting_further_choice'.
        
        choice = 1 ifTrue: [
            self printLines: {
                'You: "Byrd wasn''t a dreamer. Those coordinates mean something."'.
                'Clara: "Maybe. But I''d rather not die proving him right."'
            }
        ].
        
        choice = 2 ifTrue: [
            self printLines: {
                'You: "Even if it''s nothing, the science alone is worth it."'.
                'Clara: "Maybe. But I''d rather not die proving him right."'
            }
        ].
        
        ^ self showActEpilog
    ]

    processSupplyChoice: choice [
        self removeTask: 'awaiting_supply_choice'.
        self addTask: 'collect_supplies'.
        
        choice = 1 ifTrue: [
            self printLines: { 'You: "On my way."' . '' }
        ].
        
        choice = 2 ifTrue: [
            self printLines: {
                'You: "Are you sure you can handle it by yourself?"'.
                'Clara: "Don''t worry, doc - I''m not a kid. Go grab those supplies."'.
                ''
            }
        ]
    ]

    claraFuelComplete [
        | canister |
        canister := inventory detect: [:e | e name = 'canister'] ifNone: [nil].
        canister ifNotNil: [
            self removeFromInventory: canister.
            self removeTask: 'fuel_request'.
            self addTask: 'awaiting_supply_choice'.
            
            self printLines: {
                'You: "I have it!"'.
                'Clara: "Nice, hand it over - our bird''s thirsty."'.
                '*starts fueling the plane*'.
                'Clara: "Why don''t you gather some supplies while I finish fueling?"'.
                'Your choices:'.
                '1. "On my way"'.
                '2. "Are you sure you can handle it by yourself?"'
            }.
            
            ^ self waitForChoice: #supply
        ]
    ]

    showActEpilog [
        self addTask: 'act_finished'.
        self printLines: {
            ''.
            'You: "What do you think we''ll find out there?"'.
            'Clara: "Best case? A rock formation worth naming. Worst case? A grave with our names on it.'.
            'I don''t buy the unearthly land garbage."'.
            'You: "Neither do I, but the government does."'.
            'Clara: "I think it''s time we have a good weather."'.
            ''.
            'Preparations complete, you and Clara climb into the plane''s hatch.'.
            'Clara starts the engines, ready to challenge the icy wilderness.'.
            'The plane roars to life, cutting through swirling snow as it lifts off.'.
            ''.
            'Inside, you study the diary while Clara grips the yoke.'.
            'The horizon swallows the base camp, leaving you with a mix of anticipation -'.
            'and a hint of lurking danger.'.
            ''.
            '----------------------------ACT 1 OVER----------------------------'.
            ''.
            'Type "next" to continue, or "quit" to leave.'.
            ''
        }
    ]

    handleSpecialCommand: aCmdArray [
        | cmd |
        cmd := aCmdArray first.
        
        " Handle choice inputs "
        (self hasTask: 'awaiting_clara_choice') ifTrue: [
            ^ self handleChoiceInput: aCmdArray context: #clara
        ].
        
        (self hasTask: 'awaiting_explain_choice') ifTrue: [
            ^ self handleChoiceInput: aCmdArray context: #explain
        ].
        
        (self hasTask: 'awaiting_further_choice') ifTrue: [
            ^ self handleChoiceInput: aCmdArray context: #further
        ].
        
        (self hasTask: 'awaiting_supply_choice') ifTrue: [
            ^ self handleChoiceInput: aCmdArray context: #supply
        ].
        
        ^ nil
    ]

    handleChoiceInput: aCmdArray context: context [
        | input choice |
        input := aCmdArray first asString.
        choice := input asInteger.
        (choice between: 1 and: 3) ifTrue: [
            ^ self processChoice: choice context: context
        ].
        self printLine: 'Invalid choice - enter 1, 2, or 3.'.
        ^ nil
    ]

    handleTake: objName [
        | obj supplies |
        
        obj := self findHere: objName asLowercase.
        obj isNil ifTrue: [ ^ self printLines: { 'I don''t see ' , objName , ' here.' . '' } ].
        
        (self isInInventory: objName) ifTrue: [ 
            ^ self printLines: { 'You''re already holding it!' . '' } 
        ].
        
        " Special handling for specific items "
        objName asLowercase = 'photo' ifTrue: [
            ^ self printLines: { 'Sorry, my love, but I can''t take you with me.' . '' }
        ].
        
        objName asLowercase = 'calendar' ifTrue: [
            ^ self printLines: { 'I doubt this will be useful.' . '' }
        ].
        
        objName asLowercase = 'lighter' ifTrue: [
            ^ self printLines: { 'I''m not going to smoke now.' . '' }
        ].
        
        objName asLowercase = 'canister' ifTrue: [
            (self hasTask: 'can_take_canister') ifFalse: [
                ^ self printLines: { 'I should check the fuel TANKS first.' . '' }
            ].
            self addToInventory: obj.
            self removeFromLocation: obj.
            ^ self printLines: { 'This should be enough.' . '' }
        ].
        
        " Supply handling "
        (self isSupply: obj name) ifTrue: [
            (self hasTask: 'collect_supplies') ifFalse: [
                ^ self printLines: { 'I don''t need this right now. I should TALK to Clara first.' . '' }
            ].
            
            supplies := self countSupplies.
            supplies >= 5 ifTrue: [
                ^ self printLines: { 'You cannot take this - you''ve reached the limit (5 items).' . '' }
            ].
            
            self addToInventory: obj.
            self removeFromLocation: obj.
            ^ self printLines: { 'You take the ' , obj name , '.' . '' }
        ].
        
        " Default takeable items "
        obj takeableByDefault ifTrue: [
            self addToInventory: obj.
            self removeFromLocation: obj.
            ^ self printLines: { 'You take the ' , obj name , '.' . '' }
        ].
        
        self printLines: { 'You can''t take that.' . '' }
    ]

    handleDrop: objName [
        | obj |
        obj := inventory detect: [:e | e name = objName asLowercase] ifNone: [ nil ].
        obj isNil ifTrue: [ ^ self printLines: { 'You aren''t holding it!' . '' } ].
        
        objName asLowercase = 'lighter' ifTrue: [
            self removeFromInventory: obj.
            (self entitiesAt: currentLocation) add: obj.
            ^ self printLines: { 'I might need something else instead.' . '' }
        ].
        
        objName asLowercase = 'canister' ifTrue: [
            ^ self printLines: { 'I should get it to Clara. I''ll TALK to her.' . '' }
        ].
        
        (self isSupply: obj name) ifTrue: [
            self removeFromInventory: obj.
            (self entitiesAt: currentLocation) add: obj.
            ^ self printLines: { 'You drop the ' , obj name , '.' . '' }
        ].
        
        self removeFromInventory: obj.
        (self entitiesAt: currentLocation) add: obj.
        self printLines: { 'OK.' . '' }
    ]

    handleUse: objName [
        (objName asLowercase = 'canister' and: [self isInInventory: 'canister'] and: [currentLocation = #'runway']) ifTrue: [
            ^ self printLines: { 'I should give it to Clara. I''ll TALK to her.' . '' }
        ].
        
        (self isInInventory: objName asLowercase) 
            ifTrue: [ self printLines: { 'I can''t use that right now.' . '' } ]
            ifFalse: [ self printLines: { 'I don''t have it or I can''t use it.' . '' } ]
    ]

    printActProlog [
        self printLines: {
            'ACT 1: DEPARTURE FROM THE EDGE OF THE WORLD'.
            ''.
            'You awaken to a stark view from your window at an Antarctic base camp in'.
            'New Swabia. A desolate expanse of ice and snow stretches endlessly under'.
            'a pale, gray sky. You get up, dress in layers against the cold and step'.
            'outside.'.
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