Object subclass: Entity [
    | name description takeable |

    Entity class >> itemNamed: aName description: aDesc takeable: aBool [
        ^ self new
            name: aName;
            description: aDesc;
            takeable: aBool;
            yourself
    ]

    Entity class >> personNamed: aName description: aDesc [
        ^ self itemNamed: aName description: aDesc takeable: false
    ]

    name [ ^ name ]
    name: aString [ name := aString ]
    description [ ^ description ]
    description: aString [ description := aString ]
    takeable [ ^ takeable ]
    takeable: aBoolean [ takeable := aBoolean ]
    printOn: aStream [ aStream nextPutAll: name ]
] !


Object subclass: PlayerState [
    | inventory |

    PlayerState class >> new [
        ^ super new initialize
    ]

    initialize [
        inventory := OrderedCollection new.
        ^ self
    ]

    inventory [ ^ inventory ]
    inventory: anInventory [ inventory := anInventory ]

    addToInventory: anEntity [ inventory add: anEntity ]
    
    removeFromInventory: anEntity [ 
        | entity |
        entity := inventory detect: [:e | e name = anEntity name] ifNone: [nil].
        entity ifNotNil: [inventory remove: entity]
    ]

    hasItem: aString [ 
        ^ inventory anySatisfy: [:e | e name = aString asLowercase] 
    ]
] !

Object subclass: BaseGame [
    | currentLocation locations inventory examined talked tasks |

    BaseGame class >> new [ ^ super new initialize ]

    initialize [
        currentLocation := #'start'.
        inventory := OrderedCollection new.
        examined := Set new.
        talked := Set new.
        tasks := Set new.
        self setupLocations.
        ^ self
    ]

    " Subclasses should override this "
    setupLocations [ locations := Dictionary new ]

    " Location management "
    currentLocation [ ^ currentLocation ]
    currentLocation: aLocation [ currentLocation := aLocation ]

    " Task management "
    addTask: aString [ tasks add: aString ]
    removeTask: aString [ tasks remove: aString ifAbsent: [] ]
    hasTask: aString [ ^ tasks includes: aString ]

    " Inventory management "
    addToInventory: anEntity [ inventory add: anEntity ]
    
    removeFromInventory: anEntity [ 
        | entity |
        entity := inventory detect: [:e | e name = anEntity name] ifNone: [nil].
        entity ifNotNil: [inventory remove: entity]
    ]

    hasItem: aString [ 
        ^ inventory anySatisfy: [:e | e name = aString asLowercase] 
    ]
    
    isInInventory: aString [ ^ self hasItem: aString ]

    " Location and entity management "
    entitiesAt: aSymbol [ 
        ^ (locations at: aSymbol ifAbsent: [ OrderedCollection new ]) 
    ]

    findHere: aString [
        ^ (self entitiesAt: currentLocation)
            detect: [:e | e name = aString asLowercase]
            ifNone: [ nil ]
    ]

    findEntity: aString [
        | here |
        here := self findHere: aString.
        here ifNotNil: [ ^ here ].
        ^ inventory detect: [:e | e name = aString asLowercase] ifNone: [ nil ]
    ]

    removeFromLocation: anEntity [
        | entities |
        entities := self entitiesAt: currentLocation.
        entities remove: anEntity ifAbsent: []
    ]

    " Examination system "
    markExamined: aString [ examined add: aString ]
    hasExamined: aString [ ^ examined includes: aString ]

    " Talking system "
    markTalked: person topic [ talked add: person , '_' , topic ]
    hasTalked: person topic [ ^ talked includes: person , '_' , topic ]

    " Supply checking "
    isSupply: aString [
        ^ #( 'food' 'water' 'geiger' 'medkit' 'radio'
              'gear' 'tools' ) includes: aString asLowercase
    ]

    countSupplies [
        ^ inventory count: [:e | self isSupply: e name]
    ]

    " I/O methods "
    prompt [ Transcript show: '> ' ]
    printLine: aString [ Transcript show: aString; cr ]
    printLines: aCollection [
        aCollection do: [:ln | self printLine: ln ].
    ]


    " Common command parsing "
    parseCommand: aString [
        | parts first |
        parts := (aString asLowercase) subStrings: ' '.
        parts isEmpty ifTrue: [ ^ (Array with: #unknown) ].
        first := parts first.
        first = 'look'         ifTrue: [ ^ #( #look ) ].
        first = 'inventory'    ifTrue: [ ^ #( #inventory ) ].
        first = 'quit'         ifTrue: [ ^ #( #quit ) ].
        first = 'hint'         ifTrue: [ ^ #( #hint ) ].
        first = 'instructions' ifTrue: [ ^ #( #instructions ) ].
        first = 'next'         ifTrue: [ ^ #( #next ) ].
        first = 'go'        ifTrue: [ parts size > 1 ifTrue: [ ^ (Array with: #go with: (parts at: 2)) ] ].
        first = 'take'      ifTrue: [ parts size > 1 ifTrue: [ ^ (Array with: #take with: (parts at: 2)) ] ].
        first = 'drop'      ifTrue: [ parts size > 1 ifTrue: [ ^ (Array with: #drop with: (parts at: 2)) ] ].
        first = 'examine'   ifTrue: [ parts size > 1 ifTrue: [ ^ (Array with: #examine with: (parts at: 2)) ] ].
        first = 'talk'      ifTrue: [ parts size > 1 ifTrue: [ ^ (Array with: #talk with: (parts at: 2)) ] ].
        first = 'use'       ifTrue: [ parts size > 1 ifTrue: [ ^ (Array with: #use with: (parts at: 2)) ] ].
        ^ #( #unknown )
    ]

    " Common command implementations "
    doInventory [
        inventory isEmpty
            ifTrue: [ self printLine: 'You are not carrying anything.' ]
            ifFalse: [
                self printLine: 'You are carrying:'.
                inventory do: [:e | self printLine: e name ].
            ].
        self printLine: ''
    ]

    doInstructions [
        self printLines: #(
            'Available commands are:'
            'look               -- look around you and describe surroundings'
            'go <place>         -- go to a place'
            'examine <obj>      -- examine an object or person closely'
            'talk <person>      -- talk to someone'
            'take <obj>         -- pick up an object'
            'drop <obj>         -- put down an object'
            'use <obj>          -- use an object you''re carrying'
            'inventory          -- list currently held items'
            'instructions       -- see these instructions'
            'hint               -- get a hint if you''re stuck'
            'quit               -- end the game and quit'
            ''
        )
    ]

    " Methods to be overridden by subclasses "
    getHint [ ^ 'No hints available.' ]
    canMoveFrom: fromLoc to: toLoc [ ^ false ]
    descriptionOf: location [ ^ 'Nothing special here.' ]
    examineSpecial: key [ ^ nil ]
    handleSpecialCommand: cmdArray [ ^ nil ]

    " Common movement handling "
    handleGo: placeString [
        | loc |
        loc := self parseLocation: placeString.
        loc == #'unknown' ifTrue: [ ^ self printLines: { 'Unknown place: ' , placeString . '' } ].

        (self canMoveFrom: currentLocation to: loc)
            ifTrue: [
                currentLocation := loc.
                self printLine: (self descriptionOf: loc).
                self printLine: ''
            ]
            ifFalse: [ self printLines: { 'You can''t go to ' , placeString , ' from here.' . '' } ].
    ]

    " Common examination handling "
    handleExamine: objName [
        | result obj |
        result := self examineSpecial: objName asLowercase.
        result ifNotNil: [ ^ result ].
        
        obj := self findEntity: objName asLowercase.
        obj ifNotNil: [
            self markExamined: obj name.
            self printLines: { obj description . '' }
        ] ifNil: [
            self printLines: { 'I can''t see ' , objName , ' here or there''s nothing special about it.' . '' }
        ]
    ]

    " Default command handler "
    handleCommand: aCmdArray [
        | cmd arg result |
        cmd := aCmdArray first.
        arg := aCmdArray size > 1 ifTrue: [ aCmdArray second ] ifFalse: [ nil ].

        " Check for act completion "
        (self hasTask: 'act_finished') ifTrue: [
            cmd = #next ifTrue: [ ^ #next ].
            cmd = #quit ifTrue: [ ^ #quit ].
            self printLine: 'You''ve already finished this act. Type "quit" to exit or "next" to advance further.';
                 printLine: ''.
            ^ nil
        ].

        " Try special command handling first "
        result := self handleSpecialCommand: aCmdArray.
        result ifNotNil: [ ^ result ].

        " Standard commands "
        cmd = #look         ifTrue: [ ^ self doLook ].
        cmd = #inventory    ifTrue: [ ^ self doInventory ].
        cmd = #go           ifTrue: [ ^ self handleGo: arg ].
        cmd = #take         ifTrue: [ ^ self handleTake: arg ].
        cmd = #drop         ifTrue: [ ^ self handleDrop: arg ].
        cmd = #examine      ifTrue: [ ^ self handleExamine: arg ].
        cmd = #talk         ifTrue: [ ^ self handleTalk: arg ].
        cmd = #use          ifTrue: [ ^ self handleUse: arg ].
        cmd = #hint         ifTrue: [ ^ self doHint ].
        cmd = #instructions ifTrue: [ ^ self doInstructions ].
        cmd = #next         ifTrue: [ ^ self handleNext ].
        cmd = #quit         ifTrue: [ ^ #quit ].

        self printLine: 'Unknown command.'; printLine: ''.
    ]

    doLook [
        self printLine: (self descriptionOf: currentLocation); printLine: ''
    ]

    doHint [
        self printLine: (self getHint); printLine: ''
    ]

    handleNext [
        (self hasTask: 'act_finished') 
            ifTrue: [ ^ #next ]
            ifFalse: [ self printLine: 'You need to finish this act first.' ]
    ]

    " Methods to be implemented by subclasses "
    parseLocation: aString [ ^ #'unknown' ]
    handleTake: objName [ self printLine: 'Taking not implemented.' ]
    handleDrop: objName [ self printLine: 'Dropping not implemented.' ]
    handleTalk: whoName [ self printLine: 'Talking not implemented.' ]
    handleUse: objName [ self printLine: 'Using not implemented.' ]

    " State transfer methods "
    extractPlayerState [
        ^ PlayerState new
            inventory: inventory copy;
            yourself
    ]

    loadPlayerState: aPlayerState [
        inventory := aPlayerState inventory copy
    ]

    " Choice handling utility "
    waitForChoice: context [
        | input choice maxChoice |
        maxChoice := context = #radio ifTrue: [2] ifFalse: [
            context = #radio_background ifTrue: [2] ifFalse: [
                context = #ambush ifTrue: [4] ifFalse: [3]
            ]
        ].
        
        [
            Transcript show: 'Choose (1-' , maxChoice asString , '): '.
            input := stdin nextLine.
            choice := input asInteger.
            (choice between: 1 and: maxChoice) ifTrue: [
                ^ self processChoice: choice context: context
            ].
            self printLine: 'Invalid choice - enter 1-' , maxChoice asString , '.'.
        ] repeat
    ]

    processChoice: choice context: context [
        " Override in subclasses "
        ^ self printLine: 'Choice processing not implemented.'
    ]
] !

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


BaseGame subclass: Act2Game [

    setupLocations [
        | oc |
        locations := Dictionary new.

        " Cockpit "
        oc := OrderedCollection new.
        oc add: (Entity itemNamed: 'diary' description: '*It is open on the coordinates* There must be some truth in it.' takeable: false);
           add: (Entity itemNamed: 'radio' description: '*The radio has a frequency adjuster* Maybe I could run into something interesting by switching frequencies.' takeable: false);
           add: (Entity personNamed: 'clara' description: 'Clara pilots beside you, focused on the controls.').
        locations at: #'cockpit' put: oc.

        " Crash Site "
        oc := OrderedCollection new.
        oc add: (Entity itemNamed: 'plane' description: 'The plane''s a lost cause, but the luggage COMPARTMENT is intact. The supplies you took are probably still there.' takeable: false);
           add: (Entity itemNamed: 'compartment' description: 'The plane compartment for supplies.' takeable: false).
        locations at: #'crashsite' put: oc.

        " Compartment (will be populated with supplies from Act 1) "
        locations at: #'compartment' put: OrderedCollection new.

        " Cave "
        locations at: #'cave' put: OrderedCollection new.

        " Wreck "
        oc := OrderedCollection new.
        oc add: (Entity itemNamed: 'controls' description: 'Ancient but advanced control panels.' takeable: false);
           add: (Entity itemNamed: 'pistol' description: 'An old German Mauser C96 pistol. Still looks functional.' takeable: true).
        locations at: #'wreck' put: oc.

        " Tunnel "
        locations at: #'tunnel' put: OrderedCollection new.

        currentLocation := #'cockpit'
    ]

    canMoveFrom: fromLoc to: toLoc [
        (fromLoc = #'crashsite' and: [toLoc = #'cave']) ifTrue: [ ^ true ].
        (fromLoc = #'cave' and: [toLoc = #'crashsite']) ifTrue: [ ^ true ].
        (fromLoc = #'cave' and: [toLoc = #'wreck']) ifTrue: [ ^ true ].
        (fromLoc = #'wreck' and: [toLoc = #'cave']) ifTrue: [ ^ true ].
        (fromLoc = #'cave' and: [toLoc = #'tunnel']) ifTrue: [ ^ true ].
        (fromLoc = #'tunnel' and: [toLoc = #'cave']) ifTrue: [ ^ true ].
        ^ false
    ]

    parseLocation: aString [
        | s |
        s := aString asLowercase.
        s = 'cockpit'    ifTrue: [ ^ #'cockpit' ].
        s = 'crashsite'  ifTrue: [ ^ #'crashsite' ].
        s = 'cave'       ifTrue: [ ^ #'cave' ].
        s = 'wreck'      ifTrue: [ ^ #'wreck' ].
        s = 'tunnel'     ifTrue: [ ^ #'tunnel' ].
        s = 'compartment' ifTrue: [ ^ #'compartment' ].
        ^ #'unknown'
    ]

    descriptionOf: location [
        location = #'cockpit' ifTrue: [ ^ self cockpitDescription ].
        location = #'crashsite' ifTrue: [ ^ self crashSiteDescription ].
        location = #'cave' ifTrue: [ ^ self caveDescription ].
        location = #'wreck' ifTrue: [ ^ self wreckDescription ].
        location = #'tunnel' ifTrue: [ ^ self tunnelDescription ].
        location = #'compartment' ifTrue: [ ^ 'Inside the plane''s supply compartment.' ].
        ^ 'You see nothing special.'
    ]

    cockpitDescription [
        ^ 'The cockpit is tight and utilitarian, filled with glowing dials and humming switches.' , Character lf asString,
          'Through the windshield, Antarctica''s endless snow glitters under a gray sky.' , Character lf asString,
          'Turbulence occasionally rocks the plane, rattling the controls.' , Character lf asString,
          'Before the control panel, next to you, on the pilot''s seat is CLARA, bravely piloting the plane.' , Character lf asString,
          'On the panel sits the RADIO, and Byrd''s DIARY rests on your lap.'
    ]

    crashSiteDescription [
        (self hasTask: 'crash_site_described') ifFalse: [
            self addTask: 'crash_site_described'.
            ^ 'You wake amid the wreckage, cold seeping into your bones.' , Character lf asString,
              'The PLANE is a ruin, and CLARA lies injured nearby.' , Character lf asString,
              'Twisted metal juts from the snow, half-burying the fuselage; engine debris still smoulders.' , Character lf asString,
              'Wind howls, stinging your face with ice.' , Character lf asString,
              'CLARA slumps a few feet away, blood staining the snow beneath her head.'
        ].
        
        (self hasTask: 'injured_clara') ifTrue: [
            ^ 'The PLANE is a ruin, and CLARA lies injured nearby.' , Character lf asString,
              'Twisted metal juts from the snow, half-burying the fuselage; engine debris still smoulders.' , Character lf asString,
              'Wind howls, stinging your face with ice.' , Character lf asString,
              'CLARA slumps a few feet away, blood staining the snow beneath her head.'
        ].
        
        ^ 'The wreckage of your plane lies scattered across the frozen landscape.' , Character lf asString,
          'Clara stands nearby, looking shaken but determined.' , Character lf asString,
          'This place offers no shelter from the biting Antarctic cold.' , Character lf asString,
          'The crash site aligns with Byrd''s coordinates.'
    ]

    caveDescription [
        (self hasTask: 'entered_wreck') ifTrue: [
            ^ 'Slowly and carefully, you emerge from the wreckage.' , Character lf asString,
              'The dark cave corridor stretches before you.'
        ].
        
        (self hasTask: 'wreck_discovery') ifFalse: [
            ^ 'The cave twists downward, its walls polished and warm. A low hum vibrates the air.' , Character lf asString,
              'Smooth, spiralling walls funnel you DEEPER; faint lights pulse below.'
        ].
        
        ^ 'The entrance to the ice cave stretches before you.' , Character lf asString,
          'The unnatural smoothness of the walls suggests intelligent design.' , Character lf asString,
          'To the right, the Nazi flying-saucer WRECK remains embedded in the ice.' , Character lf asString,
          'A path leads DEEPER into the tunnel, where faint blue light pulses.' , Character lf asString,
          'A massive disk-shaped craft protrudes from the wall, its metallic surface scarred and dented-' , Character lf asString,
          'futuristic yet ancient.'
    ]

    wreckDescription [
        ^ 'The interior is cramped and dark, with control panels covered in dust and frost.' , Character lf asString,
          'Wires hang loosely, and a faint smell of oil lingers.' , Character lf asString,
          'On a seat, you spot an old German PISTOL-a Mauser C96-still holstered.'
    ]

    tunnelDescription [
        ^ 'The crash site lies in ruins, the plane''s twisted metal half-buried in snow.' , Character lf asString,
          'The wind howls mercilessly, and the sky above is a bleak, unforgiving gray.' , Character lf asString,
          'Your breath fogs in the frigid air, a stark contrast to the warmth of the hidden valley below.'
    ]

    getHint [
        (currentLocation = #'crashsite' and: [(self hasTask: 'crash_site_described') not]) ifTrue: [
            ^ 'I should first LOOK around to get my bearings.'
        ].
        
        (currentLocation = #'cockpit' and: [(self hasTask: 'radio_examined') not]) ifTrue: [
            ^ 'I need to find something to pass the time.'
        ].
        
        (currentLocation = #'cockpit' and: [self hasTask: 'radio_examined'] and: [(self hasTask: 'radio_used') not]) ifTrue: [
            ^ 'I could USE the radio to pass some time.'
        ].
        
        (currentLocation = #'cockpit' and: [self hasTask: 'radio_used'] and: [(self hasTask: 'crashed') not]) ifTrue: [
            ^ 'I should talk to Clara.'
        ].
        
        (currentLocation = #'crashsite' and: [self hasTask: 'injured_clara'] and: [(self hasTask: 'plane_examined') not]) ifTrue: [
            ^ 'Clara needs help fast, and the wreckage of the PLANE might have something useful.'
        ].
        
        (currentLocation = #'crashsite' and: [self hasTask: 'injured_clara'] and: [self hasTask: 'plane_examined']) ifTrue: [
            ^ 'Clara needs help fast, and a MEDKIT should be in the luggage COMPARTMENT.'
        ].
        
        (currentLocation = #'crashsite' and: [(self hasTask: 'injured_clara') not] and: [(self hasTask: 'compartment_checked') not]) ifTrue: [
            ^ 'I should check the luggage COMPARTMENT for the rest of the supplies.'
        ].
        
        (currentLocation = #'crashsite' and: [(self hasTask: 'injured_clara') not]) ifTrue: [
            ^ 'I should talk to Clara about our next move.'
        ].
        
        (currentLocation = #'cave' and: [self hasTask: 'wreck_discovery'] and: [(self hasTask: 'wreck_examined') not]) ifTrue: [
            ^ 'I should EXAMINE the WRECK.'
        ].
        
        (currentLocation = #'cave' and: [self hasTask: 'wreck_examined'] and: [(self hasTask: 'entered_wreck') not] and: [(self hasTask: 'wreck_discovery2') not]) ifTrue: [
            ^ 'I should talk to Clara.'
        ].
        
        (currentLocation = #'cave' and: [self hasTask: 'wreck_discovery2'] and: [(self hasTask: 'entered_wreck') not]) ifTrue: [
            ^ 'I must decide, should I GO to WRECK or GO DEEPER?'
        ].
        
        (currentLocation = #'cave') ifTrue: [ ^ 'I should talk to Clara.' ].
        
        (currentLocation = #'wreck' and: [(self hasItem: 'pistol') not]) ifTrue: [
            ^ 'I should look around for anything useful.'
        ].
        
        (currentLocation = #'wreck') ifTrue: [ ^ 'I should talk to Clara.' ].
        
        ^ 'I should try to LOOK around to get my bearings.'
    ]

    examineSpecial: key [
        (key = 'diary' and: [currentLocation = #'cockpit']) ifTrue: [
            self markExamined: 'diary'.
            self printLines: {
                '*It is open on the coordinates*'.
                'There must be some truth in it.'.
                ''
            }.
            ^ true
        ].
        
        (key = 'radio' and: [currentLocation = #'cockpit']) ifTrue: [
            self markExamined: 'radio'.
            self addTask: 'radio_examined'.
            self printLines: {
                '*The radio has a frequency adjuster*'.
                'Maybe I could run into something interesting by switching frequencies.'.
                ''
            }.
            ^ true
        ].
        
        (key = 'clara' and: [currentLocation = #'cockpit'] and: [(self hasTask: 'crashed') not]) ifTrue: [
            self printLines: {
                'Clara pilots beside you, focused on the controls.'.
                ''
            }.
            ^ true
        ].
        
        (key = 'clara' and: [currentLocation = #'crashsite'] and: [self hasTask: 'crashed']) ifTrue: [
            self printLines: {
                'She''s unconscious, her forehead gashed, her breathing shallow.'.
                'Blood soaks the snow around her leg.'.
                ''
            }.
            ^ true
        ].
        
        (key = 'plane' and: [currentLocation = #'crashsite']) ifTrue: [
            self markExamined: 'plane'.
            self addTask: 'plane_examined'.
            self printLines: {
                'The plane''s a lost cause, but the luggage COMPARTMENT is intact.'.
                'The supplies you took are probably still there.'.
                ''
            }.
            ^ true
        ].
        
        (key = 'compartment' and: [currentLocation = #'crashsite']) ifTrue: [
            ^ self handleCompartmentExamine
        ].
        
        (key = 'wreck' and: [currentLocation = #'cave']) ifTrue: [
            self markExamined: 'wreck'.
            self addTask: 'wreck_examined'.
            self printLines: {
                'A disk-shaped craft protrudes from the ice, marked with a Nazi Balkenkreuz,'.
                '"Hergestellt in Deutschland. 1944. Danzig". Machine-gun nests bristle from'.
                'its surface.'.
                ''.
                'Clara: "Made in Germany. 1944. Danzig. I think that''s Nazi tech-what''s it doing here?"'.
                ''.
                'There appears to be an entrance. You could GO inside to investigate further.'.
                ''
            }.
            ^ true
        ].
        
        (key = 'pistol' and: [currentLocation = #'wreck']) ifTrue: [
            self markExamined: 'pistol'.
            self printLines: {
                'An old German Mauser C96 pistol. Still looks functional.'.
                ''
            }.
            ^ true
        ].
        
        ^ nil
    ]

    handleCompartmentExamine [
        | supplies hasMedkit |
        self markExamined: 'compartment'.
        self addTask: 'compartment_checked'.
        
        supplies := self entitiesAt: #'compartment'.
        hasMedkit := supplies anySatisfy: [:e | e name = 'medkit'].
        
        (self hasTask: 'injured_clara' and: [hasMedkit not]) ifTrue: [
            self addTask: 'game_over'.
            self addTask: 'act_finished'.
            self printLines: {
                'You check the plane compartment for supplies.'.
                'You realize you forgot to take the MEDKIT before departure.'.
                'You: "How could I forget it? What can I do now?"'.
                'You start to panic, gasping heavily.'.
                ''.
                'You: "Clara, Clara, wake up!"'.
                'You try to rouse Clara, but it''s futile.'.
                'You: (crying) "Clara, please, I can''t do this alone."'.
                ''.
                'With her wound untreated, Clara continues to bleed.'.
                'Suddenly, her heart stops beating.'.
                'As the cold overwhelms you, the Antarctic claims you both.'.
                'GAME OVER'.
                ''
            }.
            ^ true
        ].
        
        self printLines: ({ 'You check the plane compartment for supplies.' . 'Inside you find:' } , 
                          (supplies collect: [:e | '- ' , e name]) ,
                          { '' }).
        ^ true
    ]

    handleTalk: whoName [
        whoName asLowercase = 'clara' ifTrue: [
            ^ self dialogWithClara
        ].
        
        self printLines: { 'There''s no one here to talk to by that name.' . '' }
    ]

    dialogWithClara [
        (currentLocation = #'cockpit' and: [self hasTask: 'radio_used'] and: [(self hasTask: 'crashed') not]) ifTrue: [
            self addTask: 'crashed'.
            self addTask: 'injured_clara'.
            currentLocation := #'crashsite'.
            self printLines: {
                'You: "Is everything okay?"'.
                'Clara: "I don''t know; the compass and the altimeter suddenly started going crazy,'.
                'but we''re close to our destination, so it shouldn''t be a probl-"'.
                ''.
                '*Suddenly, turbulence slams the plane. Lights flicker, instruments fail, the engines choke.*'.
                'Clara (shouting): "Brace yourself! Everything''s shutting down!"'.
                'You (screaming): "Ahh, what''s happening!?"'.
                '*The plane spirals down, crashing into the ice. Darkness falls.*'.
                ''
            }.
            ^ nil
        ].
        
        (currentLocation = #'cockpit' and: [(self hasTask: 'crashed') not] and: [(self hasTask: 'cockpit_intro_done') not]) ifTrue: [
            self addTask: 'awaiting_cockpit_choice'.
            self printLines: {
                'Clara glances over: "So, doc, what''s your take? Are we on a wild goose chase,'.
                'or is there really something out here in this frozen wasteland?"'.
                'Your choices:'.
                '1. "Byrd''s diary points to 70S, 10E. The coordinates are too specific to be nothing."'.
                '2. "I don''t know, but the weather''s turning ugly. We need to stay sharp."'.
                '3. "Whether it''s real or not, the mission''s worth it for the discovery alone."'.
                ''
            }.
            ^ self waitForChoice: #cockpit
        ].
        
        (currentLocation = #'cockpit' and: [(self hasTask: 'crashed') not]) ifTrue: [
            self printLines: {
                'You: "We''re bound to find something there - I can feel it in my bones."'.
                'Clara: "Hopefully, or all our efforts will be for nothing."'
            }.
            ^ nil
        ].
        
        (currentLocation = #'crashsite' and: [self hasTask: 'injured_clara'] and: [(self hasItem: 'medkit') not]) ifTrue: [
            self printLines: { 'She''s unconscious and needs medical attention urgently.' . '' }.
            ^ nil
        ].
        
        (currentLocation = #'crashsite' and: [(self hasTask: 'injured_clara') not] and: [(self hasTask: 'cave_advice') not]) ifTrue: [
            self addTask: 'cave_advice'.
            self printLines: {
                'Clara: "We can''t stay exposed out here. That CAVE might be our only shot,'.
                'but it''s giving me a bad feeling. We must GO now, before it gets dark."'.
                ''
            }.
            ^ nil
        ].
        
        (currentLocation = #'cave' and: [(self hasTask: 'wreck_discovery') not]) ifTrue: [
            self addTask: 'wreck_discovery'.
            self printLines: {
                'Clara: "Hey, what''s that? Do you see it?"'.
                ''.
                'On the right side of the tunnel you see a disk-shaped WRECK –'.
                'a massive saucer-like craft embedded in the ice, its metallic surface scarred and dented.'.
                'It looks futuristic yet ancient.'
            }.
            ^ nil
        ].
        
        (currentLocation = #'cave' and: [self hasTask: 'wreck_examined'] and: [(self hasTask: 'entered_wreck') not] and: [(self hasTask: 'wreck_discovery2') not]) ifTrue: [
            self addTask: 'wreck_discovery2'.
            self printLines: {
                'You: "This is incredible. A Nazi flying saucer?"'.
                'Clara: "Looks like it. But how did it get here? And why?"'.
                'You: "Maybe they were experimenting with advanced technology in Antarctica."'.
                'Clara: "Or maybe they found something here. Either way, it''s creepy."'.
                ''.
                'Clara: "Do you think we should try to get INSIDE it or don''t risk and GO DEEPER?"'
            }.
            ^ nil
        ].
        
        (currentLocation = #'wreck') ifTrue: [
            self printLines: {
                'Clara: "This place gives me the creeps. Look at these controls - they''re way ahead of their time."'.
                'You: "Yeah, it''s like something out of science-fiction. But it''s real."'.
                'Clara: "I think we''ve seen enough. Let''s keep moving; there might be more ahead. We should GO DEEPER."'.
                ''
            }.
            ^ nil
        ].
        
        self printLines: { 'There''s no one here to talk to.' . '' }
    ]

    handleSpecialCommand: aCmdArray [
        | cmd |
        cmd := aCmdArray first.
        
        " Handle choice inputs "
        (self hasTask: 'awaiting_cockpit_choice') ifTrue: [
            ^ self handleChoiceInput: aCmdArray context: #cockpit
        ].
        
        (self hasTask: 'awaiting_radio_choice') ifTrue: [
            ^ self handleChoiceInput: aCmdArray context: #radio
        ].
        
        (self hasTask: 'awaiting_radio_background') ifTrue: [
            ^ self handleChoiceInput: aCmdArray context: #radio_background
        ].
        
        " Handle GO DEEPER command "
        (cmd = #go and: [aCmdArray size > 1] and: [aCmdArray second = 'deeper']) ifTrue: [
            ^ self handleGoDeeper
        ].
        
        ^ nil
    ]

    handleChoiceInput: aCmdArray context: context [
        | input choice maxChoice |
        input := aCmdArray first asString.
        choice := input asInteger.
        maxChoice := context = #radio ifTrue: [2] ifFalse: [3].
        
        (choice between: 1 and: maxChoice) ifTrue: [
            ^ self processChoice: choice context: context
        ].
        self printLine: 'Invalid choice - enter 1-' , maxChoice asString , '.'.
        ^ nil
    ]

    processChoice: choice context: context [
        context = #cockpit ifTrue: [ ^ self processCockpitChoice: choice ].
        context = #radio ifTrue: [ ^ self processRadioChoice: choice ].
        context = #radio_background ifTrue: [ ^ self processRadioBackgroundChoice: choice ].
        ^ self printLine: 'Invalid choice context.'
    ]

    processCockpitChoice: choice [
        self removeTask: 'awaiting_cockpit_choice'.
        self addTask: 'cockpit_intro_done'.
        
        choice = 1 ifTrue: [
            self printLines: {
                'You: "Byrd''s diary points to 70S, 10E. The coordinates are too specific to be nothing."'.
                'Clara: "Specific or not, Antarctica''s a maze. Let''s hope those coordinates don''t lead us straight into trouble."'
            }
        ].
        
        choice = 2 ifTrue: [
            self printLines: {
                'You: "I don''t know, but the weather''s turning ugly. We need to stay sharp."'.
                'Clara: "Yeah, I feel it too. This storm''s got teeth. Keep your eyes peeled."'.
                ''
            }
        ].
        
        choice = 3 ifTrue: [
            self printLines: {
                'You: "Whether it''s real or not, the mission''s worth it for the discovery alone."'.
                'Clara: "Discovery''s great until the ice swallows us whole. Still, I like your optimism."'
            }
        ]
    ]

    processRadioChoice: choice [
        choice = 1 ifTrue: [
            self removeTask: 'awaiting_radio_choice'.
            self addTask: 'radio_used'.
            self printLines: {
                'You: "Nah, you''re freaking out; that''s just some usual anomalies. Focus on piloting."'.
                'Clara: "Yeah, you''re right; there''s no time for that."'
            }
        ].
        
        choice = 2 ifTrue: [
            self removeTask: 'awaiting_radio_choice'.
            self addTask: 'awaiting_radio_background'.
            self printLines: {
                'You: "Oh, you know German? I should have guessed from your surname."'.
                'Clara: "Yes, my father was a German immigrant. He went to the USA when WWI started."'.
                'Your choices:'.
                '1. "Byrd''s diary doesn''t mention Germans, but hey, we''re in what Nazi Germany claimed as their territory in Antarctica."'.
                '2. "Ah, Uncle Sam, a shelter for all the world''s people in need."'.
                ''
            }.
            ^ self waitForChoice: #radio_background
        ]
    ]

    processRadioBackgroundChoice: choice [
        self removeTask: 'awaiting_radio_background'.
        self addTask: 'radio_used'.
        
        choice = 1 ifTrue: [
            self printLines: {
                'You: "Byrd''s diary doesn''t mention Germans, but hey, we''re in what Nazi Germany claimed as their territory in Antarctica."'.
                'Clara: "I''m sure the last thing we want is for my German to come in handy. This whole mission feels unreal and ridiculous."'
            }
        ].
        
        choice = 2 ifTrue: [
            self printLines: {
                'You: "Ah, Uncle Sam, a shelter for all the world''s people in need."'.
                'Clara: "Until he sends you on a mission like this, haha."'.
                ''
            }
        ]
    ]

    handleGoDeeper [
        (currentLocation = #'cave' and: [(self hasTask: 'wreck_discovery') not]) ifTrue: [
            self addTask: 'wreck_discovery'.
            self printLines: { 'Clara: "Hey, what''s that? Do you see it?"' . '' }.
            ^ nil
        ].
        
        ((currentLocation = #'cave' or: [currentLocation = #'wreck']) and: [(self hasTask: 'wreck_examined') not] and: [(self hasTask: 'entered_wreck') not]) ifTrue: [
            self printLines: {
                'Clara: "Hold up, doc. We can''t ignore this-it''s too weird."'.
                'You should EXAMINE the WRECK.'.
                ''
            }.
            ^ nil
        ].
        
        (currentLocation = #'cave' or: [currentLocation = #'wreck']) ifTrue: [
            (currentLocation = #'wreck') ifTrue: [ currentLocation := #'cave' ].
            currentLocation := #'tunnel'.
            self addTask: 'entered_cave'.
            self addTask: 'act_finished'.
            self printLines: {
                'Slowly and carefully, you emerge from the wreckage.'.
                'The dark cave corridor stretches before you.'.
                'As you descend deeper into the tunnel, a roar shakes the walls as a'.
                'bat-winged aircraft rockets past, vanishing toward the outside world.'.
                ''.
                'Clara: "That''s Nazi design-straight out of the war!"'.
                'You: "I''m freaking out; let''s get out of here. I think I see light ahead."'.
                ''.
                'A steady glow blooms from the tunnel''s depths, pulling you forward.'.
                ''.
                '----------------------------ACT 2 OVER----------------------------'.
                ''.
                'Type "next" to continue, or "quit" to leave.'.
                ''
            }.
            ^ nil
        ].
        
        self printLines: { 'You can''t go deeper from here.' . '' }.
        ^ nil
    ]

    handleTake: objName [
        | name obj supplies |
        name := objName asLowercase.
        
        (currentLocation = #'crashsite' and: [self hasTask: 'injured_clara'] and: [name ~= 'medkit']) ifTrue: [
            ^ self printLines: { 'I need to help Clara first.' . '' }
        ].
        
        (currentLocation = #'crashsite' and: [(self hasTask: 'plane_examined') not]) ifTrue: [
            ^ self printLines: { 'I don''t know where the supplies are.' . '' }
        ].
        
        (currentLocation = #'crashsite' and: [(self hasTask: 'compartment_checked') not]) ifTrue: [
            ^ self printLines: { 'I should check the luggage COMPARTMENT.' . '' }
        ].
        
        obj := self findHere: name.
        obj ifNil: [
            " Check compartment "
            supplies := self entitiesAt: #'compartment'.
            obj := supplies detect: [:e | e name = name] ifNone: [ nil ].
        ].
        
        obj ifNil: [ ^ self printLines: { 'I don''t see ' , objName , ' here.' . '' } ].
        
        (self isInInventory: name) ifTrue: [ 
            ^ self printLines: { 'You''re already holding it!' . '' } 
        ].
        
        name = 'pistol' ifTrue: [
            self addToInventory: obj.
            self removeFromLocation: obj.
            ^ self printLines: { 'You take the PISTOL - hopefully it won''t be needed.' . '' }
        ].
        
        name = 'medkit' ifTrue: [
            self addToInventory: obj.
            " Remove from compartment if that's where it was "
            supplies := self entitiesAt: #'compartment'.
            supplies remove: obj ifAbsent: [].
            ^ self printLines: { 'Thank God I took it.' . '' }
        ].
        
        (self isSupply: name) ifTrue: [
            (self countSupplies >= 5) ifTrue: [
                ^ self printLines: { 'You cannot take this - you''ve reached the limit (5 items).' . '' }
            ].
            self addToInventory: obj.
            supplies := self entitiesAt: #'compartment'.
            supplies remove: obj ifAbsent: [].
            ^ self printLines: { 'You take the ' , obj name , '.' . '' }
        ].
        
        self printLines: { 'You can''t take that.' . '' }
    ]

    handleDrop: objName [
        | name obj |
        name := objName asLowercase.
        obj := inventory detect: [:e | e name = name] ifNone: [ nil ].
        obj ifNil: [ ^ self printLines: { 'You aren''t holding that!' . '' } ].
        
        self removeFromInventory: obj.
        (self isSupply: name) 
            ifTrue: [ self printLines: { 'You drop the ' , obj name , '.' . '' } ]
            ifFalse: [ self printLines: { 'OK.' . '' } ]
    ]

    handleUse: objName [
        | name |
        name := objName asLowercase.
        
        (name = 'radio' and: [currentLocation = #'cockpit'] and: [(self hasTask: 'radio_used') not]) ifTrue: [
            self addTask: 'awaiting_radio_choice'.
            self printLines: {
                'You playfully switch frequencies.'.
                'Clara: "What is it, doc? Are you bored?"'.
                'You: "Kind of."'.
                ''.
                'After a while, you run into something. The radio spits static until a garbled voice breaks through.'.
                ''.
                'Clara: "Wait, what? I think I hear German, but the audio is too distorted; I can''t make out the words."'.
                'Your choices:'.
                '1. "Nah, you''re freaking out; that''s just some usual anomalies. Focus on piloting."'.
                '2. "Oh, you know German? I should have guessed from your surname."'.
                ''
            }.
            ^ self waitForChoice: #radio
        ].
        
        (name = 'radio' and: [self hasItem: 'radio'] and: [(self hasTask: 'entered_cave') not] and: [(self hasTask: 'injured_clara') not]) ifTrue: [
            ^ self printLines: {
                'No signal to base, but switching channels catches German again:'.
                '"Herr [distortion], wann ist die Glocke fertig? Ich denke, wir [distortion] die Arbeit nachste Woche been..."'.
                'can be heard between the static noise.'.
                ''
            }
        ].
        
        (name = 'medkit' and: [currentLocation = #'crashsite'] and: [self hasTask: 'injured_clara']) ifTrue: [
            self removeTask: 'injured_clara'.
            self removeFromInventory: (self findEntity: 'medkit').
            ^ self printLines: {
                'You bandage Clara''s wounds; she stirs awake.'.
                'Clara (mumbling): "...what happened? Where are we?"'.
                'You: "Thank God, you''re alive. We crashed, and you''re injured, but I think you''ll be okay."'.
                'Clara: "Thanks, doc... I thought I was a goner."'.
                ''
            }
        ].
        
        (self isInInventory: name) 
            ifTrue: [ self printLines: { 'I can''t use that right now.' . '' } ]
            ifFalse: [ self printLines: { 'I don''t have it or I can''t use it.' . '' } ]
    ]

    " Override to inject supplies from Act 1 "
    loadPlayerState: aPlayerState [
        | supplies |
        super loadPlayerState: aPlayerState.
        " Move all supplies to compartment "
        supplies := inventory select: [:e | self isSupply: e name].
        supplies do: [:supply | 
            inventory remove: supply.
            (self entitiesAt: #'compartment') add: supply
        ]
    ]

    printActProlog [
        self printLines: {
            'ACT 2: DESCENT INTO THE UNKNOWN'.
            'You are sitting in the co-pilot chair of a Douglas A-20 Havoc, soaring over'.
            'Antarctica''s icy expanse. Clara pilots beside you, and you hold Admiral Byrd''s diary.'.
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

Object subclass: MainGame [

    MainGame class >> run [
        ^ self new run
    ]

    run [
        self printIntroduction.
        self getInitialCommand
    ]

    printIntroduction [
        Transcript show: 'INTRODUCTION:'; cr.
        Transcript show: 'Welcome to "The Hidden Realm", an interactive fiction adventure'; cr.
        Transcript show: 'set in the icy depths of Antarctica. Unravel ancient secrets,'; cr.
        Transcript show: 'face modern dangers, and shape your fate through your choices.'; cr.
        Transcript show: 'Don''t be afraid to use HINTS - they will help you navigate'; cr.
        Transcript show: 'challenges and uncover the truth. Stay sharp - the unknown awaits.'; cr.
        Transcript cr.
        Transcript show: 'Type "start" to play from Act 1, or "act2" / "act3" to start from later acts.'; cr.
        Transcript cr
    ]

    getInitialCommand [
        | input args cmd |
        Transcript show: '> '.
        input := stdin nextLine.
        args := input asLowercase subStrings: ' '.
        cmd := args isEmpty ifTrue: [#unknown] ifFalse: [args first asSymbol].

        cmd = #'act2' ifTrue: [
            ^ self runFromAct2
        ].
        
        cmd = #'act3' ifTrue: [
            ^ self runFromAct3
        ].
        
        (cmd = #'start' or: [cmd = #'next']) ifTrue: [
            ^ self runFromAct1
        ].
        
        cmd = #'quit' ifTrue: [
            ^ self
        ].
        
        Transcript show: 'Unknown command. Please type ''start'' to start the game or ''act2''/''act3'' to skip to a specific act.'; cr.
        Transcript cr.
        self getInitialCommand
    ]

    runFromAct1 [
        | act1 playerState act2 act3 |
        act1 := Act1Game new.
        playerState := act1 mainLoop.
        playerState ifNil: [ ^ self ].
        
        act2 := Act2Game new.
        act2 loadPlayerState: playerState.
        playerState := act2 mainLoop.
        playerState ifNil: [ ^ self ].
        
        act3 := Act3Game new.
        act3 loadPlayerState: playerState.
        act3 mainLoop.
        ^ self
    ]

    runFromAct2 [
        | supplies playerState act2 act3 |
        supplies := self getSuppliesSelectionAct2.
        playerState := PlayerState new.
        supplies do: [:supply | playerState addToInventory: supply].
        
        act2 := Act2Game new.
        act2 loadPlayerState: playerState.
        playerState := act2 mainLoop.
        playerState ifNil: [ ^ self ].
        
        act3 := Act3Game new.
        act3 loadPlayerState: playerState.
        act3 mainLoop.
        ^ self
    ]

    runFromAct3 [
        | supplies playerState act3 |
        supplies := self getSuppliesSelectionAct3.
        playerState := PlayerState new.
        supplies do: [:supply | playerState addToInventory: supply].
        
        act3 := Act3Game new.
        act3 loadPlayerState: playerState.
        act3 mainLoop.
        ^ self
    ]

    getSuppliesSelectionAct2 [
        | supplyList mapper |
        supplyList := {
            'You need to select supplies for your journey. The plane has capacity for 5 items:'.
            '1. FOOD rations'.
            '2. WATER'.
            '3. GEIGER Counter'.
            '4. MEDKIT'.
            '5. RADIO'.
            '6. Climbing GEAR'.
            '7. Navigation TOOLS'.
            'Enter the numbers of 5 items you want to take (e.g., ''1 2 3 4 5''):'.
            ''
        }.
        
        mapper := [:selection |
            selection = 1 ifTrue: [ Entity itemNamed: 'food' description: 'Canned goods and dried meals.' takeable: true ].
            selection = 2 ifTrue: [ Entity itemNamed: 'water' description: 'Fresh water in sealed containers.' takeable: true ].
            selection = 3 ifTrue: [ Entity itemNamed: 'geiger' description: 'A standard radiation detector.' takeable: true ].
            selection = 4 ifTrue: [ Entity itemNamed: 'medkit' description: 'Bandages, antiseptic, morphine…' takeable: true ].
            selection = 5 ifTrue: [ Entity itemNamed: 'radio' description: 'A shortwave field radio.' takeable: true ].
            selection = 6 ifTrue: [ Entity itemNamed: 'gear' description: 'Ropes, pitons, carabiners.' takeable: true ].
            selection = 7 ifTrue: [ Entity itemNamed: 'tools' description: 'A compass, maps, and a sextant.' takeable: true ].
            Entity itemNamed: 'food' description: 'Canned goods and dried meals.' takeable: true  " Default "
        ].
        
        ^ self getSuppliesSelection: supplyList mapper: mapper
    ]

    getSuppliesSelectionAct3 [
        | supplyList mapper |
        supplyList := {
            'You need to select supplies for your journey. The plane has capacity for 5 items:'.
            '1. FOOD rations'.
            '2. WATER'.
            '3. GEIGER Counter'.
            '4. RADIO'.
            '5. Climbing GEAR'.
            '6. Navigation TOOLS'.
            '7. PISTOL'.
            'Enter the numbers of 5 items you want to take (e.g., ''1 2 3 4 5''):'.
            ''
        }.
        
        mapper := [:selection |
            selection = 1 ifTrue: [ Entity itemNamed: 'food' description: 'Canned goods and dried meals.' takeable: true ].
            selection = 2 ifTrue: [ Entity itemNamed: 'water' description: 'Fresh water in sealed containers.' takeable: true ].
            selection = 3 ifTrue: [ Entity itemNamed: 'geiger' description: 'A standard radiation detector.' takeable: true ].
            selection = 4 ifTrue: [ Entity itemNamed: 'radio' description: 'A shortwave field radio.' takeable: true ].
            selection = 5 ifTrue: [ Entity itemNamed: 'gear' description: 'Ropes, pitons, carabiners.' takeable: true ].
            selection = 6 ifTrue: [ Entity itemNamed: 'tools' description: 'A compass, maps, and a sextant.' takeable: true ].
            selection = 7 ifTrue: [ Entity itemNamed: 'pistol' description: 'An old German Mauser C96 pistol.' takeable: true ].
            Entity itemNamed: 'food' description: 'Canned goods and dried meals.' takeable: true  " Default "
        ].
        
        ^ self getSuppliesSelection: supplyList mapper: mapper
    ]

    getSuppliesSelection: supplyList mapper: mapperBlock [
        | input selections validSelections supplies |
        
        supplyList do: [:line | Transcript show: line; cr].
        Transcript show: '> '.
        input := stdin nextLine.
        
        selections := (input subStrings: ' ') collect: [:s | s asInteger].
        validSelections := selections select: [:n | n between: 1 and: 7].
        
        validSelections size < 5 ifTrue: [
            Transcript show: 'Please select exactly 5 valid items (numbers 1-7).'; cr.
            ^ self getSuppliesSelection: supplyList mapper: mapperBlock
        ].
        
        supplies := (validSelections first: 5) collect: mapperBlock.
        ^ supplies
    ]
] !

" Start the game "
MainGame run.
ObjectMemory quit !