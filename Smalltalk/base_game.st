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