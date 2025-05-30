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