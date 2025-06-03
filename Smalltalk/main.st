"Main game controller for The Hidden Realm"

Object subclass: #GameMain
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    category: 'Game-Main'!

!GameMain class methodsFor: 'game content'!
introductionText
    ^#(
        'INTRODUCTION:'
        'Welcome to "The Hidden Realm", an interactive fiction adventure'
        'set in the icy depths of Antarctica. Unravel ancient secrets,'
        'face modern dangers, and shape your fate through your choices.'
        'Don''t be afraid to use HINTS - they will help you navigate'
        'challenges and uncover the truth. Stay sharp - the unknown awaits.'
        ''
        'Type "start" to play from Act 1, or "act2" / "act3" to start from later acts.'
        ''
    )!

act2SuppliesList
    ^#(
        'You need to select supplies for your journey. The plane has capacity for 5 items:'
        '1. FOOD rations'
        '2. WATER'
        '3. GEIGER Counter'
        '4. MEDKIT'
        '5. RADIO'
        '6. Climbing GEAR'
        '7. Navigation TOOLS'
        'Enter the numbers of 5 items you want to take (e.g., ''1 2 3 4 5''):'
        ''
    )!

act3SuppliesList
    ^#(
        'You need to select supplies for your journey. The plane has capacity for 5 items:'
        '1. FOOD rations'
        '2. WATER'
        '3. GEIGER Counter'
        '4. RADIO'
        '5. Climbing GEAR'
        '6. Navigation TOOLS'
        '7. PISTOL'
        'Enter the numbers of 5 items you want to take (e.g., ''1 2 3 4 5''):'
        ''
    )!

!GameMain class methodsFor: 'supply selection'!
mapSelectionsToEntitiesAct2: selections
    ^selections collect: [:selection |
        selection = 1 ifTrue: [Entity type: EntityType item name: 'food' description: 'Canned goods and dried meals.' takeable: true].
        selection = 2 ifTrue: [Entity type: EntityType item name: 'water' description: 'Fresh water in sealed containers.' takeable: true].
        selection = 3 ifTrue: [Entity type: EntityType item name: 'geiger' description: 'A standard radiation detector.' takeable: true].
        selection = 4 ifTrue: [Entity type: EntityType item name: 'medkit' description: 'Bandages, antiseptic, morphine…' takeable: true].
        selection = 5 ifTrue: [Entity type: EntityType item name: 'radio' description: 'A shortwave field radio.' takeable: true].
        selection = 6 ifTrue: [Entity type: EntityType item name: 'gear' description: 'Ropes, pitons, carabiners.' takeable: true].
        selection = 7 ifTrue: [Entity type: EntityType item name: 'tools' description: 'A compass, maps, and a sextant.' takeable: true].
        "Default to food if invalid selection"
        Entity type: EntityType item name: 'food' description: 'Canned goods and dried meals.' takeable: true]!

mapSelectionsToEntitiesAct3: selections
    ^selections collect: [:selection |
        selection = 1 ifTrue: [Entity type: EntityType item name: 'food' description: 'Canned goods and dried meals.' takeable: true].
        selection = 2 ifTrue: [Entity type: EntityType item name: 'water' description: 'Fresh water in sealed containers.' takeable: true].
        selection = 3 ifTrue: [Entity type: EntityType item name: 'geiger' description: 'A standard radiation detector.' takeable: true].
        selection = 4 ifTrue: [Entity type: EntityType item name: 'radio' description: 'A shortwave field radio.' takeable: true].
        selection = 5 ifTrue: [Entity type: EntityType item name: 'gear' description: 'Ropes, pitons, carabiners.' takeable: true].
        selection = 6 ifTrue: [Entity type: EntityType item name: 'tools' description: 'A compass, maps, and a sextant.' takeable: true].
        selection = 7 ifTrue: [Entity type: EntityType item name: 'pistol' description: 'An old German Mauser C96 pistol.' takeable: true].
        "Default to food if invalid selection"
        Entity type: EntityType item name: 'food' description: 'Canned goods and dried meals.' takeable: true]!

getSuppliesSelection: supplyList mapperBlock: mapperBlock
    | input selections validSelections |

    [true] whileTrue: [
        GameUtils printLines: supplyList.
        GameUtils printPrompt.
        input := GameUtils readCommand.

        selections := (input substrings) collect: [:str |
            [str asInteger] on: Error do: [0]].

        validSelections := (selections select: [:n | n >= 1 and: [n <= 7]]) first: 5.

        validSelections size = 5 ifTrue: [
            ^mapperBlock value: validSelections].

        GameUtils printLines: #('Please select exactly 5 valid items (numbers 1-7).')].!

!GameMain class methodsFor: 'main game flow'!
main
    GameUtils printLines: self introductionText.
    self getInitialCommand!

getInitialCommand
    | line command |

    [true] whileTrue: [
        GameUtils printPrompt.
        line := GameUtils readCommand.
        command := GameUtils parseCommand: line.

        command isAct2 ifTrue: [
            | supplies playerState ps2 ps3 |
            supplies := self getSuppliesSelection: self act2SuppliesList
                mapperBlock: [:sels | self mapSelectionsToEntitiesAct2: sels].
            playerState := PlayerState new inventory: supplies.
            ps2 := Act2 start: playerState.
            ps3 := Act3 start: ps2.
            ^self].

        command isAct3 ifTrue: [
            | supplies playerState ps3 |
            supplies := self getSuppliesSelection: self act3SuppliesList
                mapperBlock: [:sels | self mapSelectionsToEntitiesAct3: sels].
            playerState := PlayerState new inventory: supplies.
            ps3 := Act3 start: playerState.
            ^self].

        command isNext ifTrue: [
            | ps1 ps2 ps3 |
            ps1 := Act1 start: PlayerState empty.
            ps2 := Act2 start: ps1.
            ps3 := Act3 start: ps2.
            ^self].

        command isQuit ifTrue: [
            ^self].

        GameUtils printLines: #('Unknown command. Please type "next" to start the game or "act2"/"act3" to skip to a specific act.' '').
    ]!

!GameMain class methodsFor: 'class initialization'!
startGame
    "Entry point to start the game"
    self main! !
