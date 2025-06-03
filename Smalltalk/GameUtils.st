"Common classes and utilities for The Hidden Realm adventure game"

Object subclass: #EntityType
    instanceVariableNames: ''
    classVariableNames: 'Item Person'
    poolDictionaries: ''
    category: 'Game-Core'!

!EntityType class methodsFor: 'class initialization'!
initialize
    Item := #Item.
    Person := #Person.! !

!EntityType class methodsFor: 'constants'!
item
    ^Item!

person
    ^Person! !

Object subclass: #Location
    instanceVariableNames: ''
    classVariableNames: 'Barrack Yard Runway Depot Tent Cockpit CrashSite Cave Wreck Tunnel Compartment Ledge Tree Ruins City Rock Unknown'
    poolDictionaries: ''
    category: 'Game-Core'!

!Location class methodsFor: 'class initialization'!
initialize
    Barrack := #Barrack.
    Yard := #Yard.
    Runway := #Runway.
    Depot := #Depot.
    Tent := #Tent.
    Cockpit := #Cockpit.
    CrashSite := #CrashSite.
    Cave := #Cave.
    Wreck := #Wreck.
    Tunnel := #Tunnel.
    Compartment := #Compartment.
    Ledge := #Ledge.
    Tree := #Tree.
    Ruins := #Ruins.
    City := #City.
    Rock := #Rock.
    Unknown := #Unknown.! !

!Location class methodsFor: 'constants'!
barrack
    ^Barrack!

yard
    ^Yard!

runway
    ^Runway!

depot
    ^Depot!

tent
    ^Tent!

cockpit
    ^Cockpit!

crashSite
    ^CrashSite!

cave
    ^Cave!

wreck
    ^Wreck!

tunnel
    ^Tunnel!

compartment
    ^Compartment!

ledge
    ^Ledge!

tree
    ^Tree!

ruins
    ^Ruins!

city
    ^City!

rock
    ^Rock!

unknown
    ^Unknown! !

Object subclass: #Entity
    instanceVariableNames: 'entityType entityName entityDescription takeableByDefault'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'Game-Core'!

!Entity methodsFor: 'initialization'!
initializeType: aType name: aName description: aDescription takeable: aTakeable
    entityType := aType.
    entityName := aName.
    entityDescription := aDescription.
    takeableByDefault := aTakeable.!

!Entity class methodsFor: 'instance creation'!
type: aType name: aName description: aDescription takeable: aTakeable
    ^self new initializeType: aType name: aName description: aDescription takeable: aTakeable!

!Entity methodsFor: 'accessing'!
entityType
    ^entityType!

entityName
    ^entityName!

entityDescription
    ^entityDescription!

takeableByDefault
    ^takeableByDefault!

!Entity methodsFor: 'comparing'!
= anEntity
    ^anEntity class = self class and: [
        entityName = anEntity entityName]!

hash
    ^entityName hash! !

Object subclass: #GameState
    instanceVariableNames: 'currentLocation locationEntities inventory examined talked tasks'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'Game-Core'!

!GameState methodsFor: 'initialization'!
initialize
    super initialize.
    currentLocation := Location unknown.
    locationEntities := Dictionary new.
    inventory := OrderedCollection new.
    examined := Set new.
    talked := Set new.
    tasks := Set new.!

!GameState methodsFor: 'accessing'!
currentLocation
    ^currentLocation!

currentLocation: aLocation
    currentLocation := aLocation!

locationEntities
    ^locationEntities!

locationEntities: aDictionary
    locationEntities := aDictionary!

inventory
    ^inventory!

inventory: aCollection
    inventory := aCollection!

examined
    ^examined!

examined: aSet
    examined := aSet!

talked
    ^talked!

talked: aSet
    talked := aSet!

tasks
    ^tasks!

tasks: aSet
    tasks := aSet!

!GameState methodsFor: 'entity management'!
entitiesAt: aLocation
    ^locationEntities at: aLocation ifAbsent: [OrderedCollection new]!

findHere: entityName
    | lowerName |
    lowerName := entityName asLowercase.
    ^(self entitiesAt: currentLocation)
        detect: [:entity | entity entityName asLowercase = lowerName]
        ifNone: [nil]!

findEntity: entityName
    | lowerName allEntities |
    lowerName := entityName asLowercase.
    allEntities := (self entitiesAt: currentLocation), inventory.
    ^allEntities
        detect: [:entity | entity entityName asLowercase = lowerName]
        ifNone: [nil]!



isInInventory: entityName
    | lowerName |
    lowerName := entityName asLowercase.
    inventory ifNil: [^false].
    ^inventory anySatisfy: [:entity | entity entityName asLowercase = lowerName]!

isInInventory: itemName ifTrue: trueBlock ifFalse: falseBlock
    (self isInInventory: itemName)
        ifTrue:  [ ^trueBlock value ]
        ifFalse: [ ^falseBlock value ].
!

addToInventory: anEntity
    | currentEntities |
    inventory ifNil: [inventory := OrderedCollection new].
    currentEntities := self entitiesAt: currentLocation.
    currentEntities remove: anEntity ifAbsent: [].
    locationEntities at: currentLocation put: currentEntities.
    inventory add: anEntity.!

removeFromInventory: anEntity
    | currentEntities |
    inventory ifNil: [^self].
    inventory remove: anEntity ifAbsent: [].
    currentEntities := self entitiesAt: currentLocation.
    currentEntities add: anEntity.
    locationEntities at: currentLocation put: currentEntities.!

!GameState methodsFor: 'state management'!
markExamined: itemName
    examined ifNil: [ examined := Set new ].
    examined add: itemName!

markTalked: personName event: eventName
    talked ifNil: [ talked := Set new ].
    talked add: personName, '_', eventName!

hasExamined: itemName
    examined ifNil: [^false].
    ^examined includes: itemName!

hasTalked: personName event: eventName
    talked ifNil: [^false].
    ^talked includes: (personName, '_', eventName)!

addTask: taskName
    tasks ifNil: [ tasks := Set new ].
    tasks add: taskName!

removeTask: taskName
    tasks ifNotNil: [
        tasks remove: taskName ifAbsent: [] ]!

hasTask: taskName
    ^tasks notNil and: [ tasks includes: taskName ]!

hasItem: itemName
    ^self isInInventory: itemName!

!GameState methodsFor: 'utility'!
isSupply: itemName
    | supplyNames |
    supplyNames := #('food' 'water' 'geiger' 'medkit' 'radio' 'gear' 'tools').
    ^supplyNames includes: itemName asLowercase!

countSupplies
    inventory ifNil: [^0].
    ^inventory count: [:entity | self isSupply: entity entityName]! !

Object subclass: #PlayerState
    instanceVariableNames: 'inventory'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'Game-Core'!

!PlayerState methodsFor: 'initialization'!
initialize
    super initialize.
    inventory := OrderedCollection new.!

!PlayerState methodsFor: 'accessing'!
inventory
    ^inventory!

inventory: aCollection
    inventory := aCollection!

!PlayerState class methodsFor: 'instance creation'!
empty
    ^self new!

extractFrom: aGameState
    ^self new
        inventory: aGameState inventory copy;
        yourself! !

Object subclass: #Command
    instanceVariableNames: 'type parameter'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'Game-Core'!

!Command methodsFor: 'initialization'!
initializeType: aType parameter: aParameter
    type := aType.
    parameter := aParameter.!

!Command class methodsFor: 'instance creation'!
type: aType
    ^self new initializeType: aType parameter: nil!

type: aType parameter: aParameter
    ^self new initializeType: aType parameter: aParameter!

!Command methodsFor: 'accessing'!
type
    ^type!

parameter
    ^parameter!

!Command methodsFor: 'testing'!
isLook
    ^type = #look!

isQuit
    ^type = #quit!

isInventory
    ^type = #inventory!

isHint
    ^type = #hint!

isInstructions
    ^type = #instructions!

isGo
    ^type = #go!

isExamine
    ^type = #examine!

isTalk
    ^type = #talk!

isTake
    ^type = #take!

isDrop
    ^type = #drop!

isUse
    ^type = #use!

isNext
    ^type = #next!

isAct2
    ^type = #act2!

isAct3
    ^type = #act3!

isUnknown
    ^type = #unknown! !

Object subclass: #GameUtils
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    category: 'Game-Core'!

!GameUtils class methodsFor: 'input/output'!
printLines: lines
    lines do: [:line | Transcript show: line; cr].!

printPrompt
    Transcript show: '> '.!

readCommand
    self printPrompt.
    ^FileStream stdin nextLine!

!GameUtils class methodsFor: 'parsing'!
parseCommand: inputString
    | words firstWord |
    words := inputString asLowercase substrings.
    words isEmpty ifTrue: [^Command type: #unknown].

    firstWord := words first.

    firstWord = 'look' ifTrue: [^Command type: #look].
    firstWord = 'quit' ifTrue: [^Command type: #quit].
    firstWord = 'inventory' ifTrue: [^Command type: #inventory].
    firstWord = 'hint' ifTrue: [^Command type: #hint].
    firstWord = 'instructions' ifTrue: [^Command type: #instructions].
    firstWord = 'next' ifTrue: [^Command type: #next].
    firstWord = 'start' ifTrue: [^Command type: #next].
    firstWord = 'act2' ifTrue: [^Command type: #act2].
    firstWord = 'act3' ifTrue: [^Command type: #act3].

    (firstWord = 'go' and: [words size > 1]) ifTrue: [
        ^Command type: #go parameter: words second].

    (firstWord = 'examine' and: [words size > 1]) ifTrue: [
        ^Command type: #examine parameter: words second].

    (firstWord = 'talk' and: [words size > 1]) ifTrue: [
        ^Command type: #talk parameter: words second].

    (firstWord = 'take' and: [words size > 1]) ifTrue: [
        ^Command type: #take parameter: words second].

    (firstWord = 'drop' and: [words size > 1]) ifTrue: [
        ^Command type: #drop parameter: words second].

    (firstWord = 'use' and: [words size > 1]) ifTrue: [
        ^Command type: #use parameter: words second].

    ^Command type: #unknown!

parseLocation: locationString
    | lowerStr |
    lowerStr := locationString asLowercase.

    lowerStr = 'barrack' ifTrue: [^Location barrack].
    lowerStr = 'yard' ifTrue: [^Location yard].
    lowerStr = 'runway' ifTrue: [^Location runway].
    lowerStr = 'depot' ifTrue: [^Location depot].
    lowerStr = 'tent' ifTrue: [^Location tent].
    lowerStr = 'cockpit' ifTrue: [^Location cockpit].
    lowerStr = 'crashsite' ifTrue: [^Location crashSite].
    lowerStr = 'cave' ifTrue: [^Location cave].
    lowerStr = 'wreck' ifTrue: [^Location wreck].
    lowerStr = 'tunnel' ifTrue: [^Location tunnel].
    lowerStr = 'ledge' ifTrue: [^Location ledge].
    lowerStr = 'tree' ifTrue: [^Location tree].
    lowerStr = 'ruins' ifTrue: [^Location ruins].
    lowerStr = 'woods' ifTrue: [^Location ruins].
    lowerStr = 'city' ifTrue: [^Location city].
    lowerStr = 'rock' ifTrue: [^Location rock].

    ^Location unknown!

!GameUtils class methodsFor: 'game data'!
instructionsText
    ^#(
        'Available commands are:'
        'look               -- look around you and describe surroundings'
        'go <place>         -- go to a place'
        'examine <obj>      -- examine an object or person closely'
        'talk <person>      -- talk to someone'
        'take <obj>         -- pick up an object'
        'drop <obj>         -- put down an object'
        'use <obj>          -- use an object you are carrying'
        'inventory          -- list currently held items'
        'instructions       -- see these instructions'
        'hint               -- get a hint if you are stuck'
        'act2               -- skip directly to Act 2'
        'act3               -- skip directly to Act 3'
        'quit               -- end the game and quit'
        ''
    )! !

"Initialize the classes"
EntityType initialize.
Location initialize.
