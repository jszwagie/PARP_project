"Act 1: Departure from the Edge of the World"

Object subclass: #Act1
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    category: 'Game-Acts'!

!Act1 class methodsFor: 'game content'!
prologText
    ^#(
        'ACT 1: DEPARTURE FROM THE EDGE OF THE WORLD'
        ''
        'You awaken to a stark view from your window at an Antarctic base camp in'
        'New Swabia. A desolate expanse of ice and snow stretches endlessly under'
        'a pale, gray sky. You get up, dress in layers against the cold and step'
        'outside.'
        ''
    )!

!Act1 class methodsFor: 'initialization'!
createInitialState
    | state entities tentEntities|
    state := GameState new.
    state currentLocation: Location yard.

    entities := Dictionary new.
    entities at: Location yard put: OrderedCollection new.

    entities at: Location barrack put: (OrderedCollection with:
        (Entity type: EntityType item name: 'photo' description: 'A photo of your late wife sits on the dresser.' takeable: false) with:
        (Entity type: EntityType item name: 'lighter' description: 'A simple silver lighter. You should really quit smoking.' takeable: true) with:
        (Entity type: EntityType item name: 'calendar' description: 'August 26, 1946.' takeable: false)).

    entities at: Location runway put: (OrderedCollection with:
        (Entity type: EntityType person name: 'clara' description: 'Clara stands near the plane, wearing a military pilot''s uniform with rolled-up sleeves.' takeable: false) with:
        (Entity type: EntityType item name: 'plane' description: 'Your type served well in the war.' takeable: false) with:
        (Entity type: EntityType item name: 'tanks' description: 'Fuel tanks for the plane. They''re running low.' takeable: false)).

    entities at: Location depot put: (OrderedCollection with:
        (Entity type: EntityType item name: 'canister' description: 'A heavy fuel canister. Necessary for the journey.' takeable: true)).

    tentEntities := OrderedCollection new.
    tentEntities add: (Entity type: EntityType item name: 'food' description: 'Canned goods and dried meals.' takeable: true).
    tentEntities add: (Entity type: EntityType item name: 'water' description: 'Fresh water in sealed containers.' takeable: true).
    tentEntities add: (Entity type: EntityType item name: 'geiger' description: 'A standard radiation detector.' takeable: true).
    tentEntities add: (Entity type: EntityType item name: 'medkit' description: 'Bandages, antiseptic, morphine…' takeable: true).
    tentEntities add: (Entity type: EntityType item name: 'radio' description: 'A shortwave field radio.' takeable: true).
    tentEntities add: (Entity type: EntityType item name: 'gear' description: 'Ropes, pitons, carabiners.' takeable: true).
    tentEntities add: (Entity type: EntityType item name: 'tools' description: 'A compass, maps, and a sextant.' takeable: true).
    tentEntities add: (Entity type: EntityType item name: 'list' description: 'A supply list showing available items.' takeable: false).
    entities at: Location tent put: tentEntities.

    state locationEntities: entities.
    ^state!

!Act1 class methodsFor: 'movement'!
canMove: fromLocation to: toLocation
    fromLocation = Location yard ifTrue: [
        ^toLocation = Location barrack or: [
            toLocation = Location runway or: [
                toLocation = Location depot or: [
                    toLocation = Location tent]]]].

    (toLocation = Location yard) ifTrue: [
        ^fromLocation = Location barrack or: [
            fromLocation = Location runway or: [
                fromLocation = Location depot or: [
                    fromLocation = Location tent]]]].

    ^false!

!Act1 class methodsFor: 'descriptions'!
describeLocation: location
    location = Location yard ifTrue: [
        ^'You''re on the BARRACK yard. Nearby, a sturdy twin-engine plane rests
on a makeshift RUNWAY, its metal hull glinting faintly in the weak sunlight.
To the side, there''s a fuel DEPOT and a supply TENT. The air is frigid,
the wind howls intermittently, and the isolation weighs heavily. By the plane,
you spot your partner, Lt. CLARA Voss, a pragmatic military pilot assigned
to join you on this mission.'].

    location = Location barrack ifTrue: [
        ^'This is your resting place during the mission - small but convenient.
Your bed is neatly made, and a PHOTO of your late wife sits on the dresser beside it.
Across the room, your working desk holds mission documents, a small lamp, and a LIGHTER.
A CALENDAR hangs above the desk.
Outside - the YARD, covered in snow.'].

    location = Location depot ifTrue: [
        ^'You step into the depot, a rough but functional structure shielding fuel CANISTERs from the Antarctic cold.
Outside - the YARD, covered in snow.'].

    location = Location tent ifTrue: [
        ^'You enter the supply tent, a cramped space cluttered with gear.
Boxes and crates are labeled with essentials: FOOD, WATER, scientific tools, and survival equipment.
A LIST of stock hangs on the wall.
Outside - the YARD, covered in snow.'].

    ^'You see nothing special.'!

describeRunway: gameState
    | supplyCount |
    supplyCount := gameState countSupplies.

    supplyCount > 0 ifTrue: [
        ^'Clara has finished fueling and has something waiting for you.
You pack the supplies into the plane. The reason for your journey -
Admiral Byrd''s diary - lies open on a box in front of you, its cryptic
coordinates circled in red ink: 70S, 10E.
Clara hands you a cup of lukewarm coffee.'].

    (gameState isInInventory: 'canister') ifFalse: [
        ^'The sunlight, reflected off the steel plates, blinds you as you approach
the aircraft - a Douglas A-20 Havoc. It''s not the newest PLANE, but it''s
reliable. CLARA is tinkering with one of the engines.
Behind you - the YARD, covered in snow.'].

    ^'The sunlight, reflected off the steel plates, blinds you as you approach
the Douglas A-20 Havoc - a reliable, if not modern, PLANE.
Clara is still tinkering with one of the engines.
Behind you - the YARD, covered in snow.'!

!Act1 class methodsFor: 'hints'!
getHint: gameState
    (gameState hasTask: 'act_finished') ifTrue: [
        ^'You''ve already finished this act. Type "quit" to leave the game.'].

    (gameState currentLocation = Location barrack and: [(gameState isInInventory: 'lighter') not]) ifTrue: [
        ^'I should gather something useful.'].

    ((gameState hasTask: 'fuel_request') and: [(gameState hasTask: 'can_take_canister') not]) ifTrue: [
        ^'I should check the fuel TANKS.'].

    ((gameState hasTask: 'fuel_request') and: [(gameState isInInventory: 'canister') not]) ifTrue: [
        ^'I should gather some fuel from the DEPOT.'].

    ((gameState hasTask: 'fuel_request') and: [gameState isInInventory: 'canister']) ifTrue: [
        ^'I should give the CANISTER to Clara.'].

    ((gameState hasTask: 'collect_supplies') and: [gameState currentLocation = Location runway and: [gameState countSupplies > 0]]) ifTrue: [
        ^'I should thank her for the coffee.'].

    ((gameState hasTask: 'collect_supplies') and: [gameState currentLocation ~= Location tent]) ifTrue: [
        ^'I should gather supplies in the supply TENT.'].

    ((gameState hasTask: 'collect_supplies') and: [gameState currentLocation = Location tent]) ifTrue: [
        ^'I should take only the most necessary items for the mission.'].

    ^'I think I should talk with Clara.'!

!Act1 class methodsFor: 'dialog'!
dialogWithClara: gameState
    | newState response |
    newState := gameState copy.
    response := OrderedCollection new.

    (gameState currentLocation ~= Location runway) ifTrue: [
        newState addTask: 'go_to_clara'.
        response add: 'I''m not going to shout; I should go to her at the RUNWAY.'.
        ^Array with: newState with: response].

    ((gameState hasTask: 'fuel_request') and: [gameState isInInventory: 'canister']) ifTrue: [
        | canister |
        canister := gameState findEntity: 'canister'.
        newState removeFromInventory: canister.
        newState removeTask: 'fuel_request'.
        newState addTask: 'awaiting_supply_choice'.
        response addAll: #(
            'You: "I have it!"'
            'Clara: "Nice, hand it over - our bird''s thirsty."'
            '*starts fueling the plane*'
            'Clara: "Why don''t you gather some supplies while I finish fueling?"'
            'Your choices:'
            '1. "On my way"'
            '2. "Are you sure you can handle it by yourself?"'
        ).
        ^Array with: newState with: response].

    ((gameState hasTask: 'collect_supplies') and: [gameState countSupplies > 0 and: [(gameState hasTask: 'explain_stage') not]]) ifTrue: [
        newState addTask: 'awaiting_explain_choice'.
        newState addTask: 'explain_stage'.
        response addAll: #(
            'You: "Thank you!"'
            '*a moment of silence*'
            'Clara: "So, tell me again why we''re risking our necks for this?'
            'A diary from some explorer doesn''t scream ''top priority'' to me."'
            'Your choices:'
            '1. "Because it could be the discovery of the century."'
            '2. "Orders are orders. The government wants answers."'
            '3. "I''ve got a feeling there''s something big waiting for us."'
            ''
        ).
        ^Array with: newState with: response].

    ((gameState hasTalked: 'clara' event: 'fuel_request') not) ifTrue: [
        newState markTalked: 'clara' event: 'fuel_request'.
        newState addTask: 'awaiting_clara_choice'.
        response addAll: #(
            'Clara: "Morning, doc. We need to get this bird fueled up. Could you check the TANKS for me?"'
            'Your choices:'
            '1. "Okay, I''ll handle it now."'
            '2. "I think it''s good enough, but I could double-check."'
            '3. "Why don''t you take care of it?"'
        ).
        ^Array with: newState with: response].

    response add: 'Clara is busy working on the plane.'.
    ^Array with: newState with: response!

processChoice: choice gameState: gameState choiceType: choiceType
    | newState response |
    newState := gameState copy.
    response := OrderedCollection new.

    choiceType = 'clara' ifTrue: [
        choice = '1' ifTrue: [
            newState removeTask: 'awaiting_clara_choice'.
            newState addTask: 'fuel_request'.
            response add: 'You: "Okay, I''ll handle it now."'.
            ^Array with: newState with: response].

        choice = '2' ifTrue: [
            newState removeTask: 'awaiting_clara_choice'.
            newState addTask: 'fuel_request'.
            response addAll: #(
                'You: "I think it''s good enough, but I could double-check."'
                'Clara: "Good enough doesn''t cut it out here. Antarctica doesn''t forgive mistakes. Check it properly."'
            ).
            ^Array with: newState with: response].

        choice = '3' ifTrue: [
            newState removeTask: 'awaiting_clara_choice'.
            newState addTask: 'collect_supplies'.
            response addAll: #(
                'You: "Why don''t you take care of it?"'
                'Clara (frowning): "Oh, you''re lazy, aren''t you? Fine, I''ll handle it after I check the oil,'
                'but you''re not off the hook, doc. Go gather mandatory supplies and drop them near the plane."'
                ''
            ).
            ^Array with: newState with: response]].

    choiceType = 'supply' ifTrue: [
        (choice = '1' or: [choice = '2']) ifTrue: [
            newState removeTask: 'awaiting_supply_choice'.
            newState addTask: 'collect_supplies'.
            choice = '1'
                ifTrue: [response add: 'You: "On my way."']
                ifFalse: [response addAll: #(
                    'You: "Are you sure you can handle it by yourself?"'
                    'Clara: "Don''t worry, doc - I''m not a kid. Go grab those supplies."'
                )].
            response add: ''.
            ^Array with: newState with: response]].

    choiceType = 'explain' ifTrue: [
        choice = '1' ifTrue: [
            newState removeTask: 'awaiting_explain_choice'.
            newState addTask: 'awaiting_further_choice'.
            response addAll: #(
                'You: "Because it could be the discovery of the century."'
                'Clara: "Discovery of the century? I hope it''s not just a pile of ice and a frostbite bill."'
                'Your choice:'
                '1. "Byrd wasn''t a dreamer. Those coordinates mean something."'
                '2. "Even if it''s nothing, the science alone is worth it."'
            ).
            ^Array with: newState with: response].

        (choice = '2' or: [choice = '3']) ifTrue: [
            newState removeTask: 'awaiting_explain_choice'.
            choice = '2'
                ifTrue: [response addAll: #(
                    'You: "Orders are orders. The government wants answers."'
                    'Clara: "Yeah, and Uncle Sam loves sending us into the freezer for kicks. What''s their angle?"'
                    'You: "Cold War jitters, probably. They don''t want the Soviets sniffing around first."'
                )]
                ifFalse: [response addAll: #(
                    'You: "I''ve got a feeling there''s something big waiting for us."'
                    'Clara: "Feelings don''t keep us warm, doc. What''s in that diary that''s got you hooked?"'
                    'You: "Hints of a hidden land - geological oddities, maybe more."'
                )].
            ^self actEpilog: response gameState: newState]].

    choiceType = 'further' ifTrue: [
        (choice = '1' or: [choice = '2']) ifTrue: [
            newState removeTask: 'awaiting_further_choice'.
            choice = '1'
                ifTrue: [response addAll: #(
                    'You: "Byrd wasn''t a dreamer. Those coordinates mean something."'
                    'Clara: "Maybe. But I''d rather not die proving him right."'
                )]
                ifFalse: [response addAll: #(
                    'You: "Even if it''s nothing, the science alone is worth it."'
                    'Clara: "Maybe. But I''d rather not die proving him right."'
                )].
            ^self actEpilog: response gameState: newState]].

    response add: 'Invalid choice - enter a valid number.'.
    ^Array with: newState with: response!

actEpilog: preResponse gameState: gameState
    | newState response |
    newState := gameState copy.
    newState addTask: 'act_finished'.

    response := preResponse asOrderedCollection.
    response addAll: #(
        ''
        'You: "What do you think we''ll find out there?"'
        'Clara: "Best case? A rock formation worth naming. Worst case? A grave with our names on it.'
        'I don''t buy the unearthly land garbage."'
        'You: "Neither do I, but the government does."'
        'Clara: "I think it''s time we have a good weather."'
        ''
        'Preparations complete, you and Clara climb into the plane''s hatch.'
        'Clara starts the engines, ready to challenge the icy wilderness.'
        'The plane roars to life, cutting through swirling snow as it lifts off.'
        ''
        'Inside, you study the diary while Clara grips the yoke.'
        'The horizon swallows the base camp, leaving you with a mix of anticipation -'
        'and a hint of lurking danger.'
        ''
        '----------------------------ACT 1 OVER----------------------------'
        ''
        'Type "next" to continue, or "quit" to leave.'
        ''
    ).
    ^Array with: newState with: response!

!Act1 class methodsFor: 'special examine'!
examineSpecial: itemName gameState: gameState
    | newState response |
    newState := gameState copy.
    response := OrderedCollection new.

    (itemName = 'tanks' and: [gameState currentLocation = Location runway]) ifTrue: [
        newState addTask: 'can_take_canister'.
        newState markExamined: 'tanks'.
        response addAll: #(
            'You crouch beside the aircraft and open the fuel hatch.'
            'We''re running low. We need at least one more drum of fuel.'
            'Clara: "Told you. Go grab one from the DEPOT."'
            ''
        ).
        ^Array with: newState with: response].

    (itemName = 'list' and: [gameState currentLocation = Location tent]) ifTrue: [
        newState markExamined: 'list'.
        response addAll: #(
            '- FOOD rations'
            '- WATER'
            '- GEIGER Counter'
            '- MEDKIT'
            '- RADIO'
            '- Climbing GEAR'
            '- Navigation TOOLS'
            'The plane has a capacity of only 5 items; choose wisely.'
            ''
        ).
        ^Array with: newState with: response].

    (itemName = 'photo' and: [gameState currentLocation = Location barrack]) ifTrue: [
        newState markExamined: 'photo'.
        response addAll: #('I''ll never forget you, my love.' '').
        ^Array with: newState with: response].

    (itemName = 'canister' and: [gameState currentLocation = Location depot]) ifTrue: [
        (gameState hasTask: 'can_take_canister') ifFalse: [
            response addAll: #('I should check the fuel tanks first.' '').
            ^Array with: newState with: response].
        newState markExamined: 'canister'.
        response addAll: #('Heavy, but necessary.' '').
        ^Array with: newState with: response].

    ^nil "No special examination"!

!Act1 class methodsFor: 'game step'!
step: command gameState: gameState
    | newState response result entity |
    newState := gameState copy.
    response := OrderedCollection new.

    command isNext ifTrue: [
        (gameState hasTask: 'act_finished') ifTrue: [
            response add: ''.
            ^Array with: newState with: response].
        response add: 'You need to finish this act first.'.
        ^Array with: newState with: response].

    (gameState hasTask: 'act_finished') ifTrue: [
        response add: 'You''ve already finished this act. Type "quit" to exit or "next" to advance further.'.
        response add: ''.
        ^Array with: newState with: response].

    command isQuit ifTrue: [
        Smalltalk quitPrimitive.
        ^Array with: newState with: response].

    command isLook ifTrue: [
        gameState currentLocation = Location runway
            ifTrue: [response add: (self describeRunway: gameState)]
            ifFalse: [response add: (self describeLocation: gameState currentLocation)].
        response add: ''.
        ^Array with: newState with: response].

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
        | targetLocation |
        targetLocation := GameUtils parseLocation: command parameter.
        targetLocation = Location unknown ifTrue: [
            response add: 'Unknown place: ', command parameter.
            response add: ''.
            ^Array with: newState with: response].

        (self canMove: gameState currentLocation to: targetLocation) ifTrue: [
            newState currentLocation: targetLocation.
            targetLocation = Location runway
                ifTrue: [response add: (self describeRunway: newState)]
                ifFalse: [response add: (self describeLocation: targetLocation)].
            response add: ''.
            ^Array with: newState with: response]
        ifFalse: [
            response add: 'You can''t go to ', command parameter, ' from here.'.
            response add: ''.
            ^Array with: newState with: response]].

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
            ^self dialogWithClara: gameState].
        response add: 'There''s no one here to talk to by that name.'.
        response add: ''.
        ^Array with: newState with: response].

    command isTake ifTrue: [
        ^self handleTake: command parameter gameState: gameState].

    command isDrop ifTrue: [
        ^self handleDrop: command parameter gameState: gameState].

    command isUse ifTrue: [
        ^self handleUse: command parameter gameState: gameState].

    response add: 'Unknown command.'.
    response add: ''.
    ^Array with: newState with: response!

handleTake: itemName gameState: gameState
    | newState response entity lowerName |
    newState := gameState copy.
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

    lowerName = 'photo' ifTrue: [
        response add: 'Sorry, my love, but I can''t take you with me.'.
        response add: ''.
        ^Array with: newState with: response].

    lowerName = 'calendar' ifTrue: [
        response add: 'I doubt this will be useful.'.
        response add: ''.
        ^Array with: newState with: response].

    lowerName = 'lighter' ifTrue: [
        response add: 'I''m not going to smoke now.'.
        response add: ''.
        ^Array with: newState with: response].

    lowerName = 'canister' ifTrue: [
        (gameState hasTask: 'can_take_canister') ifFalse: [
            response add: 'I should check the fuel TANKS first.'.
            response add: ''.
            ^Array with: newState with: response].
        newState addToInventory: entity.
        response add: 'This should be enough.'.
        response add: ''.
        ^Array with: newState with: response].

    (gameState isSupply: lowerName) ifTrue: [
        (gameState hasTask: 'collect_supplies') ifFalse: [
            response add: 'I don''t need this right now. I should TALK to Clara first.'.
            response add: ''.
            ^Array with: newState with: response].

        gameState countSupplies >= 5 ifTrue: [
            response add: 'You cannot take this - you''ve reached the limit (5 items).'.
            response add: ''.
            ^Array with: newState with: response].

        newState addToInventory: entity.
        response add: 'You take the ', entity entityName, '.'.
        response add: ''.
        ^Array with: newState with: response].

    entity takeableByDefault ifTrue: [
        newState addToInventory: entity.
        response add: 'You take the ', entity entityName, '.'.
        response add: ''.
        ^Array with: newState with: response]
    ifFalse: [
        response add: 'You can''t take that.'.
        response add: ''.
        ^Array with: newState with: response]!

handleDrop: itemName gameState: gameState
    | newState response entity lowerName |
    newState := gameState copy.
    response := OrderedCollection new.
    lowerName := itemName asLowercase.

    (gameState isInInventory: lowerName) ifFalse: [
        response add: 'You aren''t carrying that!'.
        response add: ''.
        ^Array with: newState with: response].

    entity := gameState findEntity: lowerName.

    lowerName = 'lighter' ifTrue: [
        newState removeFromInventory: entity.
        response add: 'I might need something else instead.'.
        response add: ''.
        ^Array with: newState with: response].

    lowerName = 'canister' ifTrue: [
        response add: 'I should get it to Clara. I''ll TALK to her.'.
        response add: ''.
        ^Array with: newState with: response].

    (gameState isSupply: lowerName) ifTrue: [
        newState removeFromInventory: entity.
        response add: 'You drop the ', entity entityName, '.'.
        response add: ''.
        ^Array with: newState with: response].

    newState removeFromInventory: entity.
    response add: 'OK.'.
    response add: ''.
    ^Array with: newState with: response!

handleUse: itemName gameState: gameState
    | newState response lowerName |
    newState := gameState copy.
    response := OrderedCollection new.
    lowerName := itemName asLowercase.

    (lowerName = 'canister' and: [gameState isInInventory: 'canister' and: [gameState currentLocation = Location runway]]) ifTrue: [
        response add: 'I should give it to Clara. I''ll TALK to her.'.
        response add: ''.
        ^Array with: newState with: response].

    (gameState isInInventory: lowerName) ifTrue: [
        response add: 'I can''t use that right now.'.
        response add: ''.
        ^Array with: newState with: response].

    response add: 'I don''t have it or I can''t use it.'.
    response add: ''.
    ^Array with: newState with: response!

!Act1 class methodsFor: 'main game loop'!
gameLoop: initialState
    | state command result newState output line |
    state := initialState.
    [true] whileTrue: [
        line := GameUtils readCommand.

        "Handle choice inputs"
        (state hasTask: 'awaiting_clara_choice') ifTrue: [
            result := self processChoice: line gameState: state choiceType: 'clara'.
            newState := result first.
            output := result second.
            GameUtils printLines: output.
            state := newState.
            ] ifFalse: [
        (state hasTask: 'awaiting_explain_choice') ifTrue: [
            result := self processChoice: line gameState: state choiceType: 'explain'.
            newState := result first.
            output := result second.
            GameUtils printLines: output.
            state := newState.
            ] ifFalse: [
        (state hasTask: 'awaiting_further_choice') ifTrue: [
            result := self processChoice: line gameState: state choiceType: 'further'.
            newState := result first.
            output := result second.
            GameUtils printLines: output.
            state := newState.
            ] ifFalse: [
        (state hasTask: 'awaiting_supply_choice') ifTrue: [
            result := self processChoice: line gameState: state choiceType: 'supply'.
            newState := result first.
            output := result second.
            GameUtils printLines: output.
            state := newState.
            ] ifFalse: [

        "Handle regular commands"
        command := GameUtils parseCommand: line.

        command isTalk ifTrue: [
            command parameter asLowercase = 'clara' ifTrue: [
                result := self dialogWithClara: state.
                newState := result first.
                output := result second.
                GameUtils printLines: output.
                state := newState.
                ] ifFalse: [
                result := self step: command gameState: state.
                newState := result first.
                output := result second.
                GameUtils printLines: output.
                state := newState.
                ]
            ] ifFalse: [
            result := self step: command gameState: state.
            newState := result first.
            output := result second.
            GameUtils printLines: output.
            state := newState.
            ].

        command isQuit ifTrue: [^PlayerState extractFrom: newState].
        (command isNext and: [newState hasTask: 'act_finished']) ifTrue: [
            ^PlayerState extractFrom: newState].
        ]]]].
    ]!

start: playerState
    | gameState |
    GameUtils printLines: GameUtils instructionsText.
    GameUtils printLines: self prologText.
    gameState := self createInitialState.
    GameUtils printLines: (Array with: (self describeLocation: gameState currentLocation) with: '').
    ^self gameLoop: gameState! !
