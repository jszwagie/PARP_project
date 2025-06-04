"Act 2: Descent into the Unknown"

Object subclass: #Act2
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    category: 'Game-Acts'!

!Act2 class methodsFor: 'game content'!
prologText
    ^#(
        'ACT 2: DESCENT INTO THE UNKNOWN'
        'You are sitting in the co-pilot chair of a Douglas A-20 Havoc, soaring over'
        'Antarctica''s icy expanse. Clara pilots beside you, and you hold Admiral Byrd''s diary.'
    )!

epilogText
    ^#(
        '----------------------------ACT 2 OVER----------------------------'
        ''
        'Type "next" to continue, or "quit" to leave.'
        ''
    )!

!Act2 class methodsFor: 'initialization'!
createInitialState
    | state entities |
    state := GameState new.
    state currentLocation: Location cockpit.

    entities := Dictionary new.

    entities at: Location cockpit put: (OrderedCollection with:
        (Entity type: EntityType item name: 'diary' description: '*It is open on the coordinates*There must be some truth in it.' takeable: false) with:
        (Entity type: EntityType item name: 'radio' description: '*The radio has a frequency adjuster*Maybe I could run into something interesting by switching frequencies.' takeable: false) with:
        (Entity type: EntityType person name: 'clara' description: 'Clara pilots beside you, focused on the controls.' takeable: false)).

    entities at: Location crashSite put: (OrderedCollection with:
        (Entity type: EntityType item name: 'plane' description: 'The plane''s a lost cause, but the luggage COMPARTMENT is intact.The supplies you took are probably still there.' takeable: false) with:
        (Entity type: EntityType item name: 'compartment' description: 'The plane compartment for supplies.' takeable: false)).

    entities at: Location compartment put: OrderedCollection new.

    entities at: Location wreck put: (OrderedCollection with:
        (Entity type: EntityType item name: 'wreck' description: 'A wrecked plane' takeable: false) with:
        (Entity type: EntityType item name: 'pistol' description: 'A pistol, probably from the wreck.' takeable: true)).

    state locationEntities: entities.
    ^state!

injectSupplies: playerState into: gameState
    | supplies compartmentEntities |
    supplies := playerState inventory select: [:item | gameState isSupply: item entityName].
    compartmentEntities := gameState entitiesAt: Location compartment.
    compartmentEntities addAll: supplies.
    gameState locationEntities at: Location compartment put: compartmentEntities.

    "Remove supplies from player inventory and add non-supplies back to game state"
    gameState inventory: (playerState inventory reject: [:item | gameState isSupply: item entityName]).!

!Act2 class methodsFor: 'movement'!
canMove: fromLocation to: toLocation
    fromLocation = Location crashSite ifTrue: [^toLocation = Location cave].
    fromLocation = Location cave ifTrue: [^toLocation = Location crashSite or: [toLocation = Location wreck]].
    fromLocation = Location wreck ifTrue: [^toLocation = Location cave].
    fromLocation = Location cave ifTrue: [^toLocation = Location tunnel].
    fromLocation = Location tunnel ifTrue: [^toLocation = Location cave].
    ^false!

!Act2 class methodsFor: 'descriptions'!
describeLocation: location
    location = Location cockpit ifTrue: [
        ^'The cockpit is tight and utilitarian, filled with glowing dials and humming switches.
Through the windshield, Antarctica''s endless snow glitters under a gray sky.
Turbulence occasionally rocks the plane, rattling the controls.
Before the control panel, next to you, on the pilot''s seat is CLARA, bravely piloting the plane.
On the panel sits the RADIO, and Byrd''s DIARY rests on your lap.'].

    location = Location wreck ifTrue: [
        ^'The interior is cramped and dark, with control panels covered in dust and frost.
Wires hang loosely, and a faint smell of oil lingers.
On a seat, you spot an old German PISTOL—a Mauser C96—still holstered.'].

    ^'You see nothing special.'!

describeCrashSite: gameState
    (gameState hasTask: 'crash_site_described') ifFalse: [
        ^'You wake amid the wreckage, cold seeping into your bones.
The PLANE is a ruin, and CLARA lies injured nearby.
Twisted metal juts from the snow, half-burying the fuselage; engine debris still smoulders.
Wind howls, stinging your face with ice.
CLARA slumps a few feet away, blood staining the snow beneath her head.'].

    (gameState hasTask: 'injured_clara') ifTrue: [
        ^'The PLANE is a ruin, and CLARA lies injured nearby.
Twisted metal juts from the snow, half-burying the fuselage; engine debris still smoulders.
Wind howls, stinging your face with ice.
CLARA slumps a few feet away, blood staining the snow beneath her head.'].

    ^'The wreckage of your plane lies scattered across the frozen landscape.
Clara stands nearby, looking shaken but determined.
This place offers no shelter from the biting Antarctic cold.
The crash site aligns with Byrd''s coordinates.'!

describeCave: gameState
    (gameState hasTask: 'entered_wreck') ifTrue: [
        ^'Slowly and carefully, you emerge from the wreckage.
The dark cave corridor stretches before you.'].

    (gameState hasTask: 'wreck_discovery') ifFalse: [
        ^'The cave twists downward, its walls polished and warm. A low hum vibrates the air.
Smooth, spiralling walls funnel you DEEPER; faint lights pulse below.'].

    ^'The entrance to the ice cave stretches before you.
The unnatural smoothness of the walls suggests intelligent design.
To the right, the Nazi flying-saucer WRECK remains embedded in the ice.
A path leads DEEPER into the tunnel, where faint blue light pulses.
A massive disk-shaped craft protrudes from the wall, its metallic surface scarred and dented—
futuristic yet ancient.'!

!Act2 class methodsFor: 'hints'!
getHint: gameState
    (gameState currentLocation = Location crashSite and: [(gameState hasTask: 'crash_site_described') not]) ifTrue: [
        ^'I should first LOOK around to get my bearings.'].

    (gameState currentLocation = Location cockpit and: [(gameState hasTask: 'radio_examined') not]) ifTrue: [
        ^'I need to find something to pass the time.'].

    (gameState currentLocation = Location cockpit and: [(gameState hasTask: 'radio_examined') and: [(gameState hasTask: 'radio_used') not]] ) ifTrue: [
        ^'I could USE the radio to pass some time.'].

    (gameState currentLocation = Location cockpit and: [(gameState hasTask: 'radio_used') and: [(gameState hasTask: 'crashed') not]] ) ifTrue: [
        ^'I should talk to Clara.'].

    (gameState currentLocation = Location crashSite and: [(gameState hasTask: 'injured_clara') and: [(gameState hasTask: 'plane_examined') not]] ) ifTrue: [
        ^'Clara needs help fast, and the wreckage of the PLANE might have something useful.'].

    (gameState currentLocation = Location crashSite and: [(gameState hasTask: 'injured_clara')  and: [gameState hasTask: 'plane_examined']]) ifTrue: [
        ^'Clara needs help fast, and a MEDKIT should be in the luggage COMPARTMENT.'].

    (gameState currentLocation = Location crashSite and: [(gameState hasTask: 'injured_clara') not] ) ifTrue: [
        ^'I should check the luggage COMPARTMENT for the rest of the supplies.'].

    (gameState currentLocation = Location crashSite and: [(gameState hasTask: 'injured_clara') not and: [(gameState hasTask: 'compartment_checked') not]]) ifTrue: [
        ^'I should talk to Clara about our next move.'].

    (gameState currentLocation = Location cave and: [(gameState hasTask: 'wreck_discovery') and: [(gameState hasTask: 'wreck_examined') not]] ) ifTrue: [
        ^'I should EXAMINE the WRECK.'].

    (gameState currentLocation = Location cave and: [(gameState hasTask: 'wreck_examined') and: [(gameState hasTask: 'entered_wreck') not and: [(gameState hasTask: 'wreck_discovery2') not]] ] ) ifTrue: [
        ^'I should talk to Clara.'].

    (gameState currentLocation = Location cave and: [(gameState hasTask: 'wreck_discovery2') and: [(gameState hasTask: 'entered_wreck') not]]  ) ifTrue: [
        ^'I must decide, should I GO to WRECK or GO DEEPER?'].

    (gameState currentLocation = Location cave) ifTrue: [
        ^'I should talk to Clara.'].

    (gameState currentLocation = Location wreck and: [(gameState hasItem: 'pistol') not]) ifTrue: [
        ^'I should look around for anything useful.'].

    (gameState currentLocation = Location wreck) ifTrue: [
        ^'I should talk to Clara.'].

    ^'I should try to LOOK around to get my bearings.'!

!Act2 class methodsFor: 'dialog'!
dialogClara: gameState
    | newState response |
    newState := gameState deepCopy.
    response := OrderedCollection new.

    (gameState currentLocation = Location cockpit and: [(gameState hasTask: 'radio_used') and: [(gameState hasTask: 'crashed') not]] ) ifTrue: [
        newState addTask: 'crashed'.
        newState addTask: 'injured_clara'.
        newState currentLocation: Location crashSite.
        response addAll: #(
            'You: "Is everything okay?"'
            'Clara: "I don''t know; the compass and the altimeter suddenly started going crazy,'
            'but we''re close to our destination, so it shouldn''t be a probl-"'
            ''
            '*Suddenly, turbulence slams the plane. Lights flicker, instruments fail, the engines choke.*'
            'Clara (shouting): "Brace yourself! Everything''s shutting down!"'
            'You (screaming): "Ahh, what''s happening!?"'
            '*The plane spirals down, crashing into the ice. Darkness falls.*'
            ''
        ).
        ^Array with: newState with: response].

    (gameState currentLocation = Location cockpit and: [(gameState hasTask: 'crashed') not and: [(gameState hasTask: 'cockpit_intro_done') not]] ) ifTrue: [
        newState addTask: 'awaiting_cockpit_choice'.
        response addAll: #(
            'Clara glances over: "So, doc, what''s your take? Are we on a wild goose chase,'
            'or is there really something out here in this frozen wasteland?"'
            'Your choices:'
            '1. "Byrd''s diary points to 70S, 10E. The coordinates are too specific to be nothing."'
            '2. "I don''t know, but the weather''s turning ugly. We need to stay sharp."'
            '3. "Whether it''s real or not, the mission''s worth it for the discovery alone."'
            ''
        ).
        ^Array with: newState with: response].

    (gameState currentLocation = Location cockpit and: [(gameState hasTask: 'crashed') not]) ifTrue: [
        response addAll: #(
            'You: "We''re bound to find something there—-I can feel it in my bones."'
            'Clara: "Hopefully, or all our efforts will be for nothing."'
        ).
        ^Array with: newState with: response].

    (gameState currentLocation = Location crashSite and: [(gameState hasTask: 'injured_clara') and: [(gameState hasItem: 'medkit') not]] ) ifTrue: [
        response addAll: #('She''s unconscious and needs medical attention urgently.' '').
        ^Array with: newState with: response].

    (gameState currentLocation = Location crashSite and: [(gameState hasTask: 'injured_clara') not and: [(gameState hasTask: 'cave_advice') not]] ) ifTrue: [
        newState addTask: 'cave_advice'.
        response addAll: #(
            'Clara: "We can''t stay exposed out here. That CAVE might be our only shot,'
            'but it''s giving me a bad feeling. We must GO now, before it gets dark."'
            ''
        ).
        ^Array with: newState with: response].

    (gameState currentLocation = Location cave and: [(gameState hasTask: 'wreck_discovery') not]) ifTrue: [
        newState addTask: 'wreck_discovery'.
        response addAll: #(
            'Clara: "Hey, what''s that? Do you see it?"'
            ''
            'On the right side of the tunnel you see a disk-shaped WRECK –'
            'a massive saucer-like craft embedded in the ice, its metallic surface scarred and dented.'
            'It looks futuristic yet ancient.'
        ).
        ^Array with: newState with: response].

    (gameState currentLocation = Location cave and: [(gameState hasTask: 'wreck_examined') and: [(gameState hasTask: 'entered_wreck') not and: [(gameState hasTask: 'wreck_discovery2') not]]]  ) ifTrue: [
        newState addTask: 'wreck_discovery2'.
        response addAll: #(
            'You: "This is incredible. A Nazi flying saucer?"'
            'Clara: "Looks like it. But how did it get here? And why?"'
            'You: "Maybe they were experimenting with advanced technology in Antarctica."'
            'Clara: "Or maybe they found something here. Either way, it''s creepy."'
            ''
            'Clara: "Do you think we should try to get INSIDE it or don''t risk and GO DEEPER?"'
        ).
        ^Array with: newState with: response].

    (gameState currentLocation = Location wreck) ifTrue: [
        response addAll: #(
            'Clara: "This place gives me the creeps. Look at these controls — they''re way ahead of their time."'
            'You: "Yeah, it''s like something out of science-fiction. But it''s real."'
            'Clara: "I think we''ve seen enough. Let''s keep moving; there might be more ahead. We should GO DEEPER."'
            ''
        ).
        ^Array with: newState with: response].

    response add: 'There''s no one here to talk to.'.
    ^Array with: newState with: response!

processChoice: choice gameState: gameState choiceType: choiceType
    | newState response |
    newState := gameState deepCopy.
    response := OrderedCollection new.

    choiceType = 'cockpit' ifTrue: [
        newState addTask: 'cockpit_intro_done'.
        newState removeTask: 'awaiting_cockpit_choice'.

        choice = '1' ifTrue: [
            response addAll: #(
                'You: "Byrd''s diary points to 70S, 10E. The coordinates are too specific to be nothing."'
                'Clara: "Specific or not, Antarctica''s a maze. Let''s hope those coordinates don''t lead us straight into trouble."'
            )].
        choice = '2' ifTrue: [
            response addAll: #(
                'You: "I don''t know, but the weather''s turning ugly. We need to stay sharp."'
                'Clara: "Yeah, I feel it too. This storm''s got teeth. Keep your eyes peeled."'
                ''
            )].
        choice = '3' ifTrue: [
            response addAll: #(
                'You: "Whether it''s real or not, the mission''s worth it for the discovery alone."'
                'Clara: "Discovery''s great until the ice swallows us whole. Still, I like your optimism."'
            )].
        choice = '1' ifFalse: [choice = '2' ifFalse: [choice = '3' ifFalse: [
            response add: 'Invalid choice - enter 1, 2, or 3.'.
            ^Array with: gameState with: response]]].
        ^Array with: newState with: response].

    choiceType = 'radio' ifTrue: [
        choice = '1' ifTrue: [
            newState addTask: 'radio_used'.
            newState removeTask: 'awaiting_radio_choice'.
            response addAll: #(
                'You: "Nah, you''re freaking out; that''s just some usual anomalies. Focus on piloting."'
                'Clara: "Yeah, you''re right; there''s no time for that."'
            ).
            ^Array with: newState with: response].

        choice = '2' ifTrue: [
            newState addTask: 'awaiting_radio_background'.
            newState removeTask: 'awaiting_radio_choice'.
            response addAll: #(
                'You: "Oh, you know German? I should have guessed from your surname."'
                'Clara: "Yes, my father was a German immigrant. He went to the USA when WWI started."'
                'Your choices:'
                '1. "Byrd''s diary doesn''t mention Germans, but hey, we''re in what Nazi Germany claimed as their territory in Antarctica."'
                '2. "Ah, Uncle Sam, a shelter for all the world''s people in need."'
                ''
            ).
            ^Array with: newState with: response].

        response add: 'Invalid choice - enter 1 or 2.'.
        ^Array with: gameState with: response].

    choiceType = 'radio_background' ifTrue: [
        newState addTask: 'radio_used'.
        newState removeTask: 'awaiting_radio_background'.

        choice = '1' ifTrue: [
            response addAll: #(
                'You: "Byrd''s diary doesn''t mention Germans, but hey, we''re in what Nazi Germany claimed as their territory in Antarctica."'
                'Clara: "I''m sure the last thing we want is for my German to come in handy. This whole mission feels unreal and ridiculous."'
            )].
        choice = '2' ifTrue: [
            response addAll: #(
                'You: "Ah, Uncle Sam, a shelter for all the world''s people in need."'
                'Clara: "Until he sends you on a mission like this, haha."'
                ''
            )].
        choice = '1' ifFalse: [choice = '2' ifFalse: [
            response add: 'Invalid choice - enter 1 or 2.'.
            ^Array with: gameState with: response]].
        ^Array with: newState with: response].

    ^Array with: gameState with: response!

!Act2 class methodsFor: 'special examine'!
examineSpecial: itemName gameState: gameState
    | newState response |
    newState := gameState deepCopy.
    response := OrderedCollection new.

    (itemName = 'diary' and: [gameState currentLocation = Location cockpit]) ifTrue: [
        newState markExamined: 'diary'.
        response addAll: #(
            '*It is open on the coordinates*'
            'There must be some truth in it.'
            ''
        ).
        ^Array with: newState with: response].

    (itemName = 'radio' and: [gameState currentLocation = Location cockpit]) ifTrue: [
        newState addTask: 'radio_examined'.
        newState markExamined: 'radio'.
        response addAll: #(
            '*The radio has a frequency adjuster*'
            'Maybe I could run into something interesting by switching frequencies.'
            ''
        ).
        ^Array with: newState with: response].

    (itemName = 'clara' and: [(gameState currentLocation = Location cockpit) and: [(gameState hasTask: 'crashed') not]] ) ifTrue: [
        response addAll: #(
            'Clara pilots beside you, focused on the controls.'
            ''
        ).
        ^Array with: newState with: response].

    (itemName = 'clara' and: [(gameState currentLocation = Location crashSite) and: [gameState hasTask: 'crashed']] ) ifTrue: [
        response addAll: #(
            'She''s unconscious, her forehead gashed, her breathing shallow.'
            'Blood soaks the snow around her leg.'
            ''
        ).
        ^Array with: newState with: response].

    (itemName = 'plane' and: [gameState currentLocation = Location crashSite]) ifTrue: [
        newState addTask: 'plane_examined'.
        newState markExamined: 'plane'.
        response addAll: #(
            'The plane''s a lost cause, but the luggage COMPARTMENT is intact.'
            'The supplies you took are probably still there.'
            ''
        ).
        ^Array with: newState with: response].

    (itemName = 'compartment' and: [gameState currentLocation = Location crashSite]) ifTrue: [
        ^self examineCompartment: gameState].

    (itemName = 'wreck' and: [gameState currentLocation = Location cave]) ifTrue: [
        newState addTask: 'wreck_examined'.
        newState markExamined: 'wreck'.
        response addAll: #(
            'A disk-shaped craft protrudes from the ice, marked with a Nazi Balkenkreuz,'
            '"Hergestellt in Deutschland. 1944. Danzig". Machine-gun nests bristle from'
            'its surface.'
            ''
            'Clara: "Made in Germany. 1944. Danzig. I think that''s Nazi tech-what''s it doing here?"'
            ''
            'There appears to be an entrance. You could GO inside to investigate further.'
            ''
        ).
        ^Array with: newState with: response].

    (itemName = 'pistol' and: [gameState currentLocation = Location wreck]) ifTrue: [
        newState markExamined: 'pistol'.
        response addAll: #(
            'An old German Mauser C96 pistol. Still looks functional.'
            ''
        ).
        ^Array with: newState with: response].

    ^nil!

examineCompartment: gameState
    | newState response supplies hasMedkit |
    newState := gameState deepCopy.
    response := OrderedCollection new.
    newState addTask: 'compartment_checked'.
    newState markExamined: 'compartment'.
    newState markExamined: 'plane'.

    supplies := gameState entitiesAt: Location compartment.
    hasMedkit := supplies anySatisfy: [:item | item entityName asLowercase = 'medkit'].

    ((gameState hasTask: 'injured_clara') and: [hasMedkit not]) ifTrue: [
        newState addTask: 'game_over'.
        newState addTask: 'act2_finished'.
        response addAll: #(
            'You check the plane compartment for supplies.'
            'You realize you forgot to take the MEDKIT before departure.'
            'You: "How could I forget it? What can I do now?"'
            'You start to panic, gasping heavily.'
            ''
            'You: "Clara, Clara, wake up!"'
            'You try to rouse Clara, but it''s futile.'
            'You: (crying) "Clara, please, I can''t do this alone."'
            ''
            'With her wound untreated, Clara continues to bleed.'
            'Suddenly, her heart stops beating.'
            'As the cold overwhelms you, the Antarctic claims you both.'
            'GAME OVER'
            ''
        ).
        ^Array with: newState with: response].

    response add: 'You check the plane compartment for supplies.'.
    response add: 'Inside you find:'.
    supplies do: [:item | response add: '- ', item entityName].
    response add: ''.
    ^Array with: newState with: response!

!Act2 class methodsFor: 'special actions'!
goDeeper: gameState
    | newState response |
    newState := gameState deepCopy.
    response := OrderedCollection new.

    (gameState currentLocation = Location cave and: [(gameState hasTask: 'wreck_discovery') not]) ifTrue: [
        newState addTask: 'wreck_discovery'.
        response addAll: #('Clara: "Hey, what''s that? Do you see it?"' '').
        ^Array with: newState with: response].

    ((gameState currentLocation = Location cave or: [gameState currentLocation = Location wreck]) and: [(gameState hasTask: 'wreck_examined') not and: [(gameState hasTask: 'entered_wreck') not]] ) ifTrue: [
        response addAll: #(
            'Clara: "Hold up, doc. We can''t ignore this—it''s too weird."'
            'You should EXAMINE the WRECK.'
            ''
        ).
        ^Array with: newState with: response].

    (gameState currentLocation = Location cave or: [gameState currentLocation = Location wreck]) ifTrue: [
        gameState currentLocation = Location wreck ifTrue: [
            newState currentLocation: Location cave].
        newState currentLocation: Location tunnel.
        newState addTask: 'entered_cave'.
        newState addTask: 'act2_finished'.

        response addAll: #(
            'Slowly and carefully, you emerge from the wreckage.'
            'The dark cave corridor stretches before you.'
            'As you descend deeper into the tunnel, a roar shakes the walls as a'
            'bat-winged aircraft rockets past, vanishing toward the outside world.'
            ''
            'Clara: "That''s Nazi design—straight out of the war!"'
            'You: "I''m freaking out; let''s get out of here. I think I see light ahead."'
            ''
            'A steady glow blooms from the tunnel''s depths, pulling you forward.'
            ''
        ).
        response addAll: self epilogText.
        response add: ''.
        ^Array with: newState with: response].

    response addAll: #('You can''t go deeper from here.' '').
    ^Array with: newState with: response!

!Act2 class methodsFor: 'game step'!
step: command gameState: gameState
    | newState response result entity |
    newState := gameState deepCopy.
    response := OrderedCollection new.

    command isNext ifTrue: [
        (gameState hasTask: 'act_finished') ifTrue: [
            response add: ''.
            ^Array with: newState with: response].
        response add: 'You need to finish this act first.'.
        ^Array with: newState with: response].

    (gameState hasTask: 'act2_finished') ifTrue: [
        response add: 'You''ve already finished this act. Type "quit" to exit or "next" to advance further.'.
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
        ^self handleGo: command parameter gameState: gameState].

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
            ^self dialogClara: gameState].
        response add: 'There''s no one here by that name.'.
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

handleLook: gameState
    | newState response out |
    newState := gameState deepCopy.
    response := OrderedCollection new.

    gameState currentLocation = Location crashSite ifTrue: [
        (gameState hasTask: 'crash_site_described') ifFalse: [
            newState addTask: 'crash_site_described'.
            out := 'You wake amid the wreckage, cold seeping into your bones.
', (self describeCrashSite: newState)]
        ifTrue: [
            out := self describeCrashSite: gameState]]
    ifFalse: [
        out := gameState currentLocation = Location cockpit
            ifTrue: [self describeLocation: Location cockpit]
            ifFalse: [gameState currentLocation = Location cave
                ifTrue: [self describeCave: gameState]
                ifFalse: [gameState currentLocation = Location wreck
                    ifTrue: [self describeLocation: Location wreck]
                    ifFalse: [gameState currentLocation = Location tunnel
                        ifTrue: [self describeCave: gameState]
                        ifFalse: [self describeLocation: gameState currentLocation]]]]].

    response add: out.
    response add: ''.
    ^Array with: newState with: response!

handleGo: destination gameState: gameState
    | newState response targetLocation out |
    newState := gameState deepCopy.
    response := OrderedCollection new.

    destination asLowercase = 'deeper' ifTrue: [
        ^self goDeeper: gameState].

    targetLocation := GameUtils parseLocation: destination.
    targetLocation = Location unknown ifTrue: [
        response add: 'Unknown place: ', destination.
        response add: ''.
        ^Array with: newState with: response].

    (gameState currentLocation = Location crashSite and: [gameState hasTask: 'injured_clara']) ifTrue: [
        response add: 'I need to help Clara first.'.
        response add: ''.
        ^Array with: newState with: response].

    (gameState currentLocation = Location crashSite and: [(gameState entitiesAt: Location compartment) notEmpty]) ifTrue: [
        response add: 'I should check the luggage COMPARTMENT for the rest of the supplies first.'.
        response add: ''.
        ^Array with: newState with: response].

    (self canMove: gameState currentLocation to: targetLocation) ifTrue: [
        newState currentLocation: targetLocation.

        targetLocation = Location crashSite ifTrue: [
            out := 'You step toward the entrance, driven by cold and curiosity.', (self describeCrashSite: newState)]
        ifFalse: [targetLocation = Location cave ifTrue: [
            out := self describeCave: newState]
        ifFalse: [targetLocation = Location wreck ifTrue: [
            out := 'You and Clara enter the wreck through its hatch.', (self describeLocation: targetLocation).
            newState addTask: 'wreck_discovery'.
            newState addTask: 'wreck_examined'.
        ]
        ifFalse: [
            out := self describeLocation: targetLocation]]].

        response add: out.
        response add: ''.
        ^Array with: newState with: response]
    ifFalse: [
        response add: 'You can''t go to ', destination, ' from here.'.
        response add: ''.
        ^Array with: newState with: response]!

handleTake: itemName gameState: gameState
    | newState response entity lowerName compartmentEntity |
    newState := gameState deepCopy.
    response := OrderedCollection new.
    lowerName := itemName asLowercase.

    (gameState currentLocation = Location crashSite and: [(gameState hasTask: 'injured_clara') and: [lowerName ~= 'medkit']] ) ifTrue: [
        response add: 'I need to help Clara first.'.
        response add: ''.
        ^Array with: newState with: response].

    (gameState currentLocation = Location crashSite and: [(gameState hasTask: 'plane_examined') not]) ifTrue: [
        response add: 'I don''t know where the supplies are.'.
        response add: ''.
        ^Array with: newState with: response].

    (gameState currentLocation = Location crashSite and: [(gameState hasTask: 'compartment_checked') not]) ifTrue: [
        response add: 'I should check the luggage COMPARTMENT.'.
        response add: ''.
        ^Array with: newState with: response].

    (gameState isInInventory: lowerName) ifTrue: [
        response add: 'You''re already holding it!'.
        response add: ''.
        ^Array with: newState with: response].

    entity := gameState findHere: lowerName.
    entity ifNil: [
        "Check compartment too"
        compartmentEntity := (gameState entitiesAt: Location compartment)
            detect: [:item | item entityName asLowercase = lowerName]
            ifNone: [nil].
        compartmentEntity ifNil: [
            response add: 'I don''t see ', itemName, ' here.'.
            response add: ''.
            ^Array with: newState with: response].
        entity := compartmentEntity].

    lowerName = 'pistol' ifTrue: [
        newState addToInventory: entity.
        "Remove from compartment if it was there"
        entity = compartmentEntity ifTrue: [
            | compartmentEntities |
            compartmentEntities := gameState entitiesAt: Location compartment.
            compartmentEntities remove: entity.
            newState locationEntities at: Location compartment put: compartmentEntities].
        response add: 'You take the PISTOL - hopefully it won''t be needed.'.
        response add: ''.
        ^Array with: newState with: response].

    lowerName = 'medkit' ifTrue: [
        newState addToInventory: entity.
        "Remove from compartment if it was there"
        entity = compartmentEntity ifTrue: [
            | compartmentEntities |
            compartmentEntities := gameState entitiesAt: Location compartment.
            compartmentEntities remove: entity.
            newState locationEntities at: Location compartment put: compartmentEntities].
        response add: 'Thank God I took it.'.
        response add: ''.
        ^Array with: newState with: response].

    (gameState isSupply: lowerName) ifTrue: [
        (gameState countSupplies >= 5) ifTrue: [
            response add: 'You cannot take this - you''ve reached the limit (5 items).'.
            response add: ''.
            ^Array with: newState with: response].
        newState addToInventory: entity.
        "Remove from compartment if it was there"
        entity = compartmentEntity ifTrue: [
            | compartmentEntities |
            compartmentEntities := gameState entitiesAt: Location compartment.
            compartmentEntities remove: entity.
            newState locationEntities at: Location compartment put: compartmentEntities].
        response add: 'You take the ', entity entityName, '.'.
        response add: ''.
        ^Array with: newState with: response].

    response add: 'You can''t take that.'.
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

    (gameState isSupply: lowerName) ifTrue: [
        response add: 'You drop the ', entity entityName, '.']
    ifFalse: [
        response add: 'OK.'].
    response add: ''.
    ^Array with: newState with: response!

handleUse: itemName gameState: gameState
    | newState response lowerName |
    newState := gameState deepCopy.
    response := OrderedCollection new.
    lowerName := itemName asLowercase.

    (lowerName = 'radio' and: [(gameState currentLocation = Location cockpit) and: [(gameState hasTask: 'radio_used') not]]) ifTrue: [
        newState addTask: 'radio_used'.
        newState addTask: 'awaiting_radio_choice'.
        response addAll: #(
            'You playfully switch frequencies.'
            'Clara: "What is it, doc? Are you bored?"'
            'You: "Kind of."'
            ''
            'After a while, you run into something. The radio spits static until a garbled voice breaks through.'
            ''
            'Clara: "Wait, what? I think I hear German, but the audio is too distorted; I can''t make out the words."'
            'Your choices:'
            '1. "Nah, you''re freaking out; that''s just some usual anomalies. Focus on piloting."'
            '2. "Oh, you know German? I should have guessed from your surname."'
            ''
        ).
        ^Array with: newState with: response].

    (lowerName = 'radio' and: [(gameState hasItem: 'radio') and: [(gameState hasTask: 'entered_cave') not and: [(gameState hasTask: 'injured_clara') not]]]  ) ifTrue: [
        response addAll: #(
            'No signal to base, but switching channels catches German again:'
            '"Herr [distortion], wann ist die Glocke fertig? Ich denke, wir [distortion] die Arbeit nachste Woche been..."'
            'can be heard between the static noise.'
            ''
        ).
        ^Array with: newState with: response].

    (lowerName = 'medkit' and: [(gameState currentLocation = Location crashSite) and: [gameState hasTask: 'injured_clara']] ) ifTrue: [
        | medkit compartmentEntities |
        newState removeTask: 'injured_clara'.
        medkit := gameState findEntity: 'medkit'.
        medkit ifNotNil: [newState removeFromInventory: medkit].
        "Also remove from compartment"
        compartmentEntities := gameState entitiesAt: Location compartment.
        compartmentEntities := compartmentEntities reject: [:item | item entityName asLowercase = 'medkit'].
        newState locationEntities at: Location compartment put: compartmentEntities.
        newState removeTask: 'compartment_checked'.

        response addAll: #(
            'You bandage Clara''s wounds; she stirs awake.'
            'Clara (mumbling): "...what happened? Where are we?"'
            'You: "Thank God, you''re alive. We crashed, and you''re injured, but I think you''ll be okay."'
            'Clara: "Thanks, doc… I thought I was a goner."'
            ''
        ).
        ^Array with: newState with: response].

    (gameState isInInventory: lowerName) ifTrue: [
        response add: 'I can''t use that right now.'.
        response add: ''.
        ^Array with: newState with: response].

    response add: 'I don''t have it or I can''t use it.'.
    response add: ''.
    ^Array with: newState with: response!

!Act2 class methodsFor: 'main game loop'!
gameLoop: initialState
    | state command result newState output line |
    state := initialState.
    [true] whileTrue: [
        line := GameUtils readCommand.

        "Handle choice inputs"
        (state hasTask: 'awaiting_cockpit_choice') ifTrue: [
            result := self processChoice: line gameState: state choiceType: 'cockpit'.
            newState := result first.
            output := result second.
            GameUtils printLines: output.
            state := newState.
            ] ifFalse: [
        (state hasTask: 'awaiting_radio_choice') ifTrue: [
            result := self processChoice: line gameState: state choiceType: 'radio'.
            newState := result first.
            output := result second.
            GameUtils printLines: output.
            state := newState.
            ] ifFalse: [
        (state hasTask: 'awaiting_radio_background') ifTrue: [
            result := self processChoice: line gameState: state choiceType: 'radio_background'.
            newState := result first.
            output := result second.
            GameUtils printLines: output.
            state := newState.
            ] ifFalse: [

        "Handle regular commands"
        command := GameUtils parseCommand: line.

        command isTalk ifTrue: [
            command parameter asLowercase = 'clara' ifTrue: [
                result := self dialogClara: state.
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
        (command isNext and: [newState hasTask: 'act2_finished']) ifTrue: [
            ^PlayerState extractFrom: newState].
        ]]].
    ]!

start: playerState
    | gameState |
    gameState := self createInitialState.
    self injectSupplies: playerState into: gameState.
    GameUtils printLines: self prologText.
    GameUtils printLines: (Array with: (self describeLocation: Location cockpit) with: '').
    ^self gameLoop: gameState! !
