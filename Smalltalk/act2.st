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