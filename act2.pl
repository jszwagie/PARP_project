:- module(act2, []).

initialize_act :-
    retractall(i_am_at(_)),
    assert(i_am_at(cockpit)),

    retractall(at(lighter, _)),
    retractall(at(photo, _)),
    retractall(at(calendar, _)),
    retractall(at(canister, _)),
    retractall(at(tanks, _)),

    assert(at(diary, cockpit)),
    assert(at(radio, cockpit)),
    assert(at(clara, cockpit)),
    assert(at(plane, crash_site)),
    assert(at(compartment, crash_site)),
    assert(at(wreck_object, wreck)),
    assert(at(pistol, wreck)),
    findall(Item, (holding(Item), supply(Item)), SupplyList),
    move_supplies_to_compartment(SupplyList).

move_supplies_to_compartment([]).
move_supplies_to_compartment([Item|Rest]) :-
    retract(holding(Item)),
    assert(at(Item, compartment)),
    move_supplies_to_compartment(Rest).

/* Define locations */
location(cockpit).
location(crash_site).
location(cave_entrance).
location(wreck).
location(tunnel).

/* Define paths between locations */
path(crash_site, cave_entrance).
path(cave_entrance, crash_site).
path(cave_entrance, wreck).
path(wreck, cave_entrance).
path(cave_entrance, tunnel).
path(tunnel, cave_entrance).

/* Define items that can be picked up */
can_take(diary).
can_take(pistol).

take(pistol) :-
    i_am_at(wreck),
    at(pistol, wreck),
    retract(at(pistol, wreck)),
    assert(holding(pistol)),
    write('You take the Mauser pistol.'), nl,
    write('This might come in handy.'),
    !, nl.

take(X) :-
    supply(X),
    i_am_at(Place),
    at(X, Place),
    retract(at(X, Place)),
    assert(holding(X)),
    comment_take(X),
    !, nl.

take(X) :-
    holding(X),
    write('You''re already holding it!'),
    !, nl.

take(X) :-
    i_am_at(Place),
    at(X, Place),
    can_take(X),
    retract(at(X, Place)),
    assert(holding(X)),
    write('OK.'),
    !, nl.

take(_) :-
    write('I don''t see it here or can''t take that.'),
    nl.

/* These rules describe how to put down an object. */
drop(X) :-
    supply(X),
    holding(X),
    i_am_at(Place),
    retract(holding(X)),
    assert(at(X, Place)),
    comment_drop(X),
    !, nl.

drop(X) :-
    holding(X),
    i_am_at(Place),
    retract(holding(X)),
    assert(at(X, Place)),
    write('OK.'),
    !, nl.

drop(_) :-
    write('You aren''t holding it!'),
    nl.

/* Examine objects */
examine(diary) :-
    i_am_at(cockpit),
    write('*It is open on the coordinates*'), nl,
    write('There must be some truth in it.'),
    assert(examined(diary)),
    !, nl.

examine(radio) :-
    i_am_at(cockpit),
    write('*The radio has a frequency adjuster*'), nl,
    write('Maybe I could run into something interesting by switching frequencies.'),
    assert(examined(radio)),
    !, nl.

examine(clara) :-
    i_am_at(cockpit),
    not(crashed),
    write('Clara pilots beside you, focused on the controls.'),
    !, nl.

examine(clara_injured) :-
    i_am_at(crash_site),
    crashed,
    write('She''s unconscious, her forehead gashed, her breathing shallow. She bleeds from her leg.'),
    assert(is_injured(clara)),
    !, nl.

examine(plane) :-
    i_am_at(crash_site),
    write('The plane''s a lost cause, but the luggage COMPARTMENT is intact.'), nl,
    write('The supplies you took are probably still there.'),
    !, nl.

examine(compartment) :-
    i_am_at(crash_site),
    write('You check the plane compartment for supplies.'), nl,
    findall(X, (supply(X), at(X, compartment)), CompartmentSupplies),
    (member(medkit, CompartmentSupplies) ->
        % If medkit is in compartment, game continues
        write('In the compartment you find: '), nl,
        list_items(CompartmentSupplies)
    ;
        game_over_no_medkit
    ),
    !, nl.

examine(wreck) :-
    i_am_at(wreck),
    write('A disk-shaped craft protrudes from the ice, marked with a Nazi Balkenkreuz'), nl,
    write('and "Hergestellt in Deutschland. 1944. Danzig."'), nl,
    write('Machine gun nests bristle from its surface.'), nl,
    write('Clara: "Made in Germany. 1944. Danzig. I think that''s Nazi tech-what''s it doing here?"'),
    assert(examined(wreck)),
    !, nl.

examine(wreck) :-
    i_am_at(cave_entrance),
    write('A disk-shaped WRECK. A massive, saucer-like craft embedded in the ice,'), nl,
    write('its metallic surface scarred and dented. It looks futuristic yet ancient.'),
    assert(at(wreck_object, cave_entrance)),
    !, nl.

examine(wreck_object) :-
    i_am_at(cave_entrance),
    write('A disk-shaped craft protrudes from the ice, marked with a Nazi Balkenkreuz'), nl,
    write('and "Hergestellt in Deutschland. 1944. Danzig."'), nl,
    write('Machine gun nests bristle from its surface.'), nl,
    write('Clara: "Made in Germany. 1944. Danzig. I think that''s Nazi tech-what''s it doing here?"'),
    !, nl.

examine(pistol) :-
    i_am_at(wreck),
    write('An old German Mauser C96 pistol. Still looks functional.'),
    !, nl.

examine(_) :-
    write('There''s nothing special about it.'),
    !, nl.

/* Talk to people */
talk(clara) :-
    i_am_at(cockpit),
    not(crashed),
    not(talked(clara, cockpit)),
    write('Clara glances over: "So, doc, what''s your take? Are we on a wild goose chase,'), nl,
    write('or is there really something out here in this frozen wasteland?"'), nl,
    write('Your choices:'), nl,
    write('1. "Byrd''s diary points to 70S, 10E. The coordinates are too specific to be nothing."'), nl,
    write('2. "I don''t know, but the weather''s turning ugly. We need to stay sharp."'), nl,
    write('3. "Whether it''s real or not, the mission''s worth it for the discovery alone."'), nl,
    read(Choice),
    process_clara_cockpit_talk(Choice),
    assert(talked(clara, cockpit)),
    !, nl.

talk(clara) :-
    i_am_at(cockpit),
    examined(radio),
    radio_used,
    not(crashed),
    write('You: "Is everything okay?"'), nl,
    write('Clara: "I don''t know; the compass and the altimeter suddenly started going crazy,'), nl,
    write('but we''re close to our destination, so it shouldn''t be a problem-"'), nl,
    write('Suddenly, turbulence slams the plane. Lights flicker, instruments fail, and the engines choke.'), nl,
    write('Clara (shouting): "Brace yourself! Everything''s shutting down!"'), nl,
    write('You (screaming in panic): "Ahh, what''s happening!?"'), nl,
    write('The plane spirals down, crashing into the ice. Darkness falls.'), nl,
    assert(crashed),
    retract(i_am_at(cockpit)),
    assert(i_am_at(crash_site)),
    !, nl.

talk(clara) :-
    i_am_at(cockpit),
    radio_used,
    not(talked(clara, german_radio)),
    write('You: "Wait, what? I think I hear German, but the audio is too distorted;'), nl,
    write('I can''t make out the words."'), nl,
    write('Your choices:'), nl,
    write('1. "Nah, you''re freaking out; that''s just some usual anomalies. Focus on piloting."'), nl,
    write('2. "Oh, you know German? I should have guessed from your surname."'), nl,
    read(Choice),
    process_clara_german_talk(Choice),
    assert(talked(clara, german_radio)),
    !, nl.

talk(clara_injured) :-
    i_am_at(crash_site),
    crashed,
    is_injured(clara),
    write('She''s unconscious and needs medical attention urgently.'),
    !, nl.

talk(clara) :-
    i_am_at(crash_site),
    holding(medkit),
    is_injured(clara),
    retract(is_injured(clara)),
    retract(at(clara_injured, crash_site)),
    assert(at(clara, crash_site)),
    write('Clara (mumbling): "...what happened? Where are we?"'), nl,
    write('You: "Thank God, you''re alive. We crashed, and you''re injured, but I think you''ll be okay."'), nl,
    write('Clara: "Thanks, doc... I thought I was a goner there."'),
    !, nl.

talk(clara) :-
    i_am_at(cave_entrance),
    at(clara, crash_site),
    retract(at(clara, crash_site)),
    assert(at(clara, cave_entrance)),
    write('Clara has followed you to the cave entrance.'),
    !, nl.

talk(clara) :-
    i_am_at(cave_entrance),
    at(clara, cave_entrance),
    not(talked(clara, cave_advice)),
    write('Clara: "We can''t stay exposed out here. That CAVE might be our only shot,'), nl,
    write('but it''s giving me a bad feeling. We must GO now, before it gets dark."'),
    assert(talked(clara, cave_advice)),
    !, nl.

talk(clara) :-
    i_am_at(wreck),
    at(clara, cave_entrance),
    retract(at(clara, cave_entrance)),
    assert(at(clara, wreck)),
    write('Clara has followed you to the wreck.'),
    !, nl.

talk(clara) :-
    i_am_at(wreck),
    at(clara, wreck),
    not(talked(clara, wreck_chat)),
    write('You: "This is incredible. A Nazi flying saucer?"'), nl,
    write('Clara: "Looks like it. But how did it get here? And why?"'), nl,
    write('You: "Maybe they were experimenting with advanced technology in Antarctica."'), nl,
    write('Clara: "Or maybe they found something here. Either way, it''s creepy."'), nl,
    write('Clara: "Do you think we should try to get inside it or don''t risk and GO DEEPER?"'),
    assert(talked(clara, wreck_chat)),
    !, nl.

talk(clara) :-
    i_am_at(cave_entrance),
    not(entered_wreck),
    write('Clara: "Hold up, doc. We can''t ignore this-it''s too weird."'), nl,
    write('You should EXAMINE the WRECK.'),
    !, nl.

talk(clara) :-
    i_am_at(tunnel),
    (at(clara, wreck) -> retract(at(clara, wreck)); retract(at(clara, cave_entrance))),
    assert(at(clara, tunnel)),
    write('Clara has followed you deeper into the tunnel.'),
    !, nl.

talk(clara) :-
    i_am_at(tunnel),
    at(clara, tunnel),
    seen_aircraft,
    write('Clara: "That''s Nazi design-straight out of the war!"'), nl,
    write('You: "I''m freaking out; let''s get out of here. I think I see light ahead."'), nl,
    write('A steady glow blooms from the tunnel''s depths, pulling you forward.'), nl,
    act_end,
    !, nl.

talk(_) :-
    write('There''s no one here to talk to.'),
    nl.

/* Process dialog choices */
process_clara_cockpit_talk(1) :-
    write('You: "Byrd''s diary points to 70S, 10E. The coordinates are too specific to be nothing."'), nl,
    write('Clara: "Specific or not, Antarctica''s a maze. Let''s hope those coordinates don''t lead us straight into trouble."'),
    !, nl.

process_clara_cockpit_talk(2) :-
    write('You: "I don''t know, but the weather''s turning ugly. We need to stay sharp."'), nl,
    write('Clara: "Yeah, I feel it too. This storm''s got teeth. Keep your eyes peeled."'),
    !, nl.

process_clara_cockpit_talk(3) :-
    write('You: "Whether it''s real or not, the mission''s worth it for the discovery alone."'), nl,
    write('Clara: "Discovery''s great until the ice swallows us whole. Still, I like your optimism."'),
    !, nl.

process_clara_german_talk(1) :-
    write('You: "Nah, you''re freaking out; that''s just some usual anomalies. Focus on piloting."'), nl,
    write('Clara: "Yeah, you''re right; there''s no time for that."'),
    !, nl.

process_clara_german_talk(2) :-
    write('You: "Oh, you know German? I should have guessed from your surname."'), nl,
    write('Clara: "Yes, my father was a German immigrant. He went to the USA when WWI started."'), nl,
    write('Your choices:'), nl,
    write('1. "Byrd''s diary doesn''t mention Germans, but hey, we''re in what Nazi Germany claimed as their territory in Antarctica."'), nl,
    write('2. "Ah, Uncle Sam, a shelter for all the world''s people in need."'), nl,
    read(Choice),
    process_clara_german_background(Choice),
    !, nl.

process_clara_german_background(1) :-
    write('You: "Byrd''s diary doesn''t mention Germans, but hey, we''re in what Nazi Germany claimed as their territory in Antarctica."'), nl,
    write('Clara: "I''m sure the last thing we want is for my German to come in handy. This whole mission feels unreal and ridiculous."'),
    !, nl.

process_clara_german_background(2) :-
    write('You: "Ah, Uncle Sam, a shelter for all the world''s people in need."'), nl,
    write('Clara: "Until he sends you on a mission like this, haha."'),
    !, nl.

/* Special actions */
use(radio) :-
    i_am_at(cockpit),
    write('You playfully switch frequencies.'), nl,
    write('Clara: "What is it, doc? Are you bored?"'), nl,
    write('You: "Kind of."'), nl,
    write('After a while, you run into something. The radio spits static until a garbled voice breaks through.'), nl,
    write('Clara: "Wait, what? I think I hear German, but the audio is too distorted; I can''t make out the words."'),
    assert(radio_used),
    !, nl.

use(radio) :-
    holding(radio),
    at(clara, crash_site),
    write('No signal to base, but switching channels catches German again:'), nl,
    write('"Herr [distortion], wann ist die Glocke fertig? Ich denke, wir [distortion] die Arbeit nachste Woche been..."'), nl,
    write('can be heard between the static noise.'),
    assert(talked(radio, german)),
    !, nl.

use(medkit) :-
    i_am_at(crash_site),
    holding(medkit),
    is_injured(clara),
    write('You bandage her wound, and she stirs awake.'), nl,
    write('Clara (mumbling): "...what happened? Where are we?"'), nl,
    write('You: "Thank God, you''re alive. We crashed, and you''re injured, but I think you''ll be okay."'), nl,
    write('Clara: "Thanks, doc... I thought I was a goner there."'),
    retract(is_injured(clara)),
    retract(at(clara_injured, crash_site)),
    assert(at(clara, crash_site)),
    !, nl.

use(geiger) :-
    i_am_at(wreck),
    holding(geiger),
    write('It ticks softly near the wreck.'), nl,
    write('You: "Low radiation. Could''ve been nuclear-powered."'), nl,
    write('Clara: "That''s odd, from what I know nazis never discovered nuclear energy"'),
    !, nl.

use(_) :-
    write('You can''t use that right now.'),
    !, nl.

/* Movement between locations */
go(cockpit) :-
    write('You can''t go back to the cockpit; the plane has crashed.'),
    !, nl.

go(cave) :-
    i_am_at(crash_site),
    at(clara, crash_site),
    retract(i_am_at(crash_site)),
    assert(i_am_at(cave_entrance)),
    write('You step toward the entrance, driven by cold and curiosity.'), nl,
    write('The cave twists downward, its walls polished and warm. A hum vibrates the air,'), nl,
    write('and strange discoveries await. Smooth, spiraling walls funnel you deeper.'), nl,
    write('The air warms as you go, and faint lights pulse below.'),
    !, nl.

go(wreck) :-
    i_am_at(cave_entrance),
    retract(i_am_at(cave_entrance)),
    assert(i_am_at(wreck)),
    assert(entered_wreck),
    write('You and Clara enter the wreck through its hatch.'), nl,
    write('The interior is cramped and dark, with control panels covered in dust and frost.'), nl,
    write('Wires hang loosely, and a faint smell of oil lingers.'), nl,
    write('On a seat, you spot an old German PISTOL-a Mauser C96-still holstered.'),
    !, nl.

go(deeper) :-
    (i_am_at(cave_entrance); i_am_at(wreck)),
    not(examined(wreck)),
    not(entered_wreck),
    write('Clara: "Hold up, doc. We can''t ignore this-it''s too weird."'),
    !, nl.

go(deeper) :-
    (i_am_at(cave_entrance); i_am_at(wreck)),
    retract(i_am_at(_)),
    assert(i_am_at(tunnel)),
    write('As you descend deeper into the tunnel, a roar shakes the walls as a bat-winged aircraft'), nl,
    write('rockets past, vanishing higher, toward the outside world.'),
    assert(seen_aircraft),
    !, nl.

go(Place) :-
    i_am_at(Here),
    path(Here, Place),
    retract(i_am_at(Here)),
    assert(i_am_at(Place)),
    !, look.

go(_) :-
    write('You can''t go there from here.'),
    !, nl.

/* This rule tells how to look about you. */
look :-
    i_am_at(Place),
    describe(Place),
    !, nl.

/* Hint system */
hint :-
    i_am_at(cockpit),
    not(examined(radio)),
    write('I need to find something to pass the time.'),
    !, nl.

hint :-
    i_am_at(cockpit),
    examined(radio),
    not(radio_used),
    write('I could USE the radio to pass some time.'),
    !, nl.

hint :-
    i_am_at(cockpit),
    radio_used,
    not(crashed),
    write('I should talk to Clara.'),
    !, nl.

hint :-
    i_am_at(crash_site),
    crashed,
    is_injured(clara),
    write('Clara needs help fast, and the wreckage of the PLANE might have something useful.'),
    !, nl.

hint :-
    i_am_at(crash_site),
    crashed,
    not(holding(medkit)),
    at(clara_injured, crash_site),
    write('Clara needs help fast, and a MEDKIT should be in the PLANE.'),
    !, nl.

hint :-
    i_am_at(crash_site),
    at(clara, crash_site),
    write('I should check the luggage COMPARTMENT for the rest of the supplies.'),
    !, nl.

hint :-
    i_am_at(crash_site),
    at(clara, crash_site),
    examined(compartment),
    write('I should talk to Clara about our next move.'),
    !, nl.

hint :-
    i_am_at(cave_entrance),
    not(talked(clara, cave_advice)),
    write('I should talk to Clara.'),
    !, nl.

hint :-
    i_am_at(cave_entrance),
    talked(clara, cave_advice),
    not(entered_wreck),
    write('I must decide, should I GO to WRECK or GO DEEPER?'),
    !, nl.

hint :-
    i_am_at(wreck),
    not(holding(pistol)),
    write('I should look around for anything useful.'),
    !, nl.

hint :-
    i_am_at(wreck),
    holding(pistol),
    write('I should talk to Clara.'),
    !, nl.

hint :-
    i_am_at(cave_entrance),
    talked(radio, german),
    write('I should talk to Clara about that.'),
    !, nl.

hint :-
    write('I should try to DESCRIBE my surroundings to get my bearings.'),
    !, nl.

/* Describe locations */
describe(cockpit) :-
    assert(described(cockpit)),
    write('The cockpit is tight and utilitarian, filled with glowing dials and humming switches.'), nl,
    write('Through the windshield, Antarctica''s endless snow glitters under a gray sky.'), nl,
    write('Turbulence occasionally rocks the plane, rattling the controls.'), nl,
    write('Before the control panel, next to you, on the pilot''s seat is CLARA, bravely piloting the plane.'), nl,
    write('On the panel sits the RADIO, and Byrd''s DIARY rests on your lap.'),
    !, nl.

describe(crash_site) :-
    not(described(crash_site)),
    assert(described(crash_site)),
    write('You wake amid the wreckage, cold seeping into your bones.'), nl,
    write('The PLANE is a ruin, and CLARA lies injured nearby.'), nl,
    write('Twisted metal juts from the snow, half-burying the PLANE; engine debris burns nearby.'), nl,
    write('Wind howls, stinging your face with ice.'), nl,
    write('CLARA slumps a few feet away, blood staining the snow beneath her head.'),
    assert(at(clara_injured, crash_site)),
    !, nl.

describe(crash_site) :-
    described(crash_site),
    write('The wreckage of your plane lies scattered across the frozen landscape.'), nl,
    (at(clara, crash_site) ->
        write('Clara stands nearby, looking shaken but determined.') ;
        write('This place offers no shelter from the biting Antarctic cold.')
    ), nl,
    write('The crash site aligns with Byrd''s coordinates.'), nl,
    write('Nearby, a vast CAVE gapes in the ice, its edges too smooth to be natural.'),
    !, nl.

describe(cave_entrance) :-
    not(described(cave_entrance)),
    assert(described(cave_entrance)),
    write('The cave looms like a hangar carved into the ice, its perfect edges eerie and artificial.'), nl,
    write('Snow whips around, but the opening promises shelter.'), nl,
    write('On the right side of the tunnel, you see a disk-shaped WRECK.'),
    !, nl.

describe(cave_entrance) :-
    described(cave_entrance),
    write('The entrance to the ice cave stretches before you.'), nl,
    write('The unnatural smoothness of the walls suggests intelligent design.'), nl,
    write('To the right, the Nazi flying saucer WRECK remains embedded in the ice.'), nl,
    write('A path leads DEEPER into the tunnel, where faint blue light pulses.'),
    !, nl.

describe(wreck) :-
    not(described(wreck)),
    assert(described(wreck)),
    write('The interior is cramped and dark, with control panels covered in dust and frost.'), nl,
    write('Wires hang loosely, and a faint smell of oil lingers.'), nl,
    write('On a seat, you spot an old German PISTOL-a Mauser C96-still holstered.'),
    !, nl.

describe(wreck) :-
    described(wreck),
    write('The Nazi flying saucer''s interior is a testament to advanced engineering for its time.'), nl,
    write('Control panels with unfamiliar designs line the walls.'), nl,
    write('A path leads back to the CAVE entrance or you could go DEEPER into the tunnel.'),
    !, nl.

describe(tunnel) :-
    not(seen_aircraft),
    assert(seen_aircraft),
    write('As you descend deeper into the tunnel, a roar shakes the walls as a bat-winged aircraft'), nl,
    write('rockets past, vanishing higher, toward the outside world.'),
    !, nl.

describe(tunnel) :-
    seen_aircraft,
    write('The tunnel descends at a gentle slope, its walls unnaturally smooth.'), nl,
    write('A strange blue glow emanates from somewhere ahead.'), nl,
    write('The air here is warmer, almost comfortable compared to the Antarctic chill outside.'),
    !, nl.


/* Handle tell commands for dialog choices */
tell(Choice) :-
    i_am_at(cockpit),
    talked(clara, cockpit),
    process_clara_cockpit_talk(Choice),
    !, nl.

tell(Choice) :-
    i_am_at(cockpit),
    radio_used,
    talked(clara, german_radio),
    process_clara_german_talk(Choice),
    !, nl.

tell(Choice) :-
    process_clara_german_background(Choice),
    !, nl.

/* Act 2 end and initialization */

game_over_no_medkit :-
    write('You realize you forgot to take the MEDKIT before departure.'), nl,
    write('How could I forget it? What can I do now?'), nl,
    write('You start to panic, gasping heavily.'), nl,
    write('Clara, Clara, wake up!'), nl,
    write('You try to rouse Clara, but it''s futile.'), nl,
    write('(crying) Clara, please, I can''t do this alone.'), nl,
    write('With her wound untreated, Clara continues to bleed. Suddenly, her heart stops beating.'), nl,
    write('As the cold overwhelms you, the Antarctic claims you both.'), nl,
    write('GAME OVER'),
    !, nl.

act_end :-
    retractall(talked(_, _)),
    nl,
    write('----------------------------ACT 2 OVER----------------------------'),
    !, nl,
    assert(finished_act(2)),
    user:check_progress.

start_act :-
    initialize_act,
    intro.

intro :-
    nl, nl, write('ACT 2: DESCENT INTO THE UNKNOWN'), nl, nl,
    write('You are sitting in the co-pilot chair of a Douglas A-20 Havoc, soaring over'), nl,
    write('Antarctica''s icy expanse. Clara pilots beside you, and you hold Admiral Byrd''s diary.'),
    look.