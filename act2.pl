:- module(act2, []).

initialize_act :-
    retractall(i_am_at(_)),
    assert(i_am_at(cockpit)),

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

entered_cave :-
    i_am_at(cave);
    i_am_at(wreck);
    i_am_at(tunnel).

/* Define locations */
location(cockpit).
location(crash_site).
location(cave).
location(wreck).
location(tunnel).

/* Define paths between locations */
path(crash_site, cave).
path(cave, crash_site).
path(cave, wreck).
path(wreck, cave).
path(cave, tunnel).
path(tunnel, cave).

/* Define items that can be picked up */
can_take(pistol).

accessible(Item, Place) :-
    at(Item, Place).

accessible(Item, Place) :-
    i_am_at(Place),
    at(Item, compartment),
    accessible(compartment, Place).

take(_) :-
    finished_act(2),
    write('You''ve already finished this act. Enter "next." to proceed or "halt." to quit.'),
    !, nl.

take(radio) :-
    i_am_at(cockpit),
    write('You can''t take the radio; it''s bolted to the cockpit.'),
    !, nl.

take(diary) :-
    i_am_at(cockpit),
    write('I''ll put it in my jacket when we get there.'),
    !, nl.

take(pistol) :-
    i_am_at(wreck),
    write('I hope it won''t be needed.'), nl,
    write('*You take the pistol.*'),
    assert(holding(pistol)),
    !, nl.

take(X) :-
    supply(X),
    i_am_at(Place),
    accessible(X, Place),
    findall(Y, (holding(Y), supply(Y)), SupplyList),
    length(SupplyList, Count),
    Count >= 5,
    write('You cannot take this - you''ve reached supply limit - 5 items only.'),
    !, nl.

take(_) :-
    i_am_at(crash_site),
    not(examined(plane)),
    write('I don''t know where the supplies are.'),
    !, nl.

take(_) :-
    i_am_at(crash_site),
    not(examined(compartment)),
    write('I should check the luggage COMPARTMENT.'),
    !, nl.

take(X) :-
    supply(X),
    i_am_at(Place),
    accessible(X, Place),
    comment_take(X),
    retract(at(X, _)),
    assert(holding(X)),
    !, nl.

take(X) :-
    i_am_at(Place),
    accessible(X, Place),
    can_take(X),
    retract(at(X, _)),
    assert(holding(X)),
    write('OK.'),
    !, nl.

take(X) :-
    holding(X),
    write('You''re already holding it!'),
    !, nl.

take(_) :-
    write('I don''t see it here or can''t take that.'),
    nl.

/* These rules describe how to put down an object. */
drop(_) :-
    finished_act(2),
    write('You''ve already finished this act. Enter "next." to proceed or "halt." to quit.'),
    !, nl.

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
examine(_) :-
    finished_act(2),
    write('You''ve already finished this act. Enter "next." to proceed or "halt." to quit.'),
    !, nl.

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

examine(clara) :-
    i_am_at(crash_site),
    crashed,
    write('She''s unconscious, her forehead gashed, her breathing shallow. She bleeds from her leg.'),
    !, nl.

examine(plane) :-
    i_am_at(crash_site),
    write('The plane''s a lost cause, but the luggage COMPARTMENT is intact.'), nl,
    write('The supplies you took are probably still there.'),
    assert(examined(plane)),
    !, nl.

examine(compartment) :-
    i_am_at(crash_site),
    is_injured(clara),
    not(holding(medkit)),
    assert(examined(compartment)),
    assert(examined(plane)),
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

examine(compartment) :-
    i_am_at(crash_site),
    holding(medkit),
    at(_, compartment),
    write('You check the plane compartment for supplies.'), nl,
    findall(X, (supply(X), at(X, compartment)), CompartmentSupplies),
    write('In the compartment you find: '), nl,
    list_items(CompartmentSupplies),
    !, nl.

examine(compartment) :-
    write('The compartment is empty'),
    !, nl.

examine(wreck) :-
    i_am_at(cave),
    write('A disk-shaped craft protrudes from the ice, marked with a Nazi Balkenkreuz'), nl,
    write('and "Hergestellt in Deutschland. 1944. Danzig". Machine gun nests bristle from its surface.'), nl,
    write('Clara: "Made in Germany. 1944. Danzig. I think that''s Nazi tech-what''s it doing here?"'), nl,
    write('There appears to be an entrance. I could GO inside the wreck to investigate further.'), nl,
    assert(examined(wreck)),
    !.

examine(pistol) :-
    i_am_at(wreck),
    write('An old German Mauser C96 pistol. Still looks functional.'),
    !, nl.

examine(cave) :-
    talked(clara, cave_advice),
    write('This isn''t natural—someone or something shaped it.'),
    !, nl.

examine(_) :-
    write('There''s nothing special about it.'),
    !, nl.

/* Talk to people */
talk(_) :-
    finished_act(2),
    write('You''ve already finished this act. Enter "next." to proceed or "halt." to quit.'),
    !, nl.

talk(clara) :-
    i_am_at(cockpit),
    used(radio),
    not(crashed),
    write('You: "Is everything okay?"'), nl,
    write('Clara: "I don''t know; the compass and the altimeter suddenly started going crazy,'), nl,
    write('but we''re close to our destination, so it shouldn''t be a problem-"'), nl,
    write('Suddenly, turbulence slams the plane. Lights flicker, instruments fail, and the engines choke.'), nl,
    write('Clara (shouting): "Brace yourself! Everything''s shutting down!"'), nl,
    write('You (screaming in panic): "Ahh, what''s happening!?"'), nl,
    write('The plane spirals down, crashing into the ice. Darkness falls.'), nl,
    retract(at(clara, cockpit)),
    retract(at(radio, cockpit)),
    retract(at(diary, cockpit)),
    retract(i_am_at(cockpit)),
    assert(i_am_at(crash_site)),
    assert(crashed),
    assert(is_injured(clara)),
    assert(at(clara, crash_site)),
    !, nl.

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
    not(crashed),
    write('You: "We''re bound to find something there—I can feel it in my bones."'), nl,
    write('Clara: "Hopefully, or all our efforts will be for nothing."'),
    !, nl.

talk(clara) :-
    i_am_at(crash_site),
    crashed,
    is_injured(clara),
    not(holding(medkit)),
    write('She''s unconscious and needs medical attention urgently.'),
    !, nl.

talk(clara) :-
    is_injured(clara),
    use(medkit), % allows player to either use medkit and talk to clara later or talk to clara directly
    !, nl.

talk(clara) :-
    i_am_at(crash_site),
    retractall(talked(clara, cave_advice)),
    assert(talked(clara, cave_advice)),
    write('Clara: "We can''t stay exposed out here. That CAVE might be our only shot,'), nl,
    write('but it''s giving me a bad feeling. We must GO now, before it gets dark."'),
    !, nl.

talk(clara) :-
    i_am_at(cave),
    not(examined(wreck)),
    assert(talked(clara, wreck_discovery)),
    write('Clara: "Hey, what''s that? Do you see it?"'), nl,
    write('On the right side of the tunnel, you see a disk-shaped WRECK.'), nl,
    write('A massive, saucer-like craft embedded in the ice, its metallic surface scarred and dented.'), nl,
    write('It looks futuristic yet ancient.'),
    !, nl.

talk(clara) :-
    i_am_at(cave),
    examined(wreck),
    not(entered(wreck)),
    assert(talked(clara, wreck_discovery2)),
    write('You: "This is incredible. A Nazi flying saucer?"'), nl,
    write('Clara: "Looks like it. But how did it get here? And why?"'), nl,
    write('You: "Maybe they were experimenting with advanced technology in Antarctica."'), nl,
    write('Clara: "Or maybe they found something here. Either way, it''s creepy."'), nl,
    write('Clara: "Do you think we should try to get inside it or don''t risk and GO DEEPER?"'),
    !, nl.

talk(clara) :-
    (i_am_at(wreck); i_am_at(cave)),
    entered(wreck),
    write('Clara: "This place gives me the creeps. Look at these controls—they’re way ahead of their time."'), nl,
    write('You: "Yeah, it’s like something out of science fiction. But it’s real."'), nl,
    write('Clara: "I think we’ve seen enough. Let’s keep moving; there might be more ahead. We should GO DEEPER."'),
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
    !.

process_clara_german_background(1) :-
    write('You: "Byrd''s diary doesn''t mention Germans, but hey, we''re in what Nazi Germany claimed as their territory in Antarctica."'), nl,
    write('Clara: "I''m sure the last thing we want is for my German to come in handy. This whole mission feels unreal and ridiculous."'),
    !, nl.

process_clara_german_background(2) :-
    write('You: "Ah, Uncle Sam, a shelter for all the world''s people in need."'), nl,
    write('Clara: "Until he sends you on a mission like this, haha."'),
    !, nl.

/* Special actions */
use(_) :-
    finished_act(2),
    write('You''ve already finished this act. Enter "next." to proceed or "halt." to quit.'),
    !, nl.

use(radio) :-
    i_am_at(cockpit),
    write('You playfully switch frequencies.'), nl,
    write('Clara: "What is it, doc? Are you bored?"'), nl,
    write('You: "Kind of."'), nl,
    write('After a while, you run into something. The radio spits static until a garbled voice breaks through.'), nl,
    write('Clara: "Wait, what? I think I hear German, but the audio is too distorted; I can''t make out the words."'), nl,
    assert(used(radio)),
    write('Your choices:'), nl,
    write('1. "Nah, you''re freaking out; that''s just some usual anomalies. Focus on piloting."'), nl,
    write('2. "Oh, you know German? I should have guessed from your surname."'), nl,
    read(Choice),
    process_clara_german_talk(Choice),
    assert(talked(clara, german_radio)),
    !, nl.

use(radio) :-
    holding(radio),
    not(entered_cave),
    not(is_injured(clara)),
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
    !, nl.

use(geiger) :-
    (i_am_at(cave);i_am_at(wreck)),
    talked(clara, wreck_discovery),
    holding(geiger),
    write('It ticks softly near the wreck.'), nl,
    write('You: "Low radiation. Could''ve been nuclear-powered."'), nl,
    write('Clara: "That''s odd, from what I know nazis never discovered nuclear energy"'),
    !, nl.

use(_) :-
    write('You can''t use that right now.'),
    !, nl.

/* Movement between locations */
go(_) :-
    finished_act(2),
    write('You''ve already finished this act. Enter "next." to proceed or "halt." to quit.'),
    !, nl.

go(cockpit) :-
    crashed,
    write('You can''t go back to the cockpit; the plane has crashed.'),
    !, nl.

go(_) :-
    is_injured(clara),
    write('I need to help Clara first'),
    !, nl.

go(cave) :-
    i_am_at(crash_site),
    at(_, compartment),
    write('I should check the luggage COMPARTMENT for the rest of the supplies first.'),
    !, nl.

go(cave) :-
    i_am_at(crash_site),
    change_location(cave),
    write('You step toward the entrance, driven by cold and curiosity.'), nl,
    describe(cave),
    !, nl.

go(wreck) :-
    i_am_at(cave),
    change_location(wreck),
    assert(entered(wreck)),
    write('You and Clara enter the wreck through its hatch.'), nl,
    describe(wreck),
    !, nl.

go(deeper) :-
    i_am_at(cave),
    not(talked(clara, wreck_discovery)),
    write('Clara: "Hey, what''s that? Do you see it?"'),
    !, nl.

go(deeper) :-
    (i_am_at(cave); i_am_at(wreck)),
    not(examined(wreck)),
    not(entered(wreck)),
    write('Clara: "Hold up, doc. We can''t ignore this-it''s too weird."'), nl,
    write('You should EXAMINE the WRECK.'),
    !, nl.

go(deeper) :-
    (i_am_at(cave); i_am_at(wreck)),
    (entered(wreck) -> describe(cave) ; true),
    change_location(tunnel),
    write('As you descend deeper into the tunnel, a roar shakes the walls as a bat-winged aircraft'), nl,
    write('rockets past, vanishing higher, toward the outside world.'),
    write('Clara: "That''s Nazi design-straight out of the war!"'), nl,
    write('You: "I''m freaking out; let''s get out of here. I think I see light ahead."'), nl,
    write('A steady glow blooms from the tunnel''s depths, pulling you forward.'), nl,
    act_end,
    !, nl.

go(Place) :-
    i_am_at(Here),
    path(Here, Place),
    change_location(Place),
    !, look.

go(_) :-
    write('You can''t go there from here.'),
    !, nl.

change_location(Place) :-
    retract(i_am_at(_)),
    assert(i_am_at(Place)),
    retract(at(clara, _)),
    assert(at(clara, Place)).

look :-
    finished_act(2),
    write('You''ve already finished this act. Enter "next." to proceed or "halt." to quit.'),
    !, nl.

look :-
    i_am_at(Place),
    describe(Place),
    !, nl.

/* Hint system */
hint :-
    finished_act(2),
    write('You''ve already finished this act. Enter "next." to proceed or "halt." to quit.'),
    !, nl.

hint :-
    i_am_at(cockpit),
    not(examined(radio)),
    write('I need to find something to pass the time.'),
    !, nl.

hint :-
    i_am_at(cockpit),
    examined(radio),
    not(used(radio)),
    write('I could USE the radio to pass some time.'),
    !, nl.

hint :-
    i_am_at(cockpit),
    used(radio),
    not(crashed),
    write('I should talk to Clara.'),
    !, nl.

hint :-
    i_am_at(crash_site),
    described(crash_site),
    not(examined(plane)),
    is_injured(clara),
    crashed,
    write('Clara needs help fast, and the wreckage of the PLANE might have something useful.'),
    !, nl.

hint :-
    i_am_at(crash_site),
    crashed,
    is_injured(clara),
    examined(plane),
    write('Clara needs help fast, and a MEDKIT should be in the luggage COMPARTMENT.'),
    !, nl.

hint :-
    i_am_at(crash_site),
    not(is_injured(clara)),
    at(_, compartment),
    write('I should check the luggage COMPARTMENT for the rest of the supplies.'),
    !, nl.

hint :-
    i_am_at(crash_site),
    not(is_injured(clara)),
    write('I should talk to Clara about our next move.'),
    !, nl.

hint :-
    i_am_at(cave),
    talked(clara, wreck_discovery),
    not(examined(wreck)),
    write('I should EXAMINE the WRECK.'),
    !, nl.

hint :-
    i_am_at(cave),
    examined(wreck),
    not(entered(wreck)),
    not(talked(clara, wreck_discovery2)),
    write('I should talk to Clara.'),
    !, nl.

hint :-
    i_am_at(cave),
    talked(clara, wreck_discovery2),
    not(entered(wreck)),
    write('I must decide, should I GO to WRECK or GO DEEPER?'),
    !, nl.

hint :-
    i_am_at(cave),
    write('I should talk to Clara.'),
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
    write('I should try to LOOK around to get my bearings.'),
    !, nl.

/* Describe locations */
describe(cockpit) :-
    write('The cockpit is tight and utilitarian, filled with glowing dials and humming switches.'), nl,
    write('Through the windshield, Antarctica''s endless snow glitters under a gray sky.'), nl,
    write('Turbulence occasionally rocks the plane, rattling the controls.'), nl,
    write('Before the control panel, next to you, on the pilot''s seat is CLARA, bravely piloting the plane.'), nl,
    write('On the panel sits the RADIO, and Byrd''s DIARY rests on your lap.'),
    !, nl.

describe(crash_site) :-
    not(described(crash_site)),
    assert(described(crash_site)),
    write('You wake amid the wreckage, cold seeping into your bones.'),
    describe(crash_site),
    nl.

describe(crash_site) :-
    is_injured(clara),
    write('The PLANE is a ruin, and CLARA lies injured nearby.'), nl,
    write('Twisted metal juts from the snow, half-burying the PLANE; engine debris burns nearby.'), nl,
    write('Wind howls, stinging your face with ice.'), nl,
    write('CLARA slumps a few feet away, blood staining the snow beneath her head.'),
    !, nl.

describe(crash_site) :-
    not(is_injured(clara)),
    write('The wreckage of your plane lies scattered across the frozen landscape.'), nl,
    write('Clara stands nearby, looking shaken but determined.'), nl,
    write('This place offers no shelter from the biting Antarctic cold.'), nl,
    write('The crash site aligns with Byrd''s coordinates.'), nl,
    !.

describe(cave) :-
    entered(wreck),
    write('Slowly and carefully, you emerge from the wreckage.'), nl,
    write('The dark cave corridor stretches before you.'),
    !, nl.

describe(cave) :-
    not(talked(clara, wreck_discovery)),
    write('The cave twists downward, its walls polished and warm. A hum vibrates the air,'), nl,
    write('and strange discoveries await. Smooth, spiraling walls funnel you DEEPER.'), nl,
    write('The air warms as you go, and faint lights pulse below'), nl,
    !.

describe(cave) :-
    talked(clara, wreck_discovery),
    write('The entrance to the ice cave stretches before you.'), nl,
    write('The unnatural smoothness of the walls suggests intelligent design.'), nl,
    write('To the right, the Nazi flying saucer WRECK remains embedded in the ice.'), nl,
    write('A path leads DEEPER into the tunnel, where faint blue light pulses.'), nl,
    write('On the right side of the tunnel, you see a disk-shaped WRECK.'), nl,
    write('A massive, saucer-like craft embedded in the ice, its metallic surface scarred and dented.'), nl,
    write('It looks futuristic yet ancient.'),
    !, nl.

describe(wreck) :-
    write('The interior is cramped and dark, with control panels covered in dust and frost.'), nl,
    write('Wires hang loosely, and a faint smell of oil lingers.'), nl,
    write('On a seat, you spot an old German PISTOL-a Mauser C96-still holstered.'),
    !, nl.

/* Handle tell commands for dialog choices */
tell(Choice) :-
    i_am_at(cockpit),
    talked(clara, cockpit),
    process_clara_cockpit_talk(Choice),
    !, nl.

tell(Choice) :-
    i_am_at(cockpit),
    used(radio),
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
    nl,
    write('----------------------------ACT 2 OVER----------------------------'),
    !, nl,
    cleanup,
    asserta(user:finished_act(2)),
    user:check_progress.

cleanup :-
    retractall(talked(_, _)),
    retractall(at(_, _)),
    retractall(i_am_at(_)).

start_act :-
    initialize_act,
    intro.

intro :-
    nl, nl, write('ACT 2: DESCENT INTO THE UNKNOWN'), nl, nl,
    write('You are sitting in the co-pilot chair of a Douglas A-20 Havoc, soaring over'), nl,
    write('Antarctica''s icy expanse. Clara pilots beside you, and you hold Admiral Byrd''s diary.'), nl,
    look.
