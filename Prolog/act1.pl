:- module(act1, []).

initialize_act :-
    retractall(i_am_at(_)),
    assert(i_am_at(yard)),

    retractall(at(_, _)),
    assert(at(clara, runway)),
    assert(at(lighter, barrack)),
    assert(at(photo, barrack)),
    assert(at(calendar, barrack)),
    assert(at(plane, runway)),
    assert(at(canister, depot)),
    assert(at(tanks, runway)),
    assert(at(food, tent)),
    assert(at(water, tent)),
    assert(at(geiger, tent)),
    assert(at(medkit, tent)),
    assert(at(radio, tent)),
    assert(at(gear, tent)),
    assert(at(tools, tent)),

    assert(can_take(lighter)).

location(yard).
location(barrack).
location(runway).
location(depot).
location(tent).

/* Define paths between locations */
path(yard, barrack).
path(yard, runway).
path(yard, depot).
path(yard, tent).
path(barrack, yard).
path(runway, yard).
path(depot, yard).
path(tent, yard).

/* These rules describe how to pick up an object. */
take(_) :-
    finished_act(1),
    write('You''ve already finished this act. Enter "next." to proceed or "halt." to quit.'),
    !, nl.

take(X) :-
    supply(X),
    i_am_at(Place),
    at(X, Place),
    task(supplies),
    findall(Y, (holding(Y), supply(Y)), SupplyList),
    length(SupplyList, Count),
    Count < 5,
    retract(at(X, Place)),
    assert(holding(X)),
    comment_take(X),
    !, nl.

take(X) :-
    supply(X),
    task(supplies),
    findall(Y, (holding(Y), supply(Y)), SupplyList),
    length(SupplyList, Count),
    Count >= 5,
    write('You cannot take this - you''ve reached the limit - 5 items only.'),
    !, nl.

take(photo) :-
    write('Sorry, my love, but I can''t take you with me.'),
    !, nl.

take(calendar) :-
    write('I doubt this will be useful.'),
    !, nl.

take(lighter) :-
    write('I''m not going to smoke now.'),
    !, nl.

take(canister) :-
    not(can_take(canister)),
    i_am_at(Place),
    at(canister, Place),
    write('I should check the fuel tanks first.'),
    !, nl.

take(canister) :-
    can_take(canister),
    i_am_at(Place),
    at(canister, Place),
    retract(at(canister, Place)),
    assert(holding(canister)),
    write('This should be enough.'),
    !, nl.


take(X) :-
    holding(X),
    write('You''re already holding it!'),
    !, nl.

take(X) :-
    i_am_at(Place),
    at(X, Place),
    not(task(supplies)),
    write('I don''t need this right now. I should TALK to Clara first.'),
    !, nl.

take(_) :-
    write('I don''t see it here or can''t take that.'),
    nl.

/* Use command */
use(_) :-
    finished_act(1),
    write('You''ve already finished this act. Enter "next." to proceed or "halt." to quit.'),
    !, nl.

use(canister) :-
    holding(canister),
    i_am_at(runway),
    write('I should give it to Clara. I''ll TALK to her.'),
    !, nl.

use(X) :-
    holding(X),
    write('I can''t use that right now.'),
    !, nl.

use(_) :-
    write('I don''t have it or I can''t use it.'),
    nl.

/* These rules describe how to put down an object. */
drop(_) :-
    finished_act(1),
    write('You''ve already finished this act. Enter "next." to proceed or "halt." to quit.'),
    !, nl.

drop(lighter) :-
    holding(lighter),
    i_am_at(Place),
    retract(holding(lighter)),
    assert(at(lighter, Place)),
    write('I might need something else instead.'),
    !, nl.

drop(canister) :-
    holding(canister),
    write('I should get it to Clara. I''ll TALK to her.'),
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
    finished_act(1),
    write('You''ve already finished this act. Enter "next." to proceed or "halt." to quit.'),
    !, nl.

examine(photo) :-
    i_am_at(barrack),
    write('I''ll never forget you, my love.'),
    assert(examined(photo)),
    !, nl.

examine(lighter) :-
    i_am_at(barrack),
    write('I really should quit smoking.'),
    assert(examined(lighter)),
    !, nl.

examine(list) :-
    i_am_at(tent),
    write('- FOOD Rations'), nl,
    write('- WATER'), nl,
    write('- GEIGER Counter'), nl,
    write('- MEDKIT'), nl,
    write('- RADIO'), nl,
    write('- Climbing GEAR'), nl,
    write('- Navigation TOOLS'), nl,
    write('The plane has a capacity of only 5 items, I must choose intelligently.'),
    assert(examined(list)),
    !, nl.

examine(food) :-
    i_am_at(tent),
    write('Canned goods and dried meals.'), nl,
    write('Enough to last two weeks, but not exactly gourmet'), nl,
    assert(examined(food)),
    !, nl.

examine(water) :-
    i_am_at(tent),
    write('Canned goods and dried meals.'), nl,
    write('Enough to last two weeks, but not exactly gourmet'), nl,
    assert(examined(food)),
    !, nl.

examine(geiger) :-
    i_am_at(tent),
    write('A standard radiation detector.'), nl,
    write('If we stumble upon something unnatural, this could be crucial.'), nl,
    assert(examined(food)),
    !, nl.

examine(medkit) :-
    i_am_at(tent),
    write('Bandages, antiseptic, morphine...'), nl,
    write('Everything needed for basic field medical care.'), nl,
    assert(examined(food)),
    !, nl.

examine(radio) :-
    i_am_at(tent),
    write('A shortwave field radio.'), nl,
    write('Not the best range, but it should work if we''re within contact distance of the base.'), nl,
    assert(examined(food)),
    !, nl.

examine(gear) :-
    i_am_at(tent),
    write('Ropes, pitons, carabiners.'), nl,
    write('If we need to descend into something deep or climb out of trouble, this will help.'), nl,
    assert(examined(food)),
    !, nl.

examine(tools) :-
    i_am_at(tent),
    write('A compass, maps, and a sextant.'), nl,
    write('Old-school but reliable.'), nl,
    assert(examined(food)),
    !, nl.

examine(calendar) :-
    i_am_at(barrack),
    write('August 26, 1946'),
    assert(examined(calendar)),
    !, nl.

examine(plane) :-
    i_am_at(runway),
    write('Your type served well in the war.'),
    assert(examined(plane)),
    !, nl.

examine(tanks) :-
    i_am_at(runway),
    talked(clara, fuel_request),
    write('You crouch beside the aircraft and open the fuel hatch.'), nl,
    write('We''re running low. We need at least one more drum of fuel.'), nl,
    write('Clara: "Told you. Go grab one from the depot."'),
    assert(examined(tanks)),
    assert(can_take(canister)),
    !, nl.

examine(canister) :-
    i_am_at(Place),
    at(canister, Place),
    not(can_take(canister)),
    write('I should check the fuel tanks first.'),
    !, nl.

examine(canister) :-
    i_am_at(depot),
    write('Heavy, but necessary.'),
    assert(examined(canister)),
    !, nl.

examine(clara) :-
    i_am_at(runway),
    write('Clara stands near the plane, wearing a military pilot''s uniform with rolled-up sleeves.'), nl,
    write('Her dark hair is tied back, with a few strands escaping to frame her sharp eyes.'),
    !, nl.

examine(clara) :-
    write('I can''t see her clearly from here.'),
    !, nl.

examine(runway) :-
    (i_am_at(runway); i_am_at(yard)),
    write('The runway is a makeshift strip of concrete slabs, cleared of snow.'),
    !, nl.

examine(depot) :-
    (i_am_at(depot); i_am_at(yard)),
    write('The depot is a simple structure, but it keeps the fuel canisters safe from the cold.'),
    !, nl.

examine(tent) :-
    (i_am_at(tent); i_am_at(yard)),
    write('The tent is cramped but well-stocked with supplies.'),
    !, nl.

examine(barrack) :-
    (i_am_at(barrack); i_am_at(yard)),
    write('Your resting place during the mission.'),
    !, nl.

examine(yard) :-
    write('The yard is covered in snow.'),
    !, nl.

examine(X) :-
    holding(X),
    write('I''m holding it.'),
    !, nl.

examine(_) :-
    write('I can''t see it here or there''s nothing special about it.'),
    !, nl.

/* Talk to people */
talk(_) :-
    finished_act(1),
    write('You''ve already finished this act. Enter "next." to proceed or "halt." to quit.'),
    !, nl.

talk(clara) :-
    not(i_am_at(runway)),
    write('I''m not going to shout; I should go to her.'),
    !, nl.

talk(clara) :-
    i_am_at(runway),
    not(talked(clara, fuel_request)),
    not(task(fuel)),
    write('Clara: "Morning, doc. We need to get this bird fueled up. Could you check the TANKS for me?"'), nl,
    write('Your choices:'), nl,
    write('1. "Okay, I''ll handle it now."'), nl,
    write('2. "I think it''s good enough, but I could double-check."'), nl,
    write('3. "Why don''t you take care of it?"'), nl,
    read(Choice), nl,
    process_clara_fuel_talk(Choice).

talk(clara) :-
    i_am_at(runway),
    holding(canister),
    task(fuel),
    retract(holding(canister)),
    write('You: "I have it!"'), nl,
    write('Clara: "Nice, hand it over - our bird''s thirsty."'), nl,
    write('*starts fueling the plane*'), nl,
    write('Clara: "Why don''t you gather some supplies while I finish fueling?"'), nl,
    write('Your choices:'), nl,
    write('1. "On my way."'), nl,
    write('2. "Are you sure you can handle it by yourself?"'), nl,
    read(Choice), nl,
    process_clara_supplies_talk(Choice).

talk(clara) :-
    i_am_at(runway),
    task(supplies),
    findall(Y, (holding(Y), supply(Y)), SupplyList),
    length(SupplyList, Count),
    Count > 0,
    retractall(task(_)),
    write('You: "Thank you!"'), nl,
    write('*a moment of silence*'), nl,
    write('Clara: "So, tell me again why we''re risking our necks for this?'), nl,
    write('A diary from some explorer doesn''t scream ''top priority'' to me."'), nl,
    write('Your choices:'), nl,
    write('1."Because it could be the discovery of the century."'), nl,
    write('2."Orders are orders. The government wants answers."'), nl,
    write('3."I''ve got a feeling there''s something big waiting for us."'), nl,
    read(Choice), nl,
    process_clara_explain(Choice).

talk(clara) :-
    i_am_at(runway),
    task(supplies),
    findall(Y, (holding(Y), supply(Y)), SupplyList),
    length(SupplyList, Count),
    not(Count > 0),
    write('Clara: "What are you waiting for? Go grab some supplies from the TENT."'), nl,
    write('You have to grab at least one item.'),
    !, nl.

talk(clara) :-
    i_am_at(runway),
    task(fuel),
    not(holding(canister)),
    write('Clara: "What are you waiting for? Go grab a fuel CANISTER from the DEPOT."'),
    !, nl.

talk(_) :-
    write('There''s no one here to talk to or I can''t talk to it.'),
    nl.

/* Process dialog choices */
process_clara_fuel_talk(1) :-
    write('You: "Okay, I''ll handle it now."'),
    retractall(talked(clara, _)),
    assert(talked(clara, fuel_request)),
    assert(task(fuel)),
    nl.

process_clara_fuel_talk(2) :-
    write('You: "I think it''s good enough, but I could double-check."'), nl,
    write('Clara: "Good enough doesn''t cut it out here. Antarctica doesn''t forgive mistakes. Check it properly."'),
    retractall(talked(clara, _)),
    assert(talked(clara, fuel_request)),
    assert(task(fuel)),
    nl.

process_clara_fuel_talk(3) :-
    write('You: "Why don''t you take care of it?"'), nl,
    write('Clara: (frowning) "Oh, you''re lazy, aren''t you? Fine, I''ll handle it after I finish checking the oil,'), nl,
    write('but you''re not off the hook, doc. Go gather mandatory supplies and drop them near the plane."'), nl,
    retractall(task(_)),
    assert(talked(clara, fuel_request)),
    assert(task(supplies)),
    !, nl.

process_clara_supplies_talk(1) :-
    write('You: "On my way."'),
    retractall(task(_)),
    assert(task(supplies)),
    !, nl.

process_clara_supplies_talk(2) :-
    write('You: "Are you sure you can handle it by yourself?"'), nl,
    write('Clara: "Don''t worry, doc - I''m not a kid. Go grab those supplies."'),
    retractall(task(_)),
    assert(task(supplies)),
    !, nl.

process_clara_explain(1) :-
    write('You: "Because it could be the discovery of the century."'), nl,
    write('Clara: "Discovery of the century? I hope it''s not just a pile of ice and a frostbite bill."'), nl,
    retractall(task(_)),
    write("Your choice:"), nl,
    write("1. Byrd wasn't a dreamer. Those coordinates mean something."), nl,
    write("2. Even if it's nothing, the science alone is worth it."), nl,
    read(Choice), nl,
    process_further_explain(Choice).

process_clara_explain(2) :-
    write('You: "Orders are orders. The government wants answers."'), nl,
    write('Clara: "Yeah, and Uncle Sam loves sending us into the freezer for kicks. What''s their angle?"'), nl,
    write('You: "Cold War jitters, probably. They don''t want the Soviets sniffing around first."'), nl,
    retractall(task(_)),
    act_epilog.

process_clara_explain(3) :-
    write('You: "I''ve got a feeling there''s something big waiting for us."'), nl,
    write('Clara: "Feelings don''t keep us warm, doc. What''s in that diary that''s got you hooked?"'),
    write('You: "Hints of a hidden land-geological oddities, maybe more."'), nl,
    retractall(task(_)),
    act_epilog.

process_further_explain(1) :-
    write('You: "Byrd wasn''t a dreamer. Those coordinates mean something."'), nl,
    write('Clara: "Maybe. But I''d rather not die proving him right."'), nl,
    act_epilog.

process_further_explain(2) :-
    write('You: "Even if it''s nothing, the science alone is worth it."'), nl,
    write('Clara: "Maybe. But I''d rather not die proving him right."'), nl,
    act_epilog.

/* Movement between locations */
go(_) :-
    finished_act(1),
    write('You''ve already finished this act. Enter "next." to proceed or "halt." to quit.'),
    !, nl.

go(Place) :-
    i_am_at(Here),
    path(Here, Place),
    retract(i_am_at(Here)),
    assert(i_am_at(Place)),
    !, look.

go(_) :-
    write('You can''t go there from here.'),
    nl.

/* This rule tells how to look about you. */
look :-
    finished_act(1),
    write('You''ve already finished this act. Enter "next." to proceed or "halt." to quit.'),
    !, nl.

look :-
    i_am_at(Place),
    describe(Place),
    !, nl.

/* Hint system */
hint :-
    finished_act(1),
    write('You''ve already finished this act. Enter "next." to proceed or "halt." to quit.'),
    !, nl.

hint :-
    i_am_at(barrack),
    not(holding(lighter)),
    write('I should gather something useful.'),
    !, nl.

hint :-
    task(fuel),
    not(examined(tanks)),
    write('I should check the fuel TANKS.'),
    !, nl.

hint :-
    task(fuel),
    holding(canister),
    write('I should give the CANISTER to Clara.'),
    !, nl.

hint :-
    task(fuel),
    write('I should gather some fuel.'),
    !, nl.

hint :-
    task(supplies),
    i_am_at(runway),
    findall(Y, (holding(Y), supply(Y)), SupplyList),
    length(SupplyList, Count),
    Count > 0,
    write('I should thank her for the coffee.'),
    !, nl.

hint :-
    task(supplies),
    write('I should gather supplies in the supply TENT.'),
    !, nl.

hint :-
    task(supplies),
    i_am_at(tent),
    write('I should take only the most necessary items for the mission.'),
    !, nl.

hint :-
    write('I think I should talk with Clara.'),
    !, nl.

/* This rule starts Act 1 */
start_act :-
    initialize_act,
    intro.

act_end :-
    nl,
    write('----------------------------ACT 1 OVER----------------------------'),
    !, nl,
    cleanup,
    asserta(user:finished_act(1)),
    user:check_progress.

cleanup :-
    retractall(talked(_, _)),
    retractall(task(_)),
    retractall(at(_, _)),
    retractall(i_am_at(_)).

intro :-
    write('ACT 1: DEPARTURE FROM THE EDGE OF THE WORLD'), nl, nl,
    write('You awaken to a stark view from your window at an Antarctic base camp in New Swabia.'), nl,
    write('A desolate expanse of ice and snow stretches endlessly under a pale, gray sky.'), nl,
    write('You get up, dress in layers against the cold and step outside.'), nl,
    look.

/* These rules describe the various locations. */
describe(yard) :-
    write('You''re on the BARRACK yard. Nearby, a sturdy twin-engine plane rests'), nl,
    write('on a makeshift RUNWAY, its metal hull glinting faintly in the weak sunlight.'), nl,
    write('To the side, there''s a fuel DEPOT and a supply TENT. The air is frigid,'), nl,
    write('the wind howls intermittently, and the isolation weighs heavily. By the plane,'), nl,
    write('you spot your partner, Lt. CLARA Voss, a pragmatic military pilot assigned'), nl,
    write('to join you on this mission.'), nl.

describe(barrack) :-
    write('This is your resting place during the mission - small but convenient.'), nl,
    write('Your bed is neatly made, and a PHOTO of your late wife sits on the dresser beside it.'), nl,
    write('Across the room, your working desk holds mission documents, a small lamp, and a LIGHTER.'), nl,
    write('A CALENDAR hangs above the desk.'), nl,
    write('Outside - the YARD, covered in snow.').

describe(runway) :-
    findall(Y, (holding(Y), supply(Y)), SupplyList),
    length(SupplyList, Count),
    Count > 0,
    write('Clara has finished fueling and has something waiting for you.'), nl,
    write('You pack the supplies into the plane. The reason for your journey-Admiral Byrd''s diary-lies open on a box in front of you,'), nl,
    write('its cryptic coordinates circled in red ink: 70S, 10E.'), nl,
    write('Clara hands you a cup of lukewarm coffee.'),
    !, nl.


describe(runway) :-
    not(holding(canister)),
    write('The sunlight, reflected off the steel plates, blinds you as you approach the aircraft - '), nl,
    write('a Douglas A-20 Havoc. It''s not the newest PLANE, but it''s reliable.'), nl,
    (at(clara, runway) -> write('CLARA is tinkering with one of the engines.'); true), nl,
    write('Behind you - the YARD, covered in snow.').

describe(runway) :-
    holding(canister),
    write('The sunlight, reflected off the steel plates, blinds you as you approach the Douglas A-20 Havoc - '), nl,
    write('a reliable, if not modern, PLANE.'), nl,
    write('Clara is still tinkering with one of the engines.'), nl,
    write('Behind you - the YARD, covered in snow.').

describe(depot) :-
    write('You step into the depot, a rough but functional structure shielding fuel CANISTERs from the Antarctic cold.'), nl,
    write('Outside - the YARD, covered in snow.').

describe(tent) :-
    write('You enter the supply tent, a cramped space cluttered with gear.'), nl,
    write('Boxes and crates are labeled with essentials: food, water, scientific tools, and survival equipment.'), nl,
    write('A LIST of stock hangs on the wall.'), nl,
    write('Outside - the YARD, covered in snow.').

act_epilog :-
    write('You: "What do you think we''ll find out there?"'), nl,
    write('Clara: "Best case? A rock formation worth naming. Worst case? A grave with our names on it. I don''t buy the unearthly land garbage."'), nl,
    write('You: "Neither do I, but the government does."'), nl,
    write('Clara: "I think it''s time we have a good weather"'), nl, nl,
    write('Preparations complete, you and Clara climb into the plane''s hatch.'), nl,
    write('Clara starts the engines, ready to challenge the icy wilderness.'), nl,
    write('The plane roars to life, cutting through swirling snow as it lifts off.'), nl, nl,
    write('Inside, you study the diary while Clara grips the yoke.'), nl,
    write('The horizon swallows the base camp, leaving you with a mix of anticipation-and a hint of lurking danger.'), nl,
    !, act_end.
