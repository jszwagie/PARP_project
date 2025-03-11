/* Antarctic Expedition, a text adventure game in Prolog */

:- dynamic i_am_at/1, at/2, holding/1, talked/2, examined/1, task/1.
:- retractall(at(_, _)), retractall(i_am_at(_)), retractall(holding(_)),
   retractall(talked(_, _)), retractall(examined(_)), retractall(task(_)).

/* Initial state */
i_am_at(start).

/* Define locations */
location(start).
location(barrack).
location(runway).
location(depot).
location(tent).

/* Define paths between locations */
path(start, barrack).
path(start, runway).
path(start, depot).
path(start, tent).
path(barrack, start).
path(runway, start).
path(depot, start).
path(tent, start).

/* Define items at locations */
at(clara, runway).
at(lighter, barrack).
at(photo, barrack).
at(calendar, barrack).
at(plane, runway).
at(canister, depot).
at(tanks, runway).
at(food, tent).
at(water, tent).
at(geiger, tent).
at(medkit, tent).
at(radio, tent).
at(gear, tent).
at(tools, tent).

/* Define items that could be a part of the supplies */
supply(food).
supply(water).
supply(geiger).
supply(medkit).
supply(radio).
supply(gear).
supply(tools).

/* Define items that can be picked up */
can_take(lighter).
can_take(canister).

/* These rules describe how to pick up an object. */
take(X) :-
    holding(X),
    write('You''re already holding it!'),
    !, nl.

take(X) :-
    supply(X),
    i_am_at(Place),
    at(X, Place),
    findall(Y, (holding(Y), supply(Y)), SupplyList),
    length(SupplyList, Count),
    Count < 5,
    retract(at(X, Place)),
    assert(holding(X)),
    comment_take(X),
    !, nl.

take(X) :-
    supply(X),
    findall(Y, (holding(Y), supply(Y)), SupplyList),
    length(SupplyList, Count),
    Count >= 5,
    write('You cannot take this - you''ve reached the limit - 5 items only.'),
    !, nl.

take(X) :-
    i_am_at(Place),
    at(X, Place),
    can_take(X),
    retract(at(X, Place)),
    assert(holding(X)),
    write('OK.'),
    !, nl.

take(photo) :-
    write('Sorry, my love, but I can''t take you with me.'),
    !, nl.

take(calendar) :-
    write('I doubt this will be useful.'),
    !, nl.

take(_) :-
    write('I don''t see it here or can''t take that.'),
    !, nl.

/* These rules describe how to put down an object. */
drop(lighter) :-
    holding(lighter),
    i_am_at(Place),
    retract(holding(lighter)),
    assert(at(lighter, Place)),
    write('I might need something else instead.'),
    !, nl.

drop(canister) :-
    holding(canister),
    write('I should get it to Clara.'),
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
    write('- FOOD Rations"'), nl,
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
    assert(task(fuel)),
    !, nl.

examine(canister) :-
    i_am_at(depot),
    write('Heavy, but necessary.'),
    assert(examined(canister)),
    !, nl.

examine(_) :-
    write('There''s nothing special about it.'),
    !, nl.

/* Talk to people */
talk(clara) :-
    \+ i_am_at(runway),
    write('I''m not going to shout; I should go to her.'),
    !, nl.

talk(clara) :-
    i_am_at(runway),
    \+ talked(clara, fuel_request),
    \+ task(fuel),
    write('Clara: "Morning, doc. We need to get this bird fueled up. Could you check the TANKS for me?"'), nl,
    write('Your choices:'), nl,
    write('1. "Okay, I''ll handle it now."'), nl,
    write('2. "I think it''s good enough, but I could double-check."'), nl,
    write('3. "Why don''t you take care of it?"'), nl,
    read(Choice),
    process_clara_fuel_talk(Choice).

talk(clara) :-
    i_am_at(runway),
    holding(canister),
    task(fuel),
    write('I have it!'), nl,
    write('Clara: "Nice, hand it over - our bird''s thirsty."'), nl,
    write('*starts fueling the plane*'), nl,
    write('Clara: "Why don''t you gather some supplies while I finish fueling?"'), nl,
    write('Your choices:'), nl,
    write('1. "On my way."'), nl,
    write('2. "Are you sure you can handle it by yourself?"'), nl,
    read(Choice),
    process_clara_supplies_talk(Choice).

talk(_) :-
    write('There''s no one here to talk to.'),
    nl.

/* Process dialog choices */
process_clara_fuel_talk(1) :-
    write('You: "Okay, I''ll handle it now."'),
    retractall(talked(clara, _)),
    assert(talked(clara, fuel_request)),
    nl.

process_clara_fuel_talk(2) :-
    write('You: "I think it''s good enough, but I could double-check."'), nl,
    write('Clara: "Good enough doesn''t cut it out here. Antarctica doesn''t forgive mistakes. Check it properly."'),
    retractall(talked(clara, _)),
    assert(talked(clara, fuel_request)),
    nl.

process_clara_fuel_talk(3) :-
    write('You: "Why don''t you take care of it?"'), nl,
    write('Clara: (frowning) "Oh, you''re lazy, aren''t you? Fine, I''ll handle it after I finish checking the oil, but you''re not off the hook, doc. Go gather mandatory supplies and drop them near the plane."'),
    retractall(task(_)),
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

/* Movement between locations */
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
    i_am_at(Place),
    describe(Place),
    !, nl.

/* Hint system */
hint :-
    i_am_at(barrack),
    not(holding(lighter)),
    write('I should gather something useful.'),
    !, nl.

hint :-
    talked(clara, fuel_request),
    not(examined(tanks)),
    write('I should check the fuel tanks.'),
    !, nl.

hint :-
    task(fuel),
    write('I should gather some fuel.'),
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

/* Game initialization and instructions */
instructions :-
    nl,
    write('ACT 1: DEPARTURE FROM THE EDGE OF THE WORLD'), nl, nl,
    write('Enter commands using standard Prolog syntax.'), nl,
    write('Available commands are:'), nl,
    write('start.             -- to start the game.'), nl,
    write('look.              -- to look around you again.'), nl,
    write('go(Place).         -- to go to a place.'), nl,
    write('take(Object).      -- to pick up an object.'), nl,
    write('drop(Object).      -- to put down an object.'), nl,
    write('examine(Object).   -- to examine an object closely.'), nl,
    write('talk(Person).      -- to talk to someone.'), nl,
    write('hint.              -- to get a hint if you''re stuck.'), nl,
    write('instructions.      -- to see this message again.'), nl,
    write('halt.              -- to end the game and quit.'), nl,
    nl.

/* This rule prints out instructions and tells where you are. */
start :-
    instructions,
    intro.

/* This rule tells how to finish the game. */
finish :-
    nl,
    write('The game is over. Please enter the "halt." command.'),
    nl.

intro :-
    write('You awaken to a stark view from your window at an Antarctic base camp in New Swabia.'), nl,
    write('A desolate expanse of ice and snow stretches endlessly under a pale, gray sky.'), nl,
    write('You get up, dress in layers against the cold and step outside.'), nl,
    look.

/* These rules describe the various locations. */
describe(start) :-
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
    write('You can go to: START.').

describe(runway) :-
    not(holding(canister)),
    write('The sunlight, reflected off the steel plates, blinds you as you approach the aircraft - '), nl,
    write('a Douglas A-20 Havoc. It''s not the newest PLANE, but it''s reliable.'), nl,
    (at(clara, runway) -> write('CLARA is tinkering with one of the engines.'); true), nl,
    write('You can go to: START.').

describe(runway) :-
    holding(canister),
    write('The sunlight, reflected off the steel plates, blinds you as you approach the Douglas A-20 Havoc - '), nl,
    write('a reliable, if not modern, PLANE.'), nl,
    write('Clara is still tinkering with one of the engines.'), nl,
    write('You can go to: START.').

describe(depot) :-
    write('You step into the depot, a rough but functional structure shielding fuel CANISTERs from the Antarctic cold.'), nl,
    write('You can go to: START.').

describe(tent) :-
    write('You enter the supply tent, a cramped space cluttered with gear.'), nl,
    write('Boxes and crates are labeled with essentials: food, water, scientific tools, and survival equipment.'), nl,
    write('A LIST of stock hangs on the wall.'), nl,
    write('You can go to: START.').

comment_take(food) :-
    write('Essential for survival.'), nl,
    write('I don''t plan on starving out there.'), nl,
    !, nl.

comment_take(water) :-
    write('Dehydration is just as dangerous as the cold.'), nl,
    !, nl.

comment_take(geiger) :-
    write('If we''re dealing with something unnatural, this might be useful.'), nl,
    !, nl.

comment_take(medkit) :-
    write('Better to be safe than sorry.'), nl,
    !, nl.

comment_take(radio) :-
    write('If we lose contact, this might be our only way to call for help.'), nl,
    !, nl.

comment_take(gear) :-
    write('If we have to scale ice walls or descend into caves, we''ll need this.'), nl,
    !, nl.

comment_take(tools) :-
    write('We can''t afford to get lost.'), nl,
    !, nl.

comment_drop(food) :-
    write('I hope we won''t regret this.'), nl,
    !, nl.

comment_drop(water) :-
    write('Maybe there''s another source where we''re heading.'), nl,
    !, nl.

comment_drop(geiger) :-
    write('If there''s nothing radioactive, it''s just extra weight.'), nl,
    !, nl.

comment_drop(medkit) :-
    write('Risky move, but I might need something else more.'), nl,
    !, nl.

comment_drop(radio) :-
    write('We''ll just have to rely on good old-fashioned shouting.'), nl,
    !, nl.

comment_drop(gear) :-
    write('Hopefully, no steep cliffs on this trip.'), nl,
    !, nl.

comment_drop(tools) :-
    write('If Clara can fly straight, maybe we won''t need these.'), nl,
    !, nl.

