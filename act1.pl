/* Initial state */
i_am_at(yard).

/* Define locations */
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

talk(clara) :-
    i_am_at(runway),
    task(supplies),
    findall(Y, (holding(Y), supply(Y)), SupplyList),
    length(SupplyList, Count),
    Count > 0,
    retractall(task(_)),
    write('Thank you!'), nl,
    write('*a moment of silence*'), nl,
    write('Clara: "So, tell me again why we''re risking our necks for this?'), nl,
    write('A diary from some explorer doesn''t scream ''top priority'' to me."'), nl,
    write('Your choices:'), nl,
    write('1."Because it could be the discovery of the century."'), nl,
    write('2."Orders are orders. The government wants answers."'), nl,
    write('3."I''ve got a feeling there''s something big waiting for us."'), nl,
    read(Choice),
    process_clara_explain(Choice).

talk(clara) :-
    i_am_at(runway),
    task(supplies),
    findall(Y, (holding(Y), supply(Y)), SupplyList),
    length(SupplyList, Count),
    not(Count > 0),
    write('You have to grab at least one item.'),
    !, nl.

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
    read(Choice),
    process_further_explain(Choice).

process_clara_explain(2) :-
    write('You: "Orders are orders. The government wants answers."'), nl,
    write('Clara: "Yeah, and Uncle Sam loves sending us into the freezer for kicks. What''s their angle?"'), nl,
    write('You: "Cold War jitters, probably. They don''t want the Soviets sniffing around first."'), nl,
    retractall(task(_)),
    act1_epilog.

process_clara_explain(3) :-
    write('You: "I''ve got a feeling there''s something big waiting for us."'), nl,
    write('Clara: "Feelings don''t keep us warm, doc. What''s in that diary that''s got you hooked?"'),
    write('You: "Hints of a hidden land-geological oddities, maybe more."'), nl,
    retractall(task(_)),
    act1_epilog.

process_further_explain(1) :-
    write('You: "Byrd wasn''t a dreamer. Those coordinates mean something."'), nl,
    write('Clara: "Maybe. But I''d rather not die proving him right."'), nl,
    act1_epilog.

process_further_explain(2) :-
    write('You: "Even if it''s nothing, the science alone is worth it."'), nl,
    write('Clara: "Maybe. But I''d rather not die proving him right."'), nl,
    act1_epilog.

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
    task(fuel),
    not(examined(tanks)),
    write('I should check the fuel tanks.'),
    !, nl.

hint :-
    task(fuel),
    write('I should gather some fuel.'),
    !, nl.

hint :-
    task(supplies),
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

/* Game initialization and instructions */
instructions :-
    nl,
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
start_act1 :-
    instructions,
    intro.

act1_end :-
    retractall(talked(_, _)),
    retractall(task(_)),
    nl,
    write('----------------------------ACT 1 OVER----------------------------'),
    !, nl,
    assert(finished_act(1)),
    check_progress.

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
    write('You can go to: YARD.').

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
    write('You can go to: YARD.').

describe(runway) :-
    holding(canister),
    write('The sunlight, reflected off the steel plates, blinds you as you approach the Douglas A-20 Havoc - '), nl,
    write('a reliable, if not modern, PLANE.'), nl,
    write('Clara is still tinkering with one of the engines.'), nl,
    write('You can go to: YARD.').

describe(depot) :-
    write('You step into the depot, a rough but functional structure shielding fuel CANISTERs from the Antarctic cold.'), nl,
    write('You can go to: YARD.').

describe(tent) :-
    write('You enter the supply tent, a cramped space cluttered with gear.'), nl,
    write('Boxes and crates are labeled with essentials: food, water, scientific tools, and survival equipment.'), nl,
    write('A LIST of stock hangs on the wall.'), nl,
    write('You can go to: YARD.').

comment_take(food) :-
    write('Essential for survival.'), nl,
    write('I don''t plan on starving out there.'),
    !, nl.

comment_take(water) :-
    write('Dehydration is just as dangerous as the cold.'),
    !, nl.

comment_take(geiger) :-
    write('If we''re dealing with something unnatural, this might be useful.'),
    !, nl.

comment_take(medkit) :-
    write('Better to be safe than sorry.'),
    !, nl.

comment_take(radio) :-
    write('If we lose contact, this might be our only way to call for help.'),
    !, nl.

comment_take(gear) :-
    write('If we have to scale ice walls or descend into caves, we''ll need this.'),
    !, nl.

comment_take(tools) :-
    write('We can''t afford to get lost.'),
    !, nl.

comment_drop(food) :-
    write('I hope we won''t regret this.'),
    !, nl.

comment_drop(water) :-
    write('Maybe there''s another source where we''re heading.'),
    !, nl.

comment_drop(geiger) :-
    write('If there''s nothing radioactive, it''s just extra weight.'),
    !, nl.

comment_drop(medkit) :-
    write('Risky move, but I might need something else more.'),
    !, nl.

comment_drop(radio) :-
    write('We''ll just have to rely on good old-fashioned shouting.'),
    !, nl.

comment_drop(gear) :-
    write('Hopefully, no steep cliffs on this trip.'),
    !, nl.

comment_drop(tools) :-
    write('If Clara can fly straight, maybe we won''t need these.'),
    !, nl.

act1_epilog :-
    write('You: "What do you think we''ll find out there?"'), nl,
    write('Clara: "Best case? A rock formation worth naming. Worst case? A grave with our names on it. I don''t buy the unearthly land garbage."'), nl,
    write('You: "Neither do I, but the government does."'), nl,
    write('Clara: "I think it''s time we have a good weather"'), nl,
    write('Preparations complete, you and Clara climb into the plane''s hatch.'), nl,
    write('Clara starts the engines, ready to challenge the icy wilderness.'), nl,
    write('The plane roars to life, cutting through swirling snow as it lifts off.'), nl,
    write('Inside, you study the diary while Clara grips the yoke.'), nl,
    write('The horizon swallows the base camp, leaving you with a mix of anticipation-and a hint of lurking danger.'), nl,
    !, act1_end.
