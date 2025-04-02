:- module(act3, []).

:- dynamic(attempts/2).
:- dynamic(looked/1).

wrong_count(A, B, C, Count) :-
    (A =:= 2 -> WA = 0 ; WA = 1),
    (B =:= 7 -> WB = 0 ; WB = 1),
    (C =:= 4 -> WC = 0 ; WC = 1),
    Count is WA + WB + WC.

increment_attempts(radio) :-
    attempts(radio, N),
    N1 is N + 1,
    retract(attempts(radio, N)),
    assert(attempts(radio, N1)).

radio_hint(radio) :-
    attempts(radio, Count),
    ( Count =:= 2 ->
        write('HINT: "The plaque mentions ''A=Even, B=Prime, C=Square.'' And the note says ''Two''s pair''-could A be 2?"'), nl
    ; Count =:= 3 ->
        write('HINT: "Think it through: 2 is even, 7 is prime, and 4 ties to the square of 2. That matches all the clues."'), nl
    ; true ).

initialize_act :-
    retractall(i_am_at(_)),
    assert(i_am_at(ledge)),
    assert(user:task(ledge_talk)),
    assert(at(clara, ledge)),
    assert(at(creature, ruins)),
    assert(attempts(radio, 0)).

/* Define locations */
location(ledge).
location(tree).
location(ruins).
location(tunnel).
location(city).
location(rock).

/* Define paths */
path(ledge, tree).
path(tree, ledge).
path(tree, ruins).
path(ruins, tree).
path(tunnel, tree).
path(tree, tunnel).
path(tree, city).
path(city, tree).


start_act :-
    initialize_act,
    intro.


intro :-
    nl, nl, write('ACT 3: INTO THE HEART OF THE UNKNOWN'), nl, nl,
    write('You and Clara carefully climb down from the ledge, your boots sinking into the soft, mossy ground.'), nl,
    write('The valley pulses with life-chirping insects fill the air, leaves rustle in a gentle breeze, and the distant roar of an unseen beast sends a shiver down your spine.'), nl,
    write('The memory of your crash-landed supplies lingers, a heavy burden as you take your first cautious steps into this strange, uncharted world.'), nl,
    look.

/* This rule tells how to look about you. */
look :-
    finished_act(3),
    write('You''ve already finished this act. Enter "halt." to quit.'),
    !, nl.

look :-
    i_am_at(Place),
    describe(Place),
    !, nl.

/* Describe locations */
describe(marines_plane) :-
    i_am_at(marines_plane),
    write('The Marines'' plane is a sturdy transport, its fuselage bearing the marks of countless missions.'), nl,
    write('Inside, the cabin is a tight, warm space, alive with the hum of engines and the focused energy of the Marines.'),
    !, nl.

describe(ruins) :-
    i_am_at(ruins),
    write('The ruins before you are a marvel of ancient architecture, reminiscent of Egypt’s pyramids or the jungle temples of South America, yet distinctly alien.'), nl,
    write('Crumbling stone facades are adorned with intricate carvings of starships and celestial beings, hinting at a civilization far beyond human comprehension.'), nl,
    write('The air here feels thick with history and unspoken secrets.'),
    !, nl.

describe(city) :-
    i_am_at(city),
    write('The city cuts a stark silhouette against the valley’s greenery, its dark gray buildings rising like monolithic sentinels.'), nl,
    write('Swastika flags flutter ominously from every structure, their bold red and black stark against the muted stone.'), nl,
    write('The atmosphere is heavy with foreboding, as if the very walls are watching your every move.'),
    !, nl.

describe(ledge) :-
    task(ledge_talk),
    write('You both stand on a rocky ledge overlooking a hidden realm-an expansive, verdant valley cradled beneath Antarctica''s icy crust.'), nl,
    write('Bioluminescent plants emit a soft, ethereal glow, casting light across towering ferns and crystalline rivers that shimmer like liquid glass.'), nl,
    write('The air hangs warm and humid, thick with the scent of exotic blooms, a jarring contrast to the frozen desolation above.'), nl,
    write('Flying saucers, eerily similar to the wreck you stumbled upon, glide silently through the skies, their presence a quiet warning of something watchful and alive down here.'),
    !, nl.

describe(ledge) :-
    task(tree),
    assert(described(tree)),
    write('Ahead looms a massive TREE, its gnarled trunk wider than a barn, its branches clawing toward the cavern''s glowing ceiling.'), nl,
    write('Bioluminescent moss clings to its bark, pulsing faintly, while its leaves shimmer with an unearthly light, swaying as if whispering secrets to the wind.'),
    assert(looked(ledge)),
    !, nl.

describe(tree) :-
    i_am_at(tree),
    write('From the tree''s upper branches, the valley sprawls before you in breathtaking detail.'), nl,
    write('To the east, ancient-looking RUINS emerge from the foliage-crumbling pyramids and temples etched with cryptic symbols, remnants of a lost civilization.'), nl,
    write('To the west, the stark silhouette of a CITY cuts through the greenery, its dark gray buildings festooned with swastika flags fluttering ominously in the breeze, their bold red and black stark against the muted stone.'), nl,
    write('Behind you, the TUNNEL exit gapes like a dark maw, leading back to the frozen surface-a lifeline or a trap, depending on your next move.'),
    !, nl.

describe(tunnel) :-
    i_am_at(tunnel),
    task(tunnel),
    write('The crash site lies in ruins, the plane''s twisted metal half-buried in snow.'), nl,
    write('The wind howls mercilessly, and the sky above is a bleak, unforgiving gray.'), nl,
    write('Your breath fogs in the frigid air as you clutch the RADIO, your last lifeline.'),
    !, nl.

describe(tunnel) :-
    i_am_at(tunnel),
    task(radio),
    write('The crash site lies in ruins, the plane''s twisted metal half-buried in snow.'), nl,
    write('The wind howls mercilessly, and the sky above is a bleak, unforgiving gray.'), nl,
    write('Your breath fogs in the frigid air as you clutch the RADIO, your last lifeline.'),
    !, nl.


/* Hint system */
hint :-
    finished_act(3),
    write('You''ve already finished this act. Enter "halt." to quit.'),
    !, nl.

hint :-
    task(woods),
    write('We should GO to the WOODS.'),
    !, nl.

hint :-
    i_am_at(tunnel),
    holding(radio),
    task(tunnel),
    write('I should USE the RADIO, as I said.'),
    nl, !.

hint :-
    i_am_at(rock),
    task(tunnel),
    write('We should go to TUNNEL.'),
    nl, !.

hint :-
    i_am_at(rock),
    task(after_fight),
    write('I should talk to Clara.'),
    nl, !.

hint :-
    i_am_at(rock),
    holding(pistol),
    task(fight),
    write('I should hand the PISTOL to Clara.'),
    !, nl.

hint :-
    task(ambush_beginning),
    write('Maybe Clara knows what to do in this situation.'),
    !, nl.

hint :-
    task(after_radio),
    write('I should talk to Clara.'),
    !, nl.

hint :-
    task(radio),
    holding(radio),
    examined(note),
    write('The note says ''Four''s the square, Seven''s luck, Two''s pair.'' That could point to the settings for A, B, and C. The plaque might help confirm it.'),
    !, nl.

hint :-
    task(radio),
    holding(radio),
    write('I need to tune the dials to the right numbers to reach the Marines. The NOTE or the RADIO might hold the key.'),
    !, nl.

hint :-
    task(hide),
    holding(pistol),
    write('I should GO behind that ROCK.'),
    !, nl.

hint :-
    i_am_at(ledge),
    task(ledge_talk),
    write('I should talk to Clara.'),
    nl, !.

hint :-
    i_am_at(ledge),
    task(tree),
    looked(ledge),
    write('I think I could GO up on that TREE.'),
    nl, !.

hint :-
    i_am_at(ledge),
    task(tree),
    write('I should find a high place for recon.'),
    nl, !.

hint :-
    i_am_at(tree),
    write('I should LOOK around and decide where to GO.'),
    !, nl.

hint :-
    i_am_at(ruins),
    at(creature, ruins),
    write('Is the TALK the answer?'),
    !, nl.

hint :-
    i_am_at(ruins),
    task(hide),
    write('I should hide behind that ROCK'),
    !, nl.

hint :-
    i_am_at(city),
    task(hide),
    write('I should hide behind that ROCK'),
    !, nl.

hint :-
    i_am_at(ruins),
    task(tunnel),
    write('I should GO to the TUNNEL'),
    !, nl.

hint :-
    i_am_at(city),
    task(tunnel),
    write('I should GO to the TUNNEL'),
    !, nl.

hint :-
    i_am_at(ruins),
    task(woods),
    write('I should GO to the WOODS'),
    !, nl.

hint :-
    i_am_at(city),
    task(woods),
    write('I should GO to the WOODS'),
    !, nl.

examined(_) :-
    finished_act(3),
    write('You''ve already finished this act. Enter "halt." to quit.'),
    !, nl.

examine(creature) :-
    i_am_at(ruins),
    at(creature, ruins),
    write('The creature stands tall and slender, its luminous eyes studying you with an intelligence that feels ancient.'), nl,
    write('Its skin seems to shimmer faintly, and as you look closer, you realize it''s communicating directly into your mind—a melodic hum that bypasses your ears.'), nl,
    write('It exudes an aura of wisdom and otherworldliness, as if it holds secrets older than time itself.'),
    !, nl.

examine(tree) :-
    write('The tree stands ancient and imposing, its roots plunging into the earth like the veins of the valley itself.'),
    nl, !.

examine(ruins) :-
    (i_am_at(ruins); i_am_at(tree)),
    write('The ruins are a marvel of ancient architecture, reminiscent of Egypt’s pyramids or the jungle temples of South America, yet distinctly alien.'),
    nl, !.

examine(city) :-
    (i_am_at(city); i_am_at(tree)),
    write('The city cuts a stark silhouette against the valley’s greenery, its dark gray buildings rising like monolithic sentinels.'),
    nl, !.

examine(tunnel) :-
    (i_am_at(tunnel); i_am_at(tree)),
    write('The tunnel exit gapes like a dark maw, leading back to the frozen surface-a lifeline or a trap, depending on your next move.'),
    nl, !.

examine(radio) :-
    task(radio),
    holding(radio),
    write('The RADIO is a rugged military device, scratched and dented but still working.'), nl,
    write('Each dial can be set to a number between 1 and 9.'), nl,
    write('The dials click stiffly as you turn them. A small plaque beneath them reads: "Standard Marine Corps Protocol: A=Even, B=Prime, C=Square."'),
    !, nl.

examine(note) :-
    task(radio),
    holding(radio),
    write('The note is weathered, its ink blurred but readable: "Marine Corps Frequency: Alpha-Bravo-Charlie. Remember the code: Four''s the square, Seven''s luck, Two''s pair."'),
    !, nl.

examine(X) :-
    holding(X),
    write('I''m holding it.'),
    !, nl.

examine(_) :-
    write('I can''t see it here or there''s nothing special about it.'),
    !, nl.

/* drop an object */
/* These rules describe how to put down an object. */
drop(_) :-
    finished_act(3),
    write('You''ve already finished this act. Enter "next." to proceed or "halt." to quit.'),
    !, nl.

drop(pistol) :-
    holding(pistol),
    task(fight),
    i_am_at(rock),
    retract(holding(pistol)),
    retract(task(fight)),
    assert(task(after_fight)),
    write('You hand the PISTOL to Clara.'), nl,
    write('She aims the old Mauser and squeezes the trigger-a sharp crack echoes through the valley, but the gun jams mid-shot, smoke curling from the barrel like a dying breath.'), nl,
    write('The Nazis roar in fury, their rifles spitting fire in response.'), nl,
    write('Bullets chip the rock, showering you with dust and shards.'), nl,
    write('The leader bellows, his voice thick with venom:'), nl, nl,
    write('Nazi Leader: "Ihr wagt es, uns herauszufordern? Euer Blut wird dieses Tal beflecken!"'),
    !, nl.

drop(pistol) :-
    holding(pistol),
    task(fight),
    write('Clara: "Hide behind the ROCK, now!"'),
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

/* Define actions */
use(_) :-
    finished_act(3),
    write('You''ve already finished this act. Enter "halt." to quit.'),
    !, nl.

use(pistol) :-
    holding(pistol),
    task(fight),
    i_am_at(rock),
    retract(holding(pistol)),
    retract(task(fight)),
    assert(task(after_fight)),
    write('You hand the PISTOL to Clara.'), nl,
    random(C),
    (C > 0.5 ->
        write('She aims the old Mauser and squeezes the trigger-a sharp crack echoes through the valley, but the gun jams mid-shot, smoke curling from the barrel like a dying breath.'), nl,
        write('The Nazis roar in fury, their rifles spitting fire in response.'), nl,
        write('Bullets chip the rock, showering you with dust and shards.'), nl,
        write('The leader bellows, his voice thick with venom:'), nl, nl,
        write('Nazi Leader: "Ihr wagt es, uns herauszufordern? Euer Blut wird dieses Tal beflecken!"')
    ;
        write('WIP'), nl /* TODO: Shot missed text */
    ),
    !, nl.

use(pistol) :-
    holding(pistol),
    task(fight),
    write('Clara: "Hide behind the ROCK, now!"'),
    !, nl.

use(radio) :-
    holding(radio),
    task(tunnel),
    retractall(task(tunnel)),
    assert(task(radio)),
    write('The RADIO crackles in your hands, its three dials labeled A, B, and C glinting faintly in the dim light of the crash site.'), nl, nl,
    write('Each dial can be set to a number between 1 and 9.'), nl,
    write('A faded, crumpled NOTE taped to the side reads: "Marine Corps Frequency: Alpha-Bravo-Charlie."'), nl,
    write('The wind howls outside, urging you to hurry.'), nl,
    !, nl.

use(radio) :-
    holding(radio),
    task(radio),
    attempts(radio, Count),
    Count > 3,
    write('The RADIO sparks violently as you fumble again, overwhelming you with static.'), nl,
    write('It''s out of order now.'), nl,
    tunnel_game_over,
    !, nl.

use(radio) :-
    holding(radio),
    task(radio),
    write('You grip the RADIO and start adjusting the dials to tune the frequency.'), nl,
    write('Set A to: '), read(A),
    write('Set B to: '), read(B),
    write('Set C to: '), read(C),
    wrong_count(A, B, C, Count),
    (
        Count =:= 0 ->
            write('The RADIO hums as it locks onto a strong signal. A clear voice cuts through:'), nl, nl,
            write('"Mission 334, this is the 32nd Marine Corps. Coordinates received. Extraction team inbound. Over."'), nl,
            write('Clara: "Copy that. We''ll hold tight. Over."'), nl,
            write('Marine: "Copy that. We''re tracking your signal. Hold tight, over."'), nl, nl,
            write('The steady signal brings a flicker of relief amidst the chaos.'), nl,
            retract(holding(radio)),
            assert(used(radio)),
            radio_game_end
        ; Count =:= 1 ->
            writeln('The RADIO picks up a faint Marine transmission, but it''s garbled:'), nl,
            writeln('"Mission... [static]... coordinates... [static]... hold..."'),
            writeln('Clara: "Almost there, but it''s too weak. They won''t get our position like this."'),
            increment_attempts(radio),
            radio_hint(radio)
        ; Count =:= 2 ->
            writeln('A sharp burst of static erupts from the RADIO, followed by a chilling German voice:'), nl,
            writeln('"Achtung! Feindliche Ubertragung entdeckt!"'),
            writeln('Clara: "That''s the Germans-they''ve intercepted us. We''ve got to fix this now!"'),
            increment_attempts(radio),
            radio_hint(radio)
        ; Count =:= 3 ->
            writeln('The RADIO hisses with static, a grating buzz drowning out any signal.'),
            writeln('You: "Just noise. This isn''t the right frequency."'),
            increment_attempts(radio),
            radio_hint(radio)
    ), nl.

use(radio) :-
    holding(radio),
    write('The radio has no use here, this place blocks the signal.'),
    !, nl.

use(pistol) :-
    holding(pistol),
    write('Who do you want me to shoot, you psycho?'),
    !, nl.

use(geiger) :-
    write('Radiation levels - normal.'),
    !, nl.

use(X) :-
    holding(X),
    write('I can''t use that right now.'),
    !, nl.

use(_) :-
    write('I don''t have it or I can''t use it.'),
    nl.

talk(_) :-
    finished_act(3),
    write('You''ve already finished this act. Enter "halt." to quit.'),
    !, nl.

talk(clara) :-
    task(after_radio),
    write('You turn to Clara, her face illuminated by the dim cabin lights.'), nl, nl,
    write('You: "So, what—this is how it ends?"'), nl,
    write('Clara: "Wake up!"'), nl,
    write('You: "What?!"'), nl,
    write('Clara: "WAKE UP!"'), nl, nl,
    write('Suddenly, a sharper voice breaks through the haze.'), nl,
    write('Your wife: "Damn it, wake up! You''ll be late for your lectures!"'), nl,
    write('You: "What? What lectures?"'), nl,
    write('Your wife: "You were up late watching TV again. You''ve got to stop with those'), nl,
    write('ridiculous pseudo-historical documentaries on FOCUS TV or TV4-they''re frying your brain."'), nl, nl,
    write('The Antarctic adventure dissolves like mist. You blink, disoriented, as the soft '), nl,
    write('glow of your bedside lamp replaces the plane''s harsh lights. The hum of Warsaw''s '), nl,
    write('morning traffic seeps through the window, a mundane rhythm far removed from the  '), nl,
    write('valley''s eerie pulse. It was all a dream—a vivid fantasy spun from late-night '), nl,
    write('television and a restless mind. You''re not an adventurer escaping a hidden'), nl,
    write('world; you''re an ordinary professor at the Warsaw University of Technology, with'), nl,
    write('lectures to deliver and papers to grade. Reality sinks in, familiar and unrelenting.'), nl, nl,
    write('You sit up, rubbing your eyes as the dream''s vivid details—Clara''s determined '), nl,
    write('gaze, the snow-swept valley, the roar of the plane—slip away like sand through '), nl,
    write('your fingers. Your wife moves about the room, muttering about your late-night '), nl,
    write('habits, oblivious to the epic journey you''ve just imagined.'), nl, nl,
    write('Your wife: "Honestly, those conspiracy channels will be the death of you. Go to bed on time for once."'), nl, nl,
    write('You muster a faint smile, the last echoes of the dream fading into nothingness.'), nl,
    write('The adventure is over, and the real world beckons.'), nl,
    asserta(user:finished_act(3)),
    write('THE END'), !, nl.

talk(clara) :-
    task(hide),
    i_am_at(ruins),
    write('Clara: "Here, give me the pistol and get behind that ROCK-now!"'),
    !, nl.

talk(clara) :-
    task(hide),
    i_am_at(city),
    write('Clara: "Here, give me the pistol and get behind that ROCK-now!"'),
    !, nl.

talk(clara) :-
    task(tunnel),
    i_am_at(ruins),
    write('Clara: "It''s a long shot, but let''s GO to the TUNNEL now!"'),
    !, nl.

talk(clara) :-
    task(tunnel),
    i_am_at(city),
    write('Clara: "It''s a long shot, but let''s GO to the TUNNEL now!"'),
    !, nl.

talk(clara) :-
    task(tunnel),
    i_am_at(tunnel),
    not(used(radio)),
    write('Clara: "You should USE the RADIO"'),
    !, nl.

talk(clara) :-
    task(radio),
    i_am_at(tunnel),
    not(used(radio)),
    write('Clara: "You should USE the RADIO"'),
    !, nl.

talk(clara) :-
    task(woods),
    i_am_at(ruins),
    write('Clara: "It''s a long shot, but let''s GO to the WOODS now!"'),
    !, nl.

talk(clara) :-
    task(woods),
    i_am_at(city),
    write('Clara: "It''s a long shot, but let''s GO to the WOODS now!"'),
    !, nl.

talk(clara) :-
    task(after_fight),
    write('You: "What did he say?"'), nl,
    write('Clara: *breathing heavily* "Nothing good. I don''t know if we can get out of this alive."'), nl,
    write('Clara: *shouting in fright* "Wir kapitulieren! Halt!"'), nl, nl,
    write('The soldiers cease fire, their eyes still burning with rage.'), nl,
    write('They swarm closer, boots pounding the earth like war drums, and you''re wrestled to the ground, wrists bound tight with rough cord.'), nl,
    write('Their treatment is brutal - fists and threats of execution, though they spare you for now, muttering darkly about your potential value.'), nl,
    write('They march you toward the CITY, their motorcycles roaring triumphantly.'), nl,
    to_be_continued,
    !, nl.


talk(clara) :-
    task(ambush_beginning),
    write('You: "What''s our move? They''re closing in fast."'), nl,
    write('Clara: "We''re outgunned and outmanned. Fight, run, or surrender-you decide, but make it quick!"'), nl,
    write('Your choices:'), nl,
    (holding(pistol), write('1. "Let''s fight! I''ll hand you the PISTOL!"'), nl; true),
    write('2. "Run for the TUNNEL."'),
    (holding(radio), write('We can try the RADIO one more time!'); true), nl,
    write('3. "We surrender. Maybe we can talk our way out."'), nl,
    write('4. "Into the WOODS-lose them in the trees!"'), nl,
    read(Choice), nl,
    process_ambush(Choice).

talk(clara) :-
    i_am_at(ledge),
    (not(talked(clara, ledge)),
    write('Clara: "This place... it''s like stepping into a dream. Or maybe a nightmare-I can''t decide."'), nl,
    write('You: "It''s incredible-Byrd wasn''t exaggerating in that diary."'), nl,
    write('Clara: "Sure, but that diary was written before the war. No mention of Nazis anywhere in it. Do you think they beat us to this discovery?"'), nl,
    write('1. "Maybe they found it during their Antarctic expeditions in the ''30s."'), nl,
    write('2. "Or they stumbled across it after the war, looking for a place to hide."'), nl,
    assert(talked(clara, ledge)),
    read(Choice), nl, !,
    process_clara_ledge_talk(Choice); true),
    (talked(clara, ledge),
    after_ledge_talk; true), !.

talk(clara) :-
    i_am_at(ruins),
    at(creature, ruins),
    write('Clara: "Do you think we should TALK to it?"'),
    !, nl.

talk(creature) :-
    i_am_at(ruins),
    at(creature, ruins),
    write('*The creature''s voice resonates in your mind, a melodic hum that bypasses your ears entirely.*'), nl, nl,
    write('Creature: "Wanderers, greetings. Sentinel of this realm, I am, keeper of wisdom older than your civilization, hmm.'), nl,
    write('Answers you seek, yes? Give them to you, I shall.'), nl,
    write('Tied to what you call ''Atlantis,'' our kin are, though lost to your tongue, our true name is.'), nl,
    write('Arrived, the ones you call ''Germans'' did, speaking of a great calamity they fled. Stewards of peace we are, granted them refuge, we did. Yet, tidings of their shadow you bear, hmm?"'),
    nl, nl,
    write('Your choices: '), nl,
    write('1. "Those Germans-the Nazis-are monsters. They''ve waged war and killed millions."'), nl,
    write('2. "They''re exploiting you. They''ll strip this valley bare and leave nothing behind."'), nl,
    read(Choice), nl,
    process_creature_talk(Choice).

talk(clara) :-
    i_am_at(tree),
    write('Clara: Do you see anything interesting?'),
    !, nl.

talk(_) :-
    write('There''s no one here to talk to or I can''t talk to it.'),
    nl.

process_creature_talk(1) :-
    i_am_at(ruins),
    at(creature, ruins),
    write('You: "Those Germans-the Nazis-are monsters. They''ve waged war and killed millions."'), nl,
    write('Creature: "Malice such, perceived it not, we did. Blinded us, our hospitality has, to their stain."'), nl,
    creature_disappears.

process_creature_talk(2) :-
    i_am_at(ruins),
    at(creature, ruins),
    write('You: "They''re exploiting you. They''ll strip this valley bare and leave nothing behind."'), nl,
    write('Creature: "Cloaked in deception, they are, then. Harmony we cherish, yet stirred by this threat, we are. Counsel, what offer you?"'), nl,
    creature_disappears.

process_ambush(1) :-
    holding(pistol),
    retractall(task(_)),
    assert(task(hide)),
    write('You: "Let''s fight! I''ll hand you the PISTOL!"'), nl,
    write('Clara: "Here, give me the pistol and get behind that ROCK-now!"'),
    !, nl.

process_ambush(2) :-
    retractall(task(_)),
    assert(task(tunnel)),
    write('You: "To the TUNNEL - move. '),
    (holding(radio), write('We can try the RADIO one more time!'); true), nl,
    (holding(radio), write('Clara: "It''s a long shot, but let''s go!"'); true),
    (not(holding(radio)), write('Clara: "Without the RADIO, we''ll just freeze out there. Terrible plan, but I''m with you."'); true),
    !, nl.

process_ambush(3) :-
    retractall(task(_)),
    write('You: "We surrender. Maybe we can talk our way out."'), nl,
    write('You raise your hands slowly. Clara mirrors your movement and calls out to the soldiers:'), nl,
    write('Clara: "Wir kapitulieren! Kein Problem."'), nl, nl,
    write('The Nazis lower their rifles slightly, though their glares remain sharp as knives. The leader smirks, holstering his Luger with a flourish.'), nl, nl,
    write('Nazi Leader: "Kluger Schachzug, Amerikaner. Unser Kommandant mochte Sie unbedingt sehen."'), nl,
    write('They bind your hands with coarse rope, the knots biting into your wrists, and march you toward the CITY, their motorcycles roaring triumphantly.'), nl,
    to_be_continued,
    !, nl.

process_ambush(4) :-
    retractall(task(_)),
    assert(task(woods)),
    write('You: "Come on, to the WOODS-GO!"'), nl,
    !, nl.

creature_disappears :-
    i_am_at(ruins),
    at(creature, ruins),
    retract(at(creature, ruins)),
    write('*Before you can respond, the air splits with the roar of engines and sharp, guttural shouts. The ground trembles faintly-a prelude to chaos.*'), nl,
    write('Creature: "True, if what you say is, run immediately, you must. Farewell, my friends."'), nl,
    write('*The creature dissolves into the air, leaving you confused and on edge.*'), nl,
    nazi_ambush.


process_clara_ledge_talk(1) :-
    i_am_at(ledge),
    write('You: "Maybe they found it during their Antarctic expeditions in the ''30s."'), nl,
    write('Clara: "That could explain those flying saucers. They''ve had decades to dig in, hidden from the rest of the world."'), nl,
    !, after_ledge_talk.

process_clara_ledge_talk(2) :-
    i_am_at(ledge),
    write('You: "Or they stumbled across it after the war, looking for a place to hide."'), nl,
    write('Clara: "Hide? More like regroup. This could be their secret fortress, waiting for the right moment to strike back."'), nl,
    !, after_ledge_talk.

after_ledge_talk :-
    retractall(task(_)),
    assert(task(tree)),
    write('You stand together, awestruck by the valley''s haunting beauty.'), nl,
    write('The bioluminescent flora bathes the landscape in a shimmering, otherworldly hue, while the faint hum of the valley''s life-chirps, rustles, and distant cries-wraps around you like a living tapestry.'), nl,
    write('It''s a paradise untouched by time, yet the shadow of danger looms just out of sight.'), nl, nl,
    write('Clara: *gasps* "Okay, enough gawking. If we want to survive this, we need a better lay of the land. Let''s find a high spot for reconnaissance."'),
    !, nl.



process_clara_tunnel_talk(1) :-
    write('You: "You''re right. We need to explore and figure this out."'), nl,
    write('*You stay in the valley, your resolve hardening.*'),
    !, nl.

process_clara_tunnel_talk(2) :-
    write('You: No, it''s too risky. Let''s head back while we can.'), nl,
    write('Clara: "And what? Freeze out there with no plan? We''re in too deep to turn tail now."'),
    !, nl,
    process_clara_tunnel_talk_radio.

process_clara_tunnel_talk_radio :-
    holding(radio),
    write('You: "But we could try contacting the base again with the radio."'), nl,
    write('Clara: "Don''t be naive, doc. That radio''s a piece of junk-half-static on a good day-and even if we got through, what then?'), nl,
    write('Help''s days away at best, assuming they don''t think we''re delusional. We''d be stuck out there, freezing to death, praying for a miracle.'), nl,
    write('No, we push forward, find answers, and make our own way out of this mess."'),
    !, nl.

process_clara_tunnel_talk_radio :-
    write('Clara: "And what? Freeze out there with no plan? We''re in too deep to turn tail now.'), nl,
    write('Without a radio or supplies, we wouldn''t last a day on the surface. Our only chance is to keep moving, find shelter or someone who knows what''s going on-anything''s better than retreating empty-handed."'),
    !, nl.

go(_) :-
    finished_act(3),
    write('You''ve already finished this act. Enter "halt." to quit.'),
    !, nl.

go(ledge) :-
    write('We must decide where to go.'),
    !, nl.

go(woods) :-
    i_am_at(ruins),
    task(woods),
    retractall(task(_)),
    write('You sprint into the dense forest, branches snapping underfoot as you weave through the shadows.'), nl,
    write('The Nazis'' shouts fade briefly-you dare to hope-until the sky hums with menace.'), nl,
    write('A flying saucer descends, its beam of light slashing through the canopy like a blade, pinning you in its merciless glare.'), nl, nl,
    write('Nazi Pilot (over loudspeaker): "Kein Entkommen, ihr Narren! Das Reich sieht alles!"'), nl,
    write('Riflemen emerge from the trees, their grips iron as they drag you back to the group. They bind your hands with coarse rope and march you toward the CITY, their motorcycles roaring triumphantly.'), nl,
    to_be_continued,
    !, nl.

go(woods) :-
    i_am_at(city),
    task(woods),
    retractall(task(_)),
    write('You sprint into the dense forest, branches snapping underfoot as you weave through the shadows.'), nl,
    write('The Nazis'' shouts fade briefly-you dare to hope-until the sky hums with menace.'), nl,
    write('A flying saucer descends, its beam of light slashing through the canopy like a blade, pinning you in its merciless glare.'), nl, nl,
    write('Nazi Pilot (over loudspeaker): "Kein Entkommen, ihr Narren! Das Reich sieht alles!"'), nl,
    write('Riflemen emerge from the trees, their grips iron as they drag you back to the group. They bind your hands with coarse rope and march you toward the CITY, their motorcycles roaring triumphantly.'), nl,
    to_be_continued,
    !, nl.

go(rock) :-
    (i_am_at(ruins); i_am_at(city)),
    holding(pistol),
    retractall(task(_)),
    retractall(i_am_at(_)),
    assert(i_am_at(rock)),
    assert(task(fight)),
    write('You dive behind a jagged boulder, its surface slick with glowing moss, your breath ragged as you press against the cold stone.'),
    !, nl.

go(tree) :-
    i_am_at(ledge),
    retract(i_am_at(ledge)),
    assert(i_am_at(tree)),
    retract(at(clara, ledge)),
    assert(at(clara, tree)),
    write('You approach the towering TREE, its presence both majestic and unsettling.'), nl,
    write('Thick vines and sturdy branches form a natural ladder, inviting you to climb into its heights.'), nl,
    !, nl.

go(ruins) :-
    i_am_at(tree),
    retract(i_am_at(tree)),
    assert(i_am_at(ruins)),
    retract(at(clara, tree)),
    assert(at(clara, ruins)),
    write('You weave through the dense undergrowth toward the RUINS, their stone facades echoing the grandeur of Egypt''s pyramids or the jungle temples of South America, yet twisted with an alien flair.'), nl,
    write('Intricate carvings of starships and celestial beings adorn the walls, hinting at a history far beyond human understanding.'), nl,
    write('As you step deeper, a tall, slender figure emerges, its luminous eyes studying you with quiet intrigue. The CREATURE gestures gracefully, inviting conversation.'),
    !, nl.

go(tunnel) :-
    i_am_at(city),
    retractall(task(_)),
    assert(task(tunnel)),
    retractall(i_am_at(_)),
    assert(i_am_at(tunnel)),
    write('You bolt through the undergrowth, the Nazis'' shouts and revving engines hot on your heels.'), nl,
    write('Thorns snag your clothes, tearing at your skin as you burst through the TUNNEL exit and emerge at the crash site, winded and desperate.'), nl,
    write('The icy wind bites at your face, a cruel reminder of the surface''s hostility.'), nl, !,
    (not(holding(radio)), tunnel_game_over; true),
    !.

go(tunnel) :-
    i_am_at(ruins),
    retractall(task(_)),
    assert(task(tunnel)),
    retractall(i_am_at(_)),
    assert(i_am_at(tunnel)),
    write('You bolt through the undergrowth, the Nazis'' shouts and revving engines hot on your heels.'), nl,
    write('Thorns snag your clothes, tearing at your skin as you burst through the TUNNEL exit and emerge at the crash site, winded and desperate.'), nl,
    write('The icy wind bites at your face, a cruel reminder of the surface''s hostility.'), nl, !,
    (not(holding(radio)), write('You and Clara huddle in the wreckage, the valley''s secrets slipping away as the cold closes in.'), nl,
    write('Survival hangs by a thread, your fate uncertain.'), nl,
    write('GAME OVER.'), nl; true),
    !.

go(tunnel) :-
    i_am_at(tree),
    (talked(clara, tunnel),
    write('We can''t GO to TUNNEL. We went to far.'), nl;
    true),
    (not(talked(clara, tunnel)),
    assert(talked(clara, tunnel)),
    write('Clara grabs your sleeve, her grip tight.'), nl,
    write('Clara: "Hold on! We can''t just run back now-there''s too much we don''t understand."'), nl,
    write('Your choices: '), nl,
    write('1. "You''re right. We need to explore and figure this out."'), nl,
    write('2. "No, it''s too risky. Let''s head back while we can."'), nl,
    read(Choice), nl,
    process_clara_tunnel_talk(Choice),
    !, nl; true),
    !.

go(city) :-
    i_am_at(tree),
    retract(i_am_at(tree)),
    assert(i_am_at(city)),
    write('You set off toward the CITY, its ominous skyline growing sharper with each step. Before you reach its perimeter, the growl of engines cuts through the stillness.'), nl,
    write('A division of Nazis on motorcycles bursts into view, their dust trails rising like storm clouds. Clara mutters under her breath, "Looks like we''ve got company-and they don''t seem friendly."'), nl,
    !, nazi_ambush.

go(Place) :-
    i_am_at(Here),
    path(Here, Place),
    retract(i_am_at(Here)),
    assert(i_am_at(Place)),
    !, look.

go(_) :-
    write('You can''t go there from here.'),
    !, nl.

nazi_ambush :-
    retractall(task(_)),
    assert(task(ambush_beginning)),
    write('The Nazis lock eyes on you, their motorcycles skidding to a halt in a crescent of dust and menace.'), nl,
    write('Their leader, a wiry man with a scar slashing across his cheek, leaps off his bike, his black uniform pristine despite the grime of the valley.'), nl,
    write('He levels a Luger at you, his voice a guttural snarl that cuts through the humid air.'), nl, nl,
    write('Nazi Leader: "Halt! Amerikanische Spione! Werft die Waffen nieder!"'), nl,
    write('Clara (whispering): "They think we are spies. They''ve got us wrong, but I doubt they''ll listen to reason."'), nl, nl,
    write('The air thickens with tension as the Nazis fan out, their boots crunching on the gravel, rifles glinting in the bioluminescent glow.'), nl,
    write('Above, a flying saucer hums into view, its searchlight slicing through the foliage like a predator''s gaze.'), nl,
    write('Time slows-your heart pounds, and the valley''s beauty fades behind the cold reality of danger.'),
    !, nl.

to_be_continued :-
    write('The CITY looms ahead, its dark spires piercing the bioluminescent sky like '), nl,
    write('jagged teeth. Clara stumbles beside you, her face pale but defiant, though her eyes betray a flicker of fear.'), nl, nl,
    write('You steal a glance at the leader, his scar twisting as he smirks, satisfied with '), nl,
    write('his prize. What awaits in the CITY? Interrogation? Imprisonment? Or something '), nl,
    write('far worse, tied to the secrets buried in this impossible valley? The questions '), nl,
    write('gnaw at you, but answers remain elusive, shrouded in the same mystery that'), nl,
    write('cloaks this hidden world.'), nl, nl,
    write('As the CITY gates creak open, swallowing you into its shadowed maw, one thought'), nl,
    write('lingers: this is not the end, but a dark new beginning. Your fate hangs in the '), nl,
    write('balance, and the next chapter of your journey waits just beyond the horizon.'), nl, nl,
    write('TO BE CONTINUED...'),
    asserta(user:finished_act(3)),
    !, nl.

tunnel_game_over :-
    write('You and Clara huddle in the wreckage, the valley''s secrets slipping away as the cold closes in.'), nl,
    write('Survival hangs by a thread, your fate uncertain.'), nl,
    write('GAME OVER.'),
    asserta(user:finished_act(3)),
    !, nl.

radio_game_end :-
    assert(i_am_at(marines_plane)),
    write('After a tense wait, the roar of engines fills the air. A Marine transport plane descends through the snow, its lights cutting through the gloom.'), nl,
    write('You and CLARA board, the warmth of the cabin a stark contrast to the biting cold.'), nl,
    write('As the plane lifts off, a Marine hands you a stack of nondisclosure agreements.'), nl, nl,
    write('Marine: "Sign these. What you saw down there stays buried. Understood?"'), nl, nl,
    write('You nod, a heavy, unspoken weight settling over you.'), nl,
    write('The valley''s mysteries fade into the distance, shrouded in silence, as the plane carries you away.'), nl,
    retractall(task(_)),
    assert(task(after_radio)),
    !, nl.
