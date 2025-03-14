:- module(act3, []).

:- dynamic(at/2).
:- dynamic(attempts/2).
:- dynamic(examined/1).

attempts(radio, 0).

wrong_count(A, B, C, Count) :-
    (A =:= 4 -> WA = 0 ; WA = 1),
    (B =:= 7 -> WB = 0 ; WB = 1),
    (C =:= 2 -> WC = 0 ; WC = 1),
    Count is WA + WB + WC.

increments_attempts(radio) :-
    attempts(radio, N),
    N1 is N + 1,
    retract(attempts(radio, N)),
    assert(attempts(radio, N1)).

radio_hint(radio) :-
    attempts(radio, Count),
    ( Count =:= 2 ->
        write('HINT: "The plaque mentions ''A=Even, B=Prime, C=Square.'' And the note says ''Four''s might''—could A be 4?"'), nl
    ; Count =:= 3 ->
        write('HINT: "Think it through: 4 is even, 7 is prime, and 2 ties to the square root of 4. That matches all the clues."'), nl
    ; true ).

initialize_act :-
    retractall(i_am_at(_)),
    assert(i_am_at(ledge)),
    assert(task(ledge_talk)),
    assert(at(clara, ledge)),
    assert(holding(pistol)), %temporarily
    assert(holding(radio)). %temporarily

/* Define locations */
location(ledge).
location(tree).
location(ruins).
location(tunnel).
location(city).
location(rock).

/* Define location of episodes characters */
at(creature, ruins).

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
    write('The valley pulses with life—chirping insects fill the air, leaves rustle in a gentle breeze, and the distant roar of an unseen beast sends a shiver down your spine.'), nl,
    write('The memory of your crash-landed supplies lingers, a heavy burden as you take your first cautious steps into this strange, uncharted world.'), nl,
    look.

/* This rule tells how to look about you. */
look :-
    i_am_at(Place),
    describe(Place),
    !, nl.

/* Describe locations */
describe(ledge) :-
    task(ledge_talk),
    write('You both stand on a rocky ledge overlooking a hidden realm—an expansive, verdant valley cradled beneath Antarctica''s icy crust.'), nl,
    write('Bioluminescent plants emit a soft, ethereal glow, casting light across towering ferns and crystalline rivers that shimmer like liquid glass.'), nl,
    write('The air hangs warm and humid, thick with the scent of exotic blooms, a jarring contrast to the frozen desolation above.'), nl,
    write('Flying saucers, eerily similar to the wreck you stumbled upon, glide silently through the skies, their presence a quiet warning of something watchful and alive down here.'), 
    !, nl.

describe(ledge) :-
    task(tree),
    assert(described(tree)),
    write('Ahead looms a massive TREE, its gnarled trunk wider than a barn, its branches clawing toward the cavern''s glowing ceiling.'), nl,
    write('Bioluminescent moss clings to its bark, pulsing faintly, while its leaves shimmer with an unearthly light, swaying as if whispering secrets to the wind.'),
    !, nl.

describe(tree) :-
    i_am_at(tree), 
    write('From the tree''s upper branches, the valley sprawls before you in breathtaking detail.'), nl,
    write('To the east, ancient-looking RUINS emerge from the foliage—crumbling pyramids and temples etched with cryptic symbols, remnants of a lost civilization.'), nl,
    write('To the west, the stark silhouette of a CITY cuts through the greenery, its dark gray buildings festooned with swastika flags fluttering ominously in the breeze, their bold red and black stark against the muted stone.'), nl,
    write('Behind you, the TUNNEL exit gapes like a dark maw, leading back to the frozen surface—a lifeline or a trap, depending on your next move.'),
    !, nl.

describe(tunnel) :-
    i_am_at(tunnel),
    task(tunnel),
    write('The crash site lies in ruins, the plane''s twisted metal half-buried in snow.'), nl,
    write('The wind howls mercilessly, and the sky above is a bleak, unforgiving gray.'), nl,
    write('Your breath fogs in the frigid air as you clutch the RADIO, your last lifeline.'), 
    !, nl.


/* Hint system */
hint :-
    task(woods),
    write('We should GO to the WOODS.'),
    !, nl.

hint :-
    i_am_at(tunnel),
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
    task(fight),
    write('I should hand the PISTOL to Clara.'),
    !, nl.

hint :-
    task(ambush_beginning),
    write('Maybe Clara knows what to do in this situation.'),
    !, nl.

hint :-
    task(radio), 
    examined(note),
    write('The note says ''Four''s might, Seven''s luck, Two''s the root.'' That could point to the settings for A, B, and C. The plaque might help confirm it.'),
    !, nl.

hint :-
    task(radio),
    write('I need to tune the dials to the right numbers to reach the Marines. The NOTE or the RADIO might hold the key.'),
    !, nl.

hint :-
    task(hide),
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
    write('I think I could GO up on that TREE.'), 
    nl, !.

hint :-
    i_am_at(ledge),
    task(tree),
    write('I should find a high place for recon.'), 
    nl, !.

hint :-
    i_am_at(tree), 
    write('We should decide where to go.'),
    !, nl.

hint :- 
    i_am_at(ruins),
    write('Is the TALK the answer?'),
    !, nl.

examine(tree) :-
    write('The tree stands ancient and imposing, its roots plunging into the earth like the veins of the valley itself.'),
    nl, !.

examine(radio) :-
    task(radio),
    holding(radio),
    write('The RADIO is a rugged military device, scratched and dented but still working.'), nl,
    write('Each dial can be set to a number between 1 and 9.'), nl,
    write('The dials click stiffly as you turn them. A small plaque beneath them reads: "Standard Marine Corps Protocol: A=Even, B=Prime, C=Square."'),
    !, nl.

examine(note) :-
    assert(examined(note)),
    task(radio),
    holding(radio),
    write('The note is weathered, its ink blurred but readable: "Marine Corps Frequency: Alpha-Bravo-Charlie. Remember the code: Four''s might, Seven''s luck, Two''s the root."'),
    !, nl.

use(pistol) :-
    holding(pistol),
    task(fight),
    retract(holding(pistol)),
    retract(task(fight)),
    assert(task(after_fight)),
    write('You hand the PISTOL to Clara.'), nl,
    write('She aims the old Mauser and squeezes the trigger—a sharp crack echoes through the valley, but the gun jams mid-shot, smoke curling from the barrel like a dying breath.'), nl,
    write('The Nazis roar in fury, their rifles spitting fire in response.'), nl,
    write('Bullets chip the rock, showering you with dust and shards.'), nl,
    write('The leader bellows, his voice thick with venom:'), nl,
    write('Nazi Leader: "Ihr wagt es, uns herauszufordern? Euer Blut wird dieses Tal beflecken!"'), 
    !, nl.

use(radio) :-
    holding(radio),
    task(tunnel),
    retractall(task(tunnel)),
    assert(task(radio)),
    write('The RADIO crackles in your hands, its three dials labeled A, B, and C glinting faintly in the dim light of the crash site.'), nl,
    write('Each dial can be set to a number between 1 and 9.'), nl,
    write('A faded, crumpled NOTE taped to the side reads: "Marine Corps Frequency: Alpha-Bravo-Charlie."'), nl,
    write('The wind howls outside, urging you to hurry.'), nl,
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
            write('The RADIO hums as it locks onto a strong signal. A clear voice cuts through:'), nl,
            write('"Mission 334, this is the 32nd Marine Corps. Coordinates received. Extraction team inbound. Over."'), nl,
            write('Clara: "Copy that. We''ll hold tight. Over."'), nl,
            write('Marine: "Copy that. We''re tracking your signal. Hold tight, over."'), nl,
            write('The steady signal brings a flicker of relief amidst the chaos.'), nl,
            retract(holding(radio)),
            assert(used(radio)),
            radio_game_end
        ; Count =:= 1 ->
            writeln('The RADIO picks up a faint Marine transmission, but it''s garbled:'),
            writeln('"Mission... [static]... coordinates... [static]... hold..."'),
            writeln('Clara: "Almost there, but it''s too weak. They won''t get our position like this."'),
            increment_attempts(radio),
            maybe_hint(radio)
        ; Count =:= 2 ->
            writeln('A sharp burst of static erupts from the RADIO, followed by a chilling German voice:'),
            writeln('"Achtung! Feindliche Übertragung entdeckt!"'),
            writeln('Clara: "That''s the Germans—they''ve intercepted us. We''ve got to fix this now!"'),
            increment_attempts(radio),
            maybe_hint(radio)
        ; Count =:= 3 ->
            writeln('The RADIO hisses with static, a grating buzz drowning out any signal.'),
            writeln('You: "Just noise. This isn''t the right frequency."'),
            increment_attempts(radio),
            maybe_hint(radio)
    ), nl.

radio_game_end :-
    write('After a tense wait, the roar of engines fills the air. A Marine transport plane descends through the snow, its lights cutting through the gloom.'), nl,
    write('You and Clara board, the warmth of the cabin a stark contrast to the biting cold.'), nl,
    write('As the plane lifts off, a Marine hands you a stack of nondisclosure agreements.'), nl,
    write('Marine: "Sign these. What you saw down there stays buried. Understood?"'), nl,
    write('You nod, the weight of the unknown pressing on your chest. The valley''s secrets remain hidden, and your journey ends in silence.'), nl,
    write('FALSE ENDING'), 
    !, nl.

talk(clara) :-
    task(after_fight),
    write('You: "What did he say?"'), nl,
    write('Clara: *breathing heavily* "Nothing good. I don''t know if we can get out of this alive."'), nl,
    write('Clara: *shouting in fright* "Wir kapitulieren! Halt!"'), nl,
    write('The soldiers cease fire, their eyes still burning with rage.'), nl,
    write('They swarm closer, boots pounding the earth like war drums, and you''re wrestled to the ground, wrists bound tight with rough cord.'), nl,
    write('Their treatment is brutal — fists and threats of execution, though they spare you for now, muttering darkly about your potential value.'), nl,
    write('They march you toward the CITY, their motorcycles roaring triumphantly.'), nl,
    write('*END OF ACT 3 or TO BE CONTINUED?"*'), 
    !, nl.


talk(clara) :-
    task(ambush_beginning),
    write('You: "What''s our move? They''re closing in fast."'), nl, 
    write('Clara: "We''re outgunned and outmanned. Fight, run, or surrender—you decide, but make it quick!"'), nl, 
    write('Your choices:'), nl, 
    write('1. "Let''s fight! I''ll hand you the PISTOL!"'), nl, 
    write('2. "Run for the TUNNEL—we can try the RADIO again!"'), nl, 
    write('3. "We surrender. Maybe we can talk our way out."'), nl, 
    write('4. "Into the WOODS—lose them in the trees!"'), nl,
    read(Choice),
    process_ambush(Choice).

talk(clara) :-
    i_am_at(ledge),
    write('Clara: "This place… it''s like stepping into a dream. Or maybe a nightmare—I can''t decide."'), nl,
    write('You: "It''s incredible—Byrd wasn''t exaggerating in that diary."'), nl,
    write('Clara: "Sure, but that diary was written before the war. No mention of Nazis anywhere in it. Do you think they beat us to this discovery?"'), nl,
    write('1. "Maybe they found it during their Antarctic expeditions in the ''30s."'), nl,
    write('2. "Or they stumbled across it after the war, looking for a place to hide."'), nl,
    read(Choice), !,
    process_clara_ledge_talk(Choice).

talk(clara) :- 
    i_am_at(ruins),
    write('Clara: "Do you think we should TALK to it?"'),
    !, nl.

talk(creature) :- 
    i_am_at(ruins),
    at(creature, ruins),
    write('*The creature''s voice resonates in your mind, a melodic hum that bypasses your ears entirely.*'), nl,
    write('Creature: "Wanderers, greetings. Sentinel of this realm, I am, keeper of wisdom older than your civilization, hmm.'), nl,
    write('Answers you seek, yes? Give them to you, I shall.'), nl,
    write('Tied to what you call ''Atlantis,'' our kin are, though lost to your tongue, our true name is.'), nl,
    write('Arrived, the ones you call ''Germans'' did, speaking of a great calamity they fled. Stewards of peace we are, granted them refuge, we did. Yet, tidings of their shadow you bear, hmm?"'), 
    nl, nl,
    write('Your choices: '), nl,
    write('1. "Those Germans—the Nazis—are monsters. They''ve waged war and killed millions."'), nl,
    write('2. "They''re exploiting you. They''ll strip this valley bare and leave nothing behind."'), nl,
    read(Choice),
    process_creature_talk(Choice).

process_creature_talk(1) :-
    i_am_at(ruins),
    at(creature, ruins),
    write('You: "Those Germans—the Nazis—are monsters. They''ve waged war and killed millions."'), nl,
    write('Creature: "Malice such, perceived it not, we did. Blinded us, our hospitality has, to their stain."'), nl,
    creature_disappears.

process_creature_talk(2) :-
    i_am_at(ruins),
    at(creature, ruins),
    write('You: "They''re exploiting you. They''ll strip this valley bare and leave nothing behind."'), nl,
    write('Creature: "Cloaked in deception, they are, then. Harmony we cherish, yet stirred by this threat, we are. Counsel, what offer you?"'), nl,
    creature_disappears.

process_ambush(1) :-
    retractall(task(_)),
    assert(task(hide)),
    write('You: "Let''s fight! I''ll hand you the PISTOL!"'), nl,
    write('Clara: "Here, give me the pistol and get behind that ROCK—now!"'), 
    !, nl.

process_ambush(2) :-
    retractall(task(_)),
    assert(task(tunnel)),
    (write('You: "To the TUNNEL - move'); holding(radio), write('We can try the RADIO one more time!')), nl,
    (holding(radio), write('Clara: "It''s a long shot, but let''s go!"'); not(holding(radio)), write('Clara: "Without the RADIO, we''ll just freeze out there. Terrible plan, but I''m with you."')),
    !, nl.

process_ambush(3) :-
    retractall(task(_)),
    write('You: "We surrender. Maybe we can talk our way out."'), nl,
    write('You raise your hands slowly. Clara mirrors your movement and calls out to the soldiers:'), nl, 
    write('Clara: "Wir kapitulieren! Kein Problem."'), nl,
    write('The Nazis lower their rifles slightly, though their glares remain sharp as knives. The leader smirks, holstering his Luger with a flourish.'), nl,
    write('Nazi Leader: "Kluger Schachzug, Amerikaner. Unser Kommandant möchte Sie unbedingt sehen."'), nl,
    write('They bind your hands with coarse rope, the knots biting into your wrists, and march you toward the CITY, their motorcycles roaring triumphantly.'), nl,
    write('*END OF ACT 3 or TO BE CONTINUED?"*'), nl,
    !, nl.

process_ambush(4) :-
    retractall(task(_)),
    assert(task(woods)),
    write('You: "Come on, to the WOODS—GO!"'), nl,
    !, nl.

creature_disappears :-
    i_am_at(ruins),
    at(creature, ruins),
    retract(at(creature, ruins)),
    write('*Before you can respond, the air splits with the roar of engines and sharp, guttural shouts. The ground trembles faintly—a prelude to chaos.*'), nl,
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
    write('The bioluminescent flora bathes the landscape in a shimmering, otherworldly hue, while the faint hum of the valley''s life—chirps, rustles, and distant cries—wraps around you like a living tapestry.'), nl,
    write('It''s a paradise untouched by time, yet the shadow of danger looms just out of sight.'), nl,
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
    write('Clara: "Don''t be naive, doc. That radio''s a piece of junk—half-static on a good day—and even if we got through, what then?'), nl,
    write('Help''s days away at best, assuming they don''t think we''re delusional. We''d be stuck out there, freezing to death, praying for a miracle.'), nl,
    write('No, we push forward, find answers, and make our own way out of this mess."'), 
    !, nl.

process_clara_tunnel_talk_radio :-
    write('Clara: "And what? Freeze out there with no plan? We''re in too deep to turn tail now.'), nl,
    write('Without a radio or supplies, we wouldn''t last a day on the surface. Our only chance is to keep moving, find shelter or someone who knows what''s going on—anything''s better than retreating empty-handed."'), 
    !, nl.

go(tunnel) :- 
    (i_am_at(ruins); i_am_at(city)),
    retractall(task(_)),
    assert(task(tunnel)),
    retractall(i_am_at(_)),
    assert(i_am_at(tunnel)),
    write('You bolt through the undergrowth, the Nazis'' shouts and revving engines hot on your heels.'), nl,
    write('Thorns snag your clothes, tearing at your skin as you burst through the TUNNEL exit and emerge at the crash site, winded and desperate.'), nl,
    write('The icy wind bites at your face, a cruel reminder of the surface''s hostility.'), nl, !,
    not(holding(radio)), write('You and Clara huddle in the wreckage, the valley''s secrets slipping away as the cold closes in.'), nl,
    write('Survival hangs by a thread, your fate uncertain.'), nl,
    write('GAME OVER.'), nl.

go(woods) :-
    (i_am_at(ruins); i_am_at(city)),
    task(woods),
    retractall(task(_)),
    write('You sprint into the dense forest, branches snapping underfoot as you weave through the shadows.'), nl,
    write('The Nazis'' shouts fade briefly—you dare to hope—until the sky hums with menace.'), nl,
    write('A flying saucer descends, its beam of light slashing through the canopy like a blade, pinning you in its merciless glare.'), nl,
    write('Nazi Pilot (over loudspeaker): "Kein Entkommen, ihr Narren! Das Reich sieht alles!"'), nl,
    write('Riflemen emerge from the trees, their grips iron as they drag you back to the group. They bind your hands with coarse rope and march you toward the CITY, their motorcycles roaring triumphantly.'), nl,
    write('*END OF ACT 3 or TO BE CONTINUED?"*'), 
    !, nl.

go(rock) :-
    (i_am_at(ruins); i_am_at(city)),
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
    i_am_at(tree), 
    write('Clara grabs your sleeve, her grip tight.'), nl,
    write('Clara: "Hold on! We can''t just run back now—there''s too much we don''t understand."'), nl,
    write('Your choices: '), nl,
    write('1. "You''re right. We need to explore and figure this out."'), nl,
    write('2. "No, it''s too risky. Let''s head back while we can."'), nl,
    read(Choice),
    process_clara_tunnel_talk(Choice),
    !, nl.

go(city) :-
    i_am_at(tree),
    write('You set off toward the CITY, its ominous skyline growing sharper with each step. Before you reach its perimeter, the growl of engines cuts through the stillness.'), nl,
    write('A division of Nazis on motorcycles bursts into view, their dust trails rising like storm clouds. Clara mutters under her breath, "Looks like we''ve got company—and they don''t seem friendly."'), nl,
    nazi_ambush.

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
    write('He levels a Luger at you, his voice a guttural snarl that cuts through the humid air.'), nl,
    write('Nazi Leader: "Halt! Amerikanische Spione! Werft die Waffen nieder!"'), nl,
    write('Clara (whispering): "They think we are spies. They''ve got us wrong, but I doubt they''ll listen to reason."'), nl,
    write('The air thickens with tension as the Nazis fan out, their boots crunching on the gravel, rifles glinting in the bioluminescent glow.'), nl,
    write('Above, a flying saucer hums into view, its searchlight slicing through the foliage like a predator''s gaze.'), nl,
    write('Time slows—your heart pounds, and the valley''s beauty fades behind the cold reality of danger.'), 
    !, nl.
