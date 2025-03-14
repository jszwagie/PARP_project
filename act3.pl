:- module(act3, []).

initialize_act :-
    retractall(i_am_at(_)),
    assert(i_am_at(ledge)),
    assert(task(ledge_talk)),
    assert(at(clara, ledge)).

/* Define locations */
location(ledge).
location(tree).
location(ruins).
location(tunnel).
location(city).

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


/* Hint system */
hint :-
    i_am_at(ledge),
    task(ledge_talk),
    write('I should talk to Clara.'), 
    nl, !.

hint :-
    i_am_at(ledge),
    described(tree),
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

creature_disappears :-
    i_am_at(ruins),
    at(creature, ruins),
    retract(at(creature, ruins)),
    write('*Before you can respond, the air splits with the roar of engines and sharp, guttural shouts. The ground trembles faintly—a prelude to chaos.*'), nl,
    write('Creature: "True, if what you say is, run immediately, you must. Farewell, my friends."'), nl,
    write('*The creature dissolves into the air, leaving you confused and on edge.*'), 
    !, nl.


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

examine(tree) :-
    write('The tree stands ancient and imposing, its roots plunging into the earth like the veins of the valley itself.'),
    nl, !.

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
    process_clara_tunnel_talk(Choice).

go(city) :-
    i_am_at(tree),
    write('You set off toward the CITY, its ominous skyline growing sharper with each step. Before you reach its perimeter, the growl of engines cuts through the stillness.'), nl,
    write('A division of Nazis on motorcycles bursts into view, their dust trails rising like storm clouds. Clara mutters under her breath, "Looks like we''ve got company—and they don''t seem friendly."'), 
    !, nl.

process_clara_tunnel_talk(1):
    write('You: "You''re right. We need to explore and figure this out."'), nl,
    write('*You stay in the valley, your resolve hardening.*'), 
    !, nl.

process_clara_tunnel_talk(2):
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
    write('Without a radio or supplies, we wouldn''t last a day on the surface. Our only chance is to keep moving, find shelter or someone who knows what''s going on—anything''s better than retreating empty-handed."'), nl,

go(Place) :-
    i_am_at(Here),
    path(Here, Place),
    retract(i_am_at(Here)),
    assert(i_am_at(Place)),
    !, look.

go(_) :-
    write('You can''t go there from here.'),
    !, nl.
