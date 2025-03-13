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

/* Define paths */
path(ledge, tree).
path(tree, ledge).
path(tree, ruins).


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

talk(clara) :-
    i_am_at(ledge),
    write('Clara: "This place… it''s like stepping into a dream. Or maybe a nightmare—I can''t decide."'), nl,
    write('You: "It''s incredible—Byrd wasn''t exaggerating in that diary."'), nl,
    write('Clara: "Sure, but that diary was written before the war. No mention of Nazis anywhere in it. Do you think they beat us to this discovery?"'), nl,
    write('1. "Maybe they found it during their Antarctic expeditions in the ''30s."'), nl,
    write('2. "Or they stumbled across it after the war, looking for a place to hide."'), nl,
    read(Choice), !,
    process_clara_ledge_talk(Choice).

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
go(Place) :-
    i_am_at(Here),
    path(Here, Place),
    retract(i_am_at(Here)),
    assert(i_am_at(Place)),
    !, look.

go(_) :-
    write('You can''t go there from here.'),
    !, nl.
