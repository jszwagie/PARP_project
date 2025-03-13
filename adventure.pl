:- use_module(act1, []).
:- use_module(act2, []).

/* Common predicates shared between acts */
:- dynamic i_am_at/1, at/2, holding/1, talked/2, examined/1,
           is_injured/1, described/1, crashed/0,
           entered/1, seen_aircraft/0, finished_act/1,
           task/1, current_act/1, can_take/1, used/1.

next_act(1, 2).
% next_act(2, 3).
% next_act(3, 4).

call_act(1) :- act1:start_act.
call_act(2) :- act2:start_act.
% call_act(3) :- act3:start_act.
% call_act(4) :- act4:start_act.

next :-
    finished_act(CurrentAct),
    (next_act(CurrentAct, NextAct) ->
        retract(current_act(CurrentAct)),
        assert(current_act(NextAct)),
        call_act(NextAct)
    ;
        write('The end'), nl
    ).

/* Common items */
supply(food).
supply(water).
supply(geiger).
supply(medkit).
supply(radio).
supply(gear).
supply(tools).

/* Main game instructions */
instructions :-
    nl,
    write('Enter commands using standard Prolog syntax.'), nl,
    write('Available commands are:'), nl,
    write('start.             -- to start the game from Act 1.'), nl,
    write('act2.              -- to start directly from Act 2.'), nl,
    write('describe.          -- to look around you.'), nl,
    write('go(Place).         -- to go to a place.'), nl,
    write('take(Object).      -- to pick up an object.'), nl,
    write('drop(Object).      -- to put down an object.'), nl,
    write('use(Object).       -- to use an object.'), nl,
    write('examine(Object).   -- to examine an object closely.'), nl,
    write('talk(Person).      -- to talk to someone.'), nl,
    write('inventory.         -- list currently held items.'), nl,
    write('hint.              -- to get a hint if you''re stuck.'), nl,
    write('instructions.      -- to see this message again.'), nl,
    write('halt.              -- to end the game and quit.'), nl,
    nl.

/* Entry points for each act */
start :-
    instructions,
    retractall(current_act(_)),
    assert(current_act(1)),
    act1:start_act.

act2 :-
    instructions,
    act1:cleanup,
    retractall(current_act(_)),
    assert(current_act(2)),
    % Check if we're coming from Act 1 or starting directly
    (finished_act(1) -> true ; setup_default_act2),
    act2:start_act.

/* Setup defaults for Act 2 if starting directly, mainly for denug purpose*/
setup_default_act2 :-
    retractall(holding(_)),
    assert(holding(medkit)),
    assert(holding(food)),
    assert(holding(water)),
    assert(holding(lighter)),
    write('You\'re starting directly at Act 2.'), nl,
    write('You can take 2 more items from these options:'), nl,
    write('1. GEIGER Counter'), nl,
    write('2. RADIO'), nl,
    write('3. Climbing GEAR'), nl,
    write('4. Navigation TOOLS'), nl,
    write('Choose your first item (1-4): '),
    read(Choice1),
    add_item(Choice1),
    write('Choose your second item (1-4): '),
    read(Choice2),
    add_item(Choice2).

add_item(1) :- assert(holding(geiger)), !.
add_item(2) :- assert(holding(radio)), !.
add_item(3) :- assert(holding(gear)), !.
add_item(4) :- assert(holding(tools)), !.
add_item(_) :- write('Invalid choice, defaulting to navigation tools.'), nl,
               assert(holding(tools)).

check_progress :-
    finished_act(1),
    !,
    write("Enter 'next.' to start next act.").

check_progress :-
    finished_act(2),
    write('Act 3 coming soon...'),
    !.

check_progress.

comment_take(food) :-
    write('Essential for survival. I don''t plan on starving out there.'),
    !, nl.

comment_take(water) :-
    write('Dehydration is just as dangerous as the cold.'),
    !, nl.

comment_take(geiger) :-
    write('If we''re dealing with something unnatural, this might be useful.'),
    !, nl.

comment_take(medkit) :-
    current_act(1) ->
        write('Better to be safe than sorry.') ;
        write('Thank God I took it.'),
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
    write('Maybe there''s another source where we''re headed.'),
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
    write('Maybe we won''t need these.'),
    !, nl.

list_items([]) :- !.
list_items([H|T]) :-
    upcase_atom(H, Uppercase),
    write('- '), write(Uppercase), nl,
    list_items(T).

inventory :-
    findall(X, holding(X), Inventory),
    list_items(Inventory).

/* get act number and use respective predicate */
examine(X) :-
    current_act(N),
    examine(N, X).

talk(X) :-
    current_act(N),
    talk(N, X).

take(X) :-
    current_act(N),
    take(N, X).

drop(X) :-
    current_act(N),
    drop(N, X).

go(X) :-
    current_act(N),
    go(N, X).

use(X) :-
    current_act(N),
    use(N, X).

look :-
    current_act(N),
    look(N).

hint :-
    current_act(N),
    hint(N).

examine(1, X) :- act1:examine(X).
examine(2, X) :- act2:examine(X).

talk(1, X) :- act1:talk(X).
talk(2, X) :- act2:talk(X).

take(1, X) :- act1:take(X).
take(2, X) :- act2:take(X).

drop(1, X) :- act1:drop(X).
drop(2, X) :- act2:drop(X).

go(1, X) :- act1:go(X).
go(2, X) :- act2:go(X).

use(1, _).
use(2, X) :- act2:use(X).

look(1) :- act1:look.
look(2) :- act2:look.

hint(1) :- act1:hint.
hint(2) :- act2:hint.