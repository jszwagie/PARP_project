:- dynamic finished_act/1.

:- dynamic i_am_at/1, at/2, holding/1, talked/2, examined/1, task/1.
:- retractall(at(_, _)), retractall(i_am_at(_)), retractall(holding(_)),
   retractall(talked(_, _)), retractall(examined(_)), retractall(task(_)).

:- ensure_loaded('act1.pl').
:- ensure_loaded('act2.pl').


act2 :-
    start_act2.

start :-
    start_act1,
    (finished_act(1) -> act2 ; true).
