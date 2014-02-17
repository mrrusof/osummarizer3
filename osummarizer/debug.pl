:- module(debug, [start_debug/0, stop_debug/0,
                   if_debug/1, if_debug_nn/1,
                   dnl/0,
                   dwrite/1, dwrite/2,
                   dpush_write/1, dpush_write/2,
                   dpop_write/1, dpop_write/2,
                   dportray_clause/1, dportray_clause/2,
                   dpush_portray_clause/1, dpush_portray_clause/2,
                   dpop_portray_clause/1, dpop_portray_clause/2,
                   dprint/1, dprint/2,
                   dpush_print/1, dpush_print/2,
                   dpop_print/1, dpop_print/2,
                   dformat/2, dformat/3, dformat_nn/3,
                   dpush_format/2, dpush_format/3,
                   dpop_format/2, dpop_format/3
                  ], [hidden(true)]).
:- use_module('ext/utils/misc.pl', [init_counter/1, increment/2]).
:- use_module(library(lists), [last/3]).

start_debug :-
        bb_put(debug, 1),
        init_counter(debug_counter),
        bb_put(debug_indention, []).

stop_debug :-
	(   bb_delete(debug, _),
            bb_delete(debug_counter, _),
            bb_delete(debug_indention, _)
	;   true
	).

if_debug(C) :-
	(   bb_get(debug, 1) ->
	    call(user:C)
	;   true
	).

if_debug_nn(C) :-
	(   bb_get(debug, 1) ->
	    \+ \+ call(user:C)
	;   true
	).

push_indention(Out) :-
        bb_get(debug_indention, Iold),
        increment(debug_counter, C),
        last(Iold, C, I),
        bb_put(debug_indention, I),
        (   foreach(N, I),
            param(Out)
        do  format(Out, '~d  ', [N])
        ).
pop_indention(Out) :-
        bb_get(debug_indention, Iold),
        (   last(I, _, Iold) ->
            true
        ;   I = []
        ),
        bb_put(debug_indention, I),
        (   foreach(N, Iold),
            param(Out)
        do  dformat(Out, '~d  ', [N])
        ).

dnl :-  (   bb_get(debug, 1) ->
            nl
	;   true
	).

dwrite(A) :- dwrite(user_output, A).
dwrite(Out, A) :-
	(   bb_get(debug, 1) ->
	    write(Out, A)
	;   true
	).

dpush_write(A) :- dpush_write(user_output, A).
dpush_write(Out, A) :-
	(   bb_get(debug, 1) ->
            push_indention(Out),
            write(Out, A)
        ;   true
        ).
dpop_write(A) :- dpop_write(user_output, A).
dpop_write(Out, A) :-
        (   bb_get(debug, 1) ->
            pop_indention(Out),
            write(Out, A)
        ;   true
        ).

dportray_clause(A) :- dportray_clause(user_output, A).
dportray_clause(Out, A) :-
	(   bb_get(debug, 1) ->
	    portray_clause(Out, A)
	;   true
	).

dpush_portray_clause(A) :- dpush_portray_clause(user_output, A).
dpush_portray_clause(Out, A) :-
	(   bb_get(debug, 1) ->
            push_indention(Out),
            portray_clause(Out, A)
        ;   true
        ).
dpop_portray_clause(A) :- dpop_portray_clause(user_output, A).
dpop_portray_clause(Out, A) :-
        (   bb_get(debug, 1) ->
            pop_indention(Out),
            portray_clause(Out, A)
        ;   true
        ).

dprint(A) :- dprint(user_output, A).
dprint(Out, A) :-
	(   bb_get(debug, 1) ->
	    print(Out, A)
	;   true
	).

dpush_print(A) :- dpush_print(user_output, A).
dpush_print(Out, A) :-
	(   bb_get(debug, 1) ->
            push_indention(Out),
            print(Out, A)
        ;   true
        ).
dpop_print(A) :- dpop_print(user_output, A).
dpop_print(Out, A) :-
        (   bb_get(debug, 1) ->
            pop_indention(Out),
            print(Out, A)
        ;   true
        ).

dformat(A, B) :- dformat(user_output, A, B).
dformat(Out, A, B) :-
	(   bb_get(debug, 1) ->
	    format(Out, A, B)
	;   true
	).

dpush_format(A, B) :- dpush_format(user_output, A, B).
dpush_format(Out, A, B) :-
        (   bb_get(debug, 1) ->
            push_indention(Out),
            format(Out, A, B)
        ;   true
        ).

dpop_format(A, B) :- dpop_format(user_output, A, B).
dpop_format(Out, A, B) :-
        (   bb_get(debug, 1) ->
            pop_indention(Out),
            format(Out, A, B)
        ;   true
        ).

dformat_nn(Init, A, B) :-
	(   bb_get(debug, 1) ->
	    \+ \+ (
		    call(user:Init),
		    format(A, B),
		    flush_output
		  )
	;   true
	).
