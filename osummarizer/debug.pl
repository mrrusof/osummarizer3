:- module(debug, [start_debug/0, stop_debug/0,
                  dformat/2, dformat/3, dformat_nn/3,
                  dpop_format/2, dpop_format/3,
                  dpush_format/2, dpush_format/3,
                  dpop_print/1, dpop_print/2,
                  dpush_print/1, dpush_print/2,
                  dprint/1, dprint/2,
                  if_debug/1, if_debug_nn/1
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

dformat(A, B) :- dformat(user_output, A, B).
dformat(Out, A, B) :-
	(   bb_get(debug, 1) ->
	    format(Out, A, B),
	    flush_output
	;   true
	).

dpush_format(A, B) :- dpush_format(user_output, A, B).
dpush_format(Out, A, B) :-
        dpush_print(Out, ''),
        dformat(Out, A, B).

dpop_format(A, B) :- dpop_format(user_output, A, B).
dpop_format(Out, A, B) :-
        dpop_print(Out, ''),
        dformat(Out, A, B).

dformat_nn(Init, A, B) :-
	(   bb_get(debug, 1) ->
	    \+ \+ (
		    call(user:Init),
		    format(A, B),
		    flush_output
		  )
	;   true
	).

dprint(A) :- dprint(user_output, A).
dprint(Out, A) :-
	(   bb_get(debug, 1) -> 
	    print(Out, A),
	    flush_output
	;   true
	).

dpush_print(A) :- dpush_print(user_output, A).
dpush_print(Out, A) :-
	(   bb_get(debug, 1) ->         
            bb_get(debug_indention, Iold),
            increment(debug_counter, C),
            last(Iold, C, I),
            bb_put(debug_indention, I),
            (   foreach(N, I),
                param(Out)
            do  dformat(Out, '~d\t', [N])
            ),
            dformat(Out, '~p', [A])
        ;   true
        ).
dpop_print(A) :- dpop_print(user_output, A).
dpop_print(Out, A) :-
        (   bb_get(debug, 1) -> 
            bb_get(debug_indention, Iold),
            (   last(I, _, Iold) ->
                true
            ;   I = []
            ),
            bb_put(debug_indention, I),
            (   foreach(N, Iold),
                param(Out)
            do  dformat(Out, '~d\t', [N])
            ),
            dformat(Out, '~p', [A])
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
