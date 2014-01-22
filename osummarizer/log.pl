:- module(log, [start_log/0, stop_log/0,
                lformat/2, lformat/3, lformat_nn/3,
                lpop_format/2, lpop_format/3,
                lpush_format/2, lpush_format/3,
                lpop_print/1, lpop_print/2,
                lpush_print/1, lpush_print/2,
                lprint/1, lprint/2,
                if_log/1, if_log_nn/1
               ], [hidden(true)]).
:- use_module('ext/utils/misc.pl', [init_counter/1, increment/2]).
:- use_module(library(lists), [last/3]).

start_log :-
        bb_put(log, 1),
        init_counter(log_counter),
        bb_put(log_indention, []).

stop_log :-
	(   bb_delete(log, _),
            bb_delete(log_counter, _),
            bb_delete(log_indention, _)
	;   true
	).

lformat(A, B) :- lformat(user_output, A, B).
lformat(Out, A, B) :-
	(   bb_get(log, 1) ->
	    format(Out, A, B),
	    flush_output
	;   true
	).

lpush_format(A, B) :- lpush_format(user_output, A, B).
lpush_format(Out, A, B) :-
        lpush_print(Out, ''),
        lformat(Out, A, B).

lpop_format(A, B) :- lpop_format(user_output, A, B).
lpop_format(Out, A, B) :-
        lpop_print(Out, ''),
        lformat(Out, A, B).

lformat_nn(Init, A, B) :-
	(   bb_get(log, 1) ->
	    \+ \+ (
		    call(user:Init),
		    format(A, B),
		    flush_output
		  )
	;   true
	).

lprint(A) :- lprint(user_output, A).
lprint(Out, A) :-
	(   bb_get(log, 1) -> 
	    print(Out, A),
	    flush_output
	;   true
	).

lpush_print(A) :- lpush_print(user_output, A).
lpush_print(Out, A) :-
	(   bb_get(log, 1) ->         
            bb_get(log_indention, Iold),
            increment(log_counter, C),
            last(Iold, C, I),
            bb_put(log_indention, I),
            (   foreach(N, I),
                param(Out)
            do  lformat(Out, '~d\t', [N])
            ),
            lformat(Out, '~p', [A])
        ;   true
        ).
lpop_print(A) :- lpop_print(user_output, A).
lpop_print(Out, A) :-
        (   bb_get(log, 1) -> 
            bb_get(log_indention, Iold),
            (   last(I, _, Iold) ->
                true
            ;   I = []
            ),
            bb_put(log_indention, I),
            (   foreach(N, Iold),
                param(Out)
            do  lformat(Out, '~d\t', [N])
            ),
            lformat(Out, '~p', [A])
        ;   true
        ).

if_log(C) :-
	(   bb_get(log, 1) ->
	    call(user:C)
	;   true
	).

if_log_nn(C) :-
	(   bb_get(log, 1) ->
	    \+ \+ call(user:C)
	;   true
	).
