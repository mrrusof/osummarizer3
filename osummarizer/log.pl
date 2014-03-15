:- module(log, [start_log/0, stop_log/0,
                if_log/1, if_log_nn/1,
                lpush/0, lpop/0, lindent/0, lindent/1,
                lnl/0,
                lwrite/1, lwrite/2,
                lpush_write/1, lpush_write/2,
                lpop_write/1, lpop_write/2,
                lportray_clause/1, lportray_clause/2,
                lpush_portray_clause/1, lpush_portray_clause/2,
                lpop_portray_clause/1, lpop_portray_clause/2,
                lprint/1, lprint/2,
                lpush_print/1, lpush_print/2,
                lpop_print/1, lpop_print/2,
                lformat/2, lformat/3, lformat_nn/3,
                lpush_format/2, lpush_format/3,
                lpop_format/2, lpop_format/3
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

lpush_indent(Out) :-
        (   bb_get(log, 1) ->
            lpush,
            bb_get(log_indention, I),
            (   foreach(N, I),
                param(Out)
            do  format(Out, '~d  ', [N])
            )
        ;   true
        ).
lpop_indent(Out) :-
        (   bb_get(log, 1) ->
            bb_get(log_indention, Iold),
            lpop,
            (   foreach(N, Iold),
                param(Out)
            do  lformat(Out, '~d  ', [N])
            )
        ;   true
        ).

lpush :-
        (   bb_get(log, 1) ->
            bb_get(log_indention, Iold),
            increment(log_counter, C),
            last(Iold, C, I),
            bb_put(log_indention, I)
        ;   true
        ).
lpop :-
        (   bb_get(log, 1) ->
            bb_get(log_indention, Iold),
            (   last(I, _, Iold) ->
                true
            ;   I = []
            ),
            bb_put(log_indention, I)
        ;   true
        ).
lindent :- lindent(user_output).
lindent(Out) :-
        (   bb_get(log, 1) ->
            bb_get(log_indention, I),
            (   foreach(N, I),
                param(Out)
            do  lformat(Out, '~d  ', [N])
            )
        ;   true
        ).

lnl :-  (   bb_get(log, 1) ->
            nl
	;   true
	).

lwrite(A) :- lwrite(user_output, A).
lwrite(Out, A) :-
	(   bb_get(log, 1) ->
	    write(Out, A)
	;   true
	).

lpush_write(A) :- lpush_write(user_output, A).
lpush_write(Out, A) :-
	(   bb_get(log, 1) ->
            lpush_indent(Out),
            write(Out, A)
        ;   true
        ).
lpop_write(A) :- lpop_write(user_output, A).
lpop_write(Out, A) :-
        (   bb_get(log, 1) ->
            lpop_indent(Out),
            write(Out, A)
        ;   true
        ).

lportray_clause(A) :- lportray_clause(user_output, A).
lportray_clause(Out, A) :-
	(   bb_get(debug, 1) ->
	    portray_clause(Out, A)
	;   true
	).

lpush_portray_clause(A) :- lpush_portray_clause(user_output, A).
lpush_portray_clause(Out, A) :-
	(   bb_get(debug, 1) ->
            lpush_indent(Out),
            portray_clause(Out, A)
        ;   true
        ).
lpop_portray_clause(A) :- lpop_portray_clause(user_output, A).
lpop_portray_clause(Out, A) :-
        (   bb_get(debug, 1) ->
            lpop_indent(Out),
            potray_clause(Out, A)
        ;   true
        ).

lprint(A) :- lprint(user_output, A).
lprint(Out, A) :-
	(   bb_get(log, 1) ->
	    print(Out, A)
	;   true
	).

lpush_print(A) :- lpush_print(user_output, A).
lpush_print(Out, A) :-
	(   bb_get(log, 1) ->
            lpush_indent(Out),
            print(Out, A)
        ;   true
        ).
lpop_print(A) :- lpop_print(user_output, A).
lpop_print(Out, A) :-
        (   bb_get(log, 1) ->
            lpop_indent(Out),
            print(Out, A)
        ;   true
        ).

lformat(A, B) :- lformat(user_output, A, B).
lformat(Out, A, B) :-
	(   bb_get(log, 1) ->
	    format(Out, A, B)
	;   true
	).

lpush_format(A, B) :- lpush_format(user_output, A, B).
lpush_format(Out, A, B) :-
        (   bb_get(log, 1) ->
            lpush_indent(Out),
            format(Out, A, B)
        ;   true
        ).

lpop_format(A, B) :- lpop_format(user_output, A, B).
lpop_format(Out, A, B) :-
        (   bb_get(log, 1) ->
            lpop_indent(Out),
            format(Out, A, B)
        ;   true
        ).

lformat_nn(Init, A, B) :-
	(   bb_get(log, 1) ->
	    \+ \+ (
		    call(user:Init),
		    format(A, B),
		    flush_output
		  )
	;   true
	).
