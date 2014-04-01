:- module(pp, [start_pp/0,
               stop_pp/0,
               push_pp/0,
               pop_pp/0]).
:- use_module(plterms, [pretty_ground_term/2]).
:- use_module(ast,     [ml_const/1,
                        ml_id/1,
                        string/1]).
:- use_module(library(avl), [avl_to_list/2]).

:- multifile user:portray/1.
:- dynamic user:portray/1.

put_indent_pp(I) :-
        bb_put(pp_e_indent, I).
get_indent_pp(I) :-
        (   bb_get(pp_e_indent, I) ->
            true
        ;   I = ""
        ).

push_pp :- push_pp(_, _).
push_pp(I, J) :-
        get_indent_pp(I),
        append(I, "  ", J),
        put_indent_pp(J).

pop_pp :-
        get_indent_pp(I),
        append(J, "  ", I),
        put_indent_pp(J).

stop_pp :-
        retractall(portray(_)).
start_pp :-
        assert((
user:portray(Term) :-
        (   compound(Term) ->
            (   Term = (Head:-Body) ->
                get_indent_pp(I),
                format('~s', [I]),
                print(Head),
                write(' :- '),
                print(Body),
                write('.')
            ;   Term =.. ['\\+'|[A]] ->
                format('\\+~p', [A])
            ;   Term = (A -> B ; C) ->
                format('(~p -> ~p ; ~p)', [A,B,C])
            ;   Term =.. [';'|[A|Args]] ->
                format('(~p', [A]),
                (   foreach(Ai, Args)
                do  write(' ; '),
                    print(Ai)
                ),
                write(')')
            ;   Term =.. [','|[A|Args]] ->
                print(A),
                (   foreach(Ai, Args)
                do  write(', '),
                    print(Ai)
                )
            ;   (   Term = _@_:_:_-->_
                ;   Term = _@_:_:_
                ;   Term = _@_:_ ) ->
                pretty_ground_term(Term, Ground),
                pp_e(Ground)
            ;   Term = X:(T1->T2) ->
                format('~p:(', [X]),
                print(T1),
                write(' -> '),
                print(T2),
                write(')')
            ;   Term = (T1->T2) ->
                paren_t(T1),
                write(' -> '),
                print(T2)
            ;   Term = node(_, _, _, _, _) ->
                avl_to_list(Term, List),
                get_indent_pp(I),
                append(I, "          ", J),
                put_indent_pp(J),
                (   foreach(X-(K,E), List)
                do  format("~s~p |->\n~s       K: ~p\n~s  ^E@L:N:\n~p", [I, X, I, K, I, E])
                ),
                put_indent_pp(I)
            ;   Term =.. [F|Args], F \== '.',
                (   Args = [A] ->
                    format('~q(~p)', [F, A])
                ;   \+ (   current_op(_, T, F),
                           ( T == xfx ; T == xfy ; T == yfx ) ),
                    A =.. [','|Args],
                    format('~q(~p)', [F, A])
                )
            )
        )
        )).

/*
paren_t(+T)
*/
paren_t(T) :-
        (   compound(T), T = (_->_) ->
            write('('),
            print(T),
            write(')')
        ;   write(T)
        ).

pp_const_parenthesis(+).
pp_const_parenthesis(-).
pp_const_parenthesis(*).
pp_const_parenthesis(/).
pp_const_parenthesis(=).
pp_const_parenthesis(<>).
pp_const_parenthesis(>).
pp_const_parenthesis(<).
pp_const_parenthesis(>=).
pp_const_parenthesis(<=).
pp_const_parenthesis(&&).

pp_e(ELN-->K) :- !,
        pp_e(ELN),
        write(' --> '),
        print(K).
pp_e(E@_:X:T) :- !,
        pp_e(E),
        write(':'),
        print(X:T).
pp_e(E@_:T) :- !,
        pp_e(E),
        write(':'),
        paren_t(T).
pp_e(app(Ef, Es)) :- !,
        push_pp(I, _),
        format('~s(\n', [I]),
        pp_e(Ef),
        (   foreach(Ei, Es)
        do  write('\n'),
            pp_e(Ei)
        ),
        format('\n~s)', [I]),
        pop_pp.
pp_e(abs(Xs, Eb)) :- !,
        push_pp(I, _),
        format('~s(fun\n', [I]),
        (   Xs = [Xh|Xr] ->
            pp_e(Xh),
            (   foreach(X, Xr)
            do  write('\n'),
                pp_e(X)
            )
        ;   Xs = [X] ->
            pp_e(X)
        ),
        format('\n~s->\n', [I]),
        print(Eb),
        format('\n~s)', [I]),
        pop_pp.
pp_e(ite(E1, E2, E3)) :- !,
        push_pp(I, _),
        format('~s(if\n', [I]),
        print(E1),
        format('\n~sthen\n', [I]),
        print(E2),
        format('\n~selse\n', [I]),
        print(E3),
        format('\n~s)', [I]),
        pop_pp.
pp_e(let(X, E1, E2)) :- !,
        push_pp(I, _),
        format('~s(let\n', [I]),
        print(X),
        format('\n~s=\n', [I]),
        print(E1),
        format('\n~sin\n', [I]),
        print(E2),
        format('\n~s)', [I]),
        pop_pp.
pp_e(assert(Ec)) :- !,
        push_pp(I, _),
        (   compound(Ec),
            ( Ec = E@_:_:_-->_
            ; Ec = E@_:_:_
            ; Ec = E@_:_ ),
            ( E == true ; E == false ) ->
            format('~s(assert(\n', [I]),
            print(Ec),
            format('\n~s))', [I])
        ;   format('~s(assert\n', [I]),
            print(Ec),
            format('\n~s)', [I])
        ),
        pop_pp.
pp_e(assume(Ec)) :- !,
        push_pp(I, _),
        (   compound(Ec),
            ( Ec = E@_:_:_-->_
            ; Ec = E@_:_:_
            ; Ec = E@_:_ ),
            ( E == true ; E == false ) ->
            format('~s(assume(\n', [I]),
            print(Ec),
            format('\n~s))', [I])
        ;   format('~s(assume\n', [I]),
            print(Ec),
            format('\n~s)', [I])
        ),
        pop_pp.
pp_e(E) :-
        push_pp(I, _),
        format('~s', [I]),
        (   pp_const_parenthesis(E) ->
            write('('),
            write(E),
            write(')')
        ;   string(E) ->
            format("\"~s\"", [E])
        ;   ( ml_const(E) ; ml_id(E) ) ->
            write(E)
        ),
        pop_pp.
