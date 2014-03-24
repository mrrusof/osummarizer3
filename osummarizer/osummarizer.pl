:- use_module('terms.pl').
:- use_module('log.pl', [start_log/0, if_log/1,
                         lpush/0, lpop/0, lindent/0,
                         lprint/1, lformat/2]).
:- use_module('debug.pl', [start_debug/0,
                           dpush_portray_clause/1, dpop_portray_clause/1,
                           dnl/0]).
:- use_module('ext/utils/misc.pl', [format_atom/3]).
:- use_module('ext/utils/list_utils.pl', [list2tuple/2,
                                          tuple2flatlist/2]).
:- use_module(library(avl), [avl_fetch/3, avl_store/4, avl_to_list/2]).
:- use_module(library(lists), [rev/2, maplist/3, include/3]).
:- use_module(library(ordsets), [ord_add_element/3, ord_union/2]).
:- use_module(library(terms), [term_variables/2]).

:- multifile user:portray/1.
:- dynamic user:portray/1.

:- op(540, xfy, @). % A@B:C == (A@B):C because current_op(N, _, :) ---> N = 550
:- op(560, xfy, -->). % A@B:C-->D == ((A@B):C)-->D because current_op(N, _, :) ---> N = 550



% **********************************************************************
% Syntax and well-formedness of typed expressions

/*

* Syntax

Expression e:
x                   ::=  lowercase atom
c                   ::=  path | pervasives | string
e, ..., e           ::=  tup([e, ..., e])
e e ... e           ::=  app(e, [e, ..., e])
fun x ... x -> e    ::=  abs([x, ..., x], e)
if e then e else e  ::=  ite(e, e, e)
let x = e in e      ::=  let(x, e, e)
let rec x = e
    and ... and
        x = e in e  ::=  ltr([(x, e), ..., (x, e)], e)
assert(e)           ::=  assert(e)
assume(e)           ::=  assume(e)

Expression e (continuation):
X                   ::=  capitalized atom
X(e, ..., e)        ::=
match e with
 | p when e -> e
 ...
 | p when e -> e    ::=  mae(e, [(p, e, e), ..., (p, e, e)])


Pattern p:
x                   ::=  lowercase atom
X(p, ..., p)        ::=  app(X, [p, ..., p])


Type t:
'a                  ::=  prolog var
b                   ::=  bool | int
t -> t              ::=  t -> t
t * ... * t         ::=  t * ... * t
X(t, ..., t)        ::=  X(t, ..., t)


Typed expression: substitute e:t for e in Expression.
Typed pattern: substitute p:t for p in Pattern.
*/

ml_const(C) :- ground(C), ( number(C) ; string(C) ; ml_const_pervasives(C) ; ml_const_path(C) ; ml_const_custom(C) ).
ml_const_custom(nondet).
ml_const_path('List.nil').
ml_const_path('List.cons').
ml_const_path('List.map').
ml_const_path('List.length').
ml_const_path('Random.int').
ml_const_pervasives(+).
ml_const_pervasives(-).
ml_const_pervasives(*).
ml_const_pervasives(/).
ml_const_pervasives(=).
ml_const_pervasives(<>).
ml_const_pervasives(>).
ml_const_pervasives(<).
ml_const_pervasives(>=).
ml_const_pervasives(<=).
ml_const_pervasives(&&).
ml_const_pervasives('||').
ml_const_pervasives('not').
ml_const_pervasives(true).
ml_const_pervasives(false).
ml_const_pervasives(unit).
ml_const_pervasives(read_int).

string(C) :- ( foreach(N, C) do number(N) ).

ml_id(X) :- atom(X), \+ml_const(X).

base_type(int).
base_type(bool).
base_type(unit).
base_type(string).
function_type(T) :- compound(T), T = (_->_).
nullary_type(T) :- base_type(T). % ( base_type(T) -> true ; T =..['*'|_] ).
type_var(T) :- var(T).
nullary_named_type(_:T) :- nullary_type(T).


/*

* Well-formedness of typed expressions

wf_typed_exp(+ELT)
*/
wf_typed_exp(E@L:T) :- wf_t_e(E, L, T).

/*
wf_t_e(+E, +L, +T)
*/
wf_t_e(app(Ef@Lf:Tf, ELTs), L, T) :- !,
        dpush_portray_clause(wf_t_e(app(Ef@Lf:Tf, ELTs), L, T)-in),
        wf_l(L),
        wf_t(T),
        wf_t_e(Ef, Lf, Tf),
        length(ELTs, Count),
        Count > 0,
        remove_formals_ty(Count, Tf, T),
        (   foreach(Ei@Li:Ti, ELTs)
        do  wf_t_e(Ei, Li, Ti)
        ),
        dpop_portray_clause(wf_t_e(app(Ef@Lf:Tf, ELTs), L, T)-out).
wf_t_e(abs(XLTs, Eb@Lb:Tb), L, T) :- !,
        dpush_portray_clause(wf_t_e(abs(XLTs, Eb@Lb:Tb), L, T)-in),
        wf_l(L),
        wf_t(T),
        XLTs = [_|_],
        (   foreach(Xi@Li:Ti, XLTs)
        do  ml_id(Xi),
            wf_l(Li),
            wf_t(Ti)
        ),
        wf_t_e(Eb, Lb, Tb),
        dpop_portray_clause(wf_t_e(abs(XLTs, Eb@Lb:Tb), L, T)-out).
wf_t_e(ite(E1@L1:T1, E2@L2:T2, E3@L3:T3), L, T) :- !,
        dpush_portray_clause(wf_t_e(ite(E1@L1:T1, E2@L2:T2, E3@L3:T3), L, T)-in),
        T1 == bool,
        wf_l(L),
        wf_t(T),
        T2 == T3,
        T == T3,
        wf_t_e(E1, L1, T1),
        wf_t_e(E2, L2, T2),
        wf_t_e(E3, L3, T3),
        dpop_portray_clause(wf_t_e(ite(E1@L1:T1, E2@L2:T2, E3@L3:T3), L, T)-out).
wf_t_e(let(X@Lx:Tx, E1@L1:T1, E2@L2:T2), L, T) :- !,
        dpush_portray_clause(wf_t_e(let(X@Lx:Tx, E1@L1:T1, E2@L2:T2), L, T)-in),
        wf_l(L),
        wf_t(T),
        ml_id(X),
        wf_l(Lx),
        wf_t(Tx),
        wf_t_e(E1, L1, T1),
        wf_t_e(E2, L2, T2),
        dpop_portray_clause(wf_t_e(let(X@Lx:Tx, E1@L1:T1, E2@L2:T2), L, T)-out).
wf_t_e(assert(Ec@Lc:Tc), L, T) :- !,
        dpush_portray_clause(wf_t_e(assert(Ec@Lc:Tc), L, T)-assert-in),
        T == unit,
        wf_t_e(Ec, Lc, Tc),
        wf_l(L),
        dpop_portray_clause(wf_t_e(assert(Ec@Lc:Tc), L, T)-assert-out).
wf_t_e(assume(Ec@Lc:Tc), L, T) :- !,
        dpush_portray_clause(wf_t_e(assume(Ec@Lc:Tc), L, T)-assume-in),
        T == unit,
        wf_t_e(Ec, Lc, Tc),
        wf_l(L),
        dpop_portray_clause(wf_t_e(assume(Ec@Lc:Tc), L, T)-assume-out).
wf_t_e(E, L, T) :-
        dpush_portray_clause(wf_t_e(E, L, T)-in),
        wf_l(L),
        wf_t(T),
        ( ml_const(E) ; ml_id(E) ),
        dpop_portray_clause(wf_t_e(E, L, T)-out).

/*
wf_l(+L)
*/
wf_l(loc(Name, Line1, LineOff1, Line2, LineOff2, Start, Stop)) :- !,
        atom(Name),
        number(Line1),
        number(LineOff1),
        number(Line2),
        number(LineOff2),
        number(Start),
        number(Stop).

/*
wf_t(+T)
*/
wf_t(T) :-
        (   compound(T) ->
            (   T = (T1->T2) ->
                wf_t(T1),
                wf_t(T2)
            % ;   T =.. ['*'|Ts],
            %     (   foreach(Ti, Ts)
            %     do  wf_t(Ti)
            %     )
            )
        ;   ( type_var(T) ; base_type(T) )
        ).

/*
remove_formals_ty(+Count, +T, -R) :-
*/
remove_formals_ty(Count, T, R) :-
	(   Count > 0 ->
	    T = (_->T2),
	    Count1 is Count-1,
	    remove_formals_ty(Count1, T2, R)
	;   R = T
	).



% **********************************************************************
% Pretty printing

pretty_ground_term(T1, T2) :-
        copy_term(T1, T2),
        term_variables(T2, Vars),
        (   foreach(V, Vars),
            count(N, 65, _)
        do  atom_codes(A, [N]),
            V = A
        ).

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
portray(Term) :-
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



% **********************************************************************
% Naming of typed expressions

/*
t_e_to_n_e1(+E, +L, +T, +X, +Env, -ELN)
*/
t_e_to_n_e1(app(Ef@Lf:Tf, ELTs), L, T, X, Env, app(Efn@Lf:Nf, ELNs)@L:X:Npre) :- !,
        dpush_portray_clause(t_e_to_n_e1(app(Ef@Lf:Tf, ELTs), L, T, X, Env, app(Efn@Lf:Nf, ELNs)@L:X:Npre)-in),
        (   ml_const(Ef) ->
            ml_const_to_name(Ef, C),
            format_atom('~p_~p', [C, X], Xf)
        ;   ml_id(Ef) ->
            format_atom('~p_~p', [Ef, X], Xf)
        ;   format_atom('f_~p', [X], Xf)
        ),
        t_e_to_n_e1(Ef, Lf, Tf, Xf, Env, Efn@Lf:PreNf),
        formals(PreNf, Fs),
        (   foreach(Ei@Li:Ti, ELTs),
            fromto(Fs, [Xi:_|OutFs], OutFs, _),
            foreach(ELNi, ELNs),
            count(_, 1, Count),
            param(Env)
        do  t_e_to_n_e1(Ei, Li, Ti, Xi, Env, ELNi)
        ),
        rev(ELNs, RevELNs),
        roots(Count, PreNf, Rs),
        rev(Rs, RevRs),
        remove_formals(Count, PreNf, _:Npre),
        (   foreach(_@_:Nj, RevELNs),
            fromto(RevRs, [Rj|OutRs], OutRs, _),
            fromto(X:Npre, InNf, Rj:(Nj->InNf), Nf)
        do  true
        ),
        dpop_portray_clause(t_e_to_n_e1(app(Ef@Lf:Tf, ELTs), L, T, X, Env, app(Efn@Lf:Nf, ELNs)@L:X:Npre)-out).
t_e_to_n_e1(abs(XLTs, Eb@Lb:Tb), L, T, X, Env, abs(XLNs, Ebr@Lbr:Nbr)@L:X:Npre) :- !,
        dpush_portray_clause(t_e_to_n_e1(abs(XLTs, Eb@Lb:Tb), L, T, X, Env, abs(XLNs, Ebr@Lbr:Nbr)@L:X:Npre)-in),
        (   foreach(Xi@Li:Ti, XLTs),
            foreach(Xi@Li:Ni, XLNs),
            fromto(Env, InEnv, OutEnv, Envb)
        do  name_type(Xi, Ti, Ni),
            avl_store(Xi, InEnv, Ni, OutEnv)
        ),
        format_atom('ret_~p', [X], Xb),
        t_e_to_n_e1(Eb, Lb, Tb, Xb, Envb, Ebr@Lbr:Nbr),
        rev(XLNs, RevXLNs),
        length(XLNs, Count),
        (   foreach(_Xj@_Lj:Nj, RevXLNs),
            fromto(Nbr, InNbr, Fj:(Nj -> InNbr), _:Npre),
            fromto(Count, InCount, OutCount, _),
            param(X)
        do  OutCount is InCount - 1,
            format_atom('f~d_~p', [OutCount, X], Fj)
        ),
        dpop_portray_clause(t_e_to_n_e1(abs(XLTs, Eb@Lb:Tb), L, T, X, Env, abs(XLNs, Ebr@Lbr:Nbr)@L:X:Npre)-out).
t_e_to_n_e1(ite(E1@L1:T1, E2@L2:T2, E3@L3:T3), L, T, X, Env, ite(R1, R2, R3)@L:N) :- !,
        dpush_portray_clause(t_e_to_n_e1(ite(E1@L1:T1, E2@L2:T2, E3@L3:T3), L, T, X, Env, ite(R1, R2, R3)@L:N)-in),
        format_atom('c_~p', [X], X1),
        t_e_to_n_e1(E1, L1, T1, X1, Env, R1),
        t_e_to_n_e1(E2, L2, T2, X,  Env, R2),
        t_e_to_n_e1(E3, L3, T3, X,  Env, R3),
        name_type(X, T, N),
        dpop_portray_clause(t_e_to_n_e1(ite(E1@L1:T1, E2@L2:T2, E3@L3:T3), L, T, X, Env, ite(R1, R2, R3)@L:N)-out).
t_e_to_n_e1(let(Y@Ly:Ty, E1@L1:T1, E2@L2:T2), L, T, X, Env, let(Y@Ly:Ny, ELN1, E2rL2r:N2)@L:N2) :- !,
        dpush_portray_clause(t_e_to_n_e1(let(Y@Ly:Ty, E1@L1:T1, E2@L2:T2), L, T, X, Env, let(Y@Ly:Ny, ELN1, E2rL2r:N2)@L:N2)-in),
        (   function_type(T1) ->
            ELN1 = E1@L1:T1,
            t_e_to_n_e1(E1, L1, T1, Y, Env, _:Ny)
        ;   t_e_to_n_e1(E1, L1, T1, Y, Env, ELN1),
            _:Ny = ELN1
        ),
        avl_store(Y, Env, Ny, Env1),
        t_e_to_n_e1(E2, L2, T2, X, Env1, E2rL2r:N2),
        dpop_portray_clause(t_e_to_n_e1(let(Y@Ly:Ty, E1@L1:T1, E2@L2:T2), L, T, X, Env, let(Y@Ly:Ny, ELN1, E2rL2r:N2)@L:N2)-out).
t_e_to_n_e1(assert(Ec@Lc:Tc), L, T, X, Env, assert(Rc)@L:N) :- !,
        dpush_portray_clause(t_e_to_n_e1(assert(Ec@Lc:Tc), L, T, X, Env, assert(Rc)@L:N)-in),
        format_atom('ase_~p', [X], Xc),
        t_e_to_n_e1(Ec, Lc, Tc, Xc, Env, Rc),
        name_type(X, T, N),
        dpop_portray_clause(t_e_to_n_e1(assert(Ec@Lc:Tc), L, T, X, Env, assert(Rc)@L:N)-out).
t_e_to_n_e1(assume(Ec@Lc:Tc), L, T, X, Env, assume(Rc)@L:N) :- !,
        dpush_portray_clause(t_e_to_n_e1(assume(Ec@Lc:Tc), L, T, X, Env, assume(Rc)@L:N)-in),
        format_atom('asu_~p', [X], Xc),
        t_e_to_n_e1(Ec, Lc, Tc, Xc, Env, Rc),
        name_type(X, T, N),
        dpop_portray_clause(t_e_to_n_e1(assume(Ec@Lc:Tc), L, T, X, Env, assume(Rc)@L:N)-out).
t_e_to_n_e1(E, L, T, X, Env, E@L:N) :-
        dpush_portray_clause(t_e_to_n_e1(E, L, T, X, Env, E@L:N)-id-cst-in),
        (   ml_const(E) ->
            (   function_type(T) ->
                name_type(X, T, N)
            ;   N = X:T
            )
        ;   ml_id(E) ->
            (   function_type(T) ->
                name_type(X, T, Nloc),
                avl_fetch(E, Env, Nenv),
                choose_names(Nenv, Nloc, _:Tn),
                N = X:Tn
            ;   N = X:T
            )
        ),
        dpop_portray_clause(t_e_to_n_e1(E, L, T, X, Env, E@L:N)-id-cst-out).

/*
name_type(+X, +T, -N)
*/
name_type(X, T, X:R) :-
        (   function_type(T) ->
            n_t1('', X, T, _:R)
        ;   R = T
        ).
n_t1(P, X, T, Y:R) :-
        format_atom('~p_~p', [P, X], Y),
        (   function_type(T) ->
            T = (T1->T2),
            format_atom('~pa', [P], P1),
            format_atom('~pb', [P], P2),
            n_t1(P1, X, T1, N1),
            n_t1(P2, X, T2, N2),
            R = (N1->N2)
        ;   R = T
        ).

/*
choose_names(+Nenv, +Nloc, -Nres)
*/
choose_names(X:S, Y:T, R) :-
        (   compound(S) ->
            S = (S1->S2),
            T = (T1->T2),
            choose_names(S1, T1, R1),
            choose_names(S2, T2, R2),
            R = X:(R1->R2)
        ;   (   compound(T) ->
                R = X:T
            ;   R = Y:T
            )
        ).

/*
formals(+N, -Formals)
*/
formals(_:T, Formals) :-
        (   compound(T), T = (R->T2) ->
            formals(T2, Rs),
            Formals = [R|Rs]
        ;   Formals = []
        ).

/*
roots(+Count, +N, -Roots)
*/
roots(Count, X:T, Roots) :-
        (   Count > 0, compound(T), T = (_->T2) ->
            Count1 is Count - 1,
            roots(Count1, T2, Rs),
            Roots = [X|Rs]
        ;   Roots = []
        ).

/*
remove_formals(+Count, +N, -R) :-
*/
remove_formals(Count, N, R) :-
	(   Count > 0 ->
	    N = _:(_->N2),
	    Count1 is Count-1,
	    remove_formals(Count1, N2, R)
	;   R = N
	).

/*
ml_const_to_name(?Const, ?Name)
*/
ml_const_to_name(+, plus).
ml_const_to_name(-, sub).
ml_const_to_name(*, mult).
ml_const_to_name(/, div).
ml_const_to_name(=, eq).
ml_const_to_name(<>, neq).
ml_const_to_name(>, gt).
ml_const_to_name(<, lt).
ml_const_to_name(>=, geq).
ml_const_to_name(<=, leq).
ml_const_to_name(not, not).
ml_const_to_name(&&, and).
ml_const_to_name('||', or).
ml_const_to_name(nondet, nondet).



% **********************************************************************
% Path of named expressions

/*
n_e_to_p_e1(+E, +L, +N, -ELNK)
*/
n_e_to_p_e1(app(nondet@Lf:Nf, [unit@Lu:Nu]), L, X:T, app(nondet@Lf:Nf, [unit@Lu:Nu])@L:X:T-->(Xu='_')) :- !,
        dpush_portray_clause(n_e_to_p_e1(app(nondet@Lf:Nf, [unit@Lu:Nu]), L, X:T, app(nondet@Lf:Nf, [unit@Lu:Nu])@L:X:T-->(Xu='_'))-in),
        uppercase_atom(X, Xu),
        dpop_portray_clause(n_e_to_p_e1(app(nondet@Lf:Nf, [unit@Lu:Nu]), L, X:T, app(nondet@Lf:Nf, [unit@Lu:Nu])@L:X:T-->(Xu='_'))-out).
n_e_to_p_e1(app(Ef@Lf:Xf:Tf, ELNs), L, X:T, app(Ekf@Lf:Xf:Tf, ELNKs)@L:X:T-->Kd) :- !,
        dpush_portray_clause(n_e_to_p_e1(app(Ef@Lf:Xf:Tf, ELNs), L, X:T, app(Ekf@Lf:Xf:Tf, ELNKs)@L:X:T-->Kd)-in),
        (   Ef == not ->
            Ekf = Ef,
            [E1@L1:X1:T1] = ELNs,
            n_e_to_p_e1(E1, L1, X1:T1, ELN1-->K1),
            ELNKs = [ELN1-->K1],
            uppercase_atom(X1, X1u),
            uppercase_atom(X, Xu),
            Kd = ((X1u=0 -> Xu=1 ; Xu=0), K1)
        ;   ( Ef == '&&' ; Ef == '||' ) ->
            Ekf = Ef,
            (   foreach(Ei@Li:Xi:Ti, ELNs),
                foreach(ELNKi, ELNKs),
                foreach(Xiu=1, Actuals),
                fromto([], InKs, OutKs, Ks)
            do  n_e_to_p_e1(Ei, Li, Xi:Ti, ELNKi),
                uppercase_atom(Xi, Xiu),
                (_-->Ki) = ELNKi,
                OutKs = [Ki|InKs]
            ),
            ml_const_to_prolog_const(Ef, Ep),
            formals(X:T, NFormals),
            (   foreach(N:_, NFormals),
                foreach(Nu=1, Formals)
            do  uppercase_atom(N, Nu)
            ),
            append(Actuals, Formals, AsFs),
            Cond =.. [Ep|AsFs],
            return(X:T, R:_),
            uppercase_atom(R, Ru),
            list2tuple([(Cond -> Ru=1 ; Ru=0)|Ks], Kd)
        ;   ( ml_const(Ef) ; ml_id(Ef) ) ->
            Ekf = Ef,
            (   foreach(Ei@Li:Xi:Ti, ELNs),
                foreach(ELNKi, ELNKs),
                fromto([], InAs, OutAs, RevActuals),
                fromto([], InKs, OutKs, Ks)
            do  n_e_to_p_e1(Ei, Li, Xi:Ti, ELNKi),
                (   compound(Ti) ->
                    OutAs = InAs,
                    OutKs = InKs
                ;   ml_const(Ei) ->
                    (_-->(_=Ai)) = ELNKi,
                    OutAs = [Ai|InAs],
                    OutKs = InKs
                ;   ml_id(Ei) ->
                    uppercase_atom(Ei, Ai),
                    OutAs = [Ai|InAs],
                    OutKs = InKs
                ;   uppercase_atom(Xi, Ai),
                    (_-->Ki) = ELNKi,
                    OutAs = [Ai|InAs],
                    OutKs = [Ki|InKs]
                )
            ),
            formals(X:T, NFormals),
            maplist(name_of_type, NFormals, Formals),
            maplist(uppercase_atom, Formals, UFormals),
            rev(RevActuals, Actuals),
            append(Actuals, UFormals, AsFs),
            return(X:T, R:B),
            uppercase_atom(R, Ru),
            (   ml_const(Ef) ->
                (   ml_const_to_prolog_const(Ef, Ep) ->
                    Call =.. [Ep|AsFs]
                ;   Call =.. [Ef|AsFs]
                ),
                (   ( B == bool ; B == unit ) ->
                    list2tuple([(Call -> Ru=1 ; Ru=0)|Ks], Kd)
                ;   list2tuple([Ru=Call|Ks], Kd)
                )
            ;   summ_sy(Ef:Tf, Ssy),
                append(AsFs, [Ru], AsFsR),
                Call =.. [Ssy|AsFsR],
                list2tuple([Call|Ks], Kd)
            )
        ),
        dpop_portray_clause(n_e_to_p_e1(app(Ef@Lf:Xf:Tf, ELNs), L, X:T, app(Ekf@Lf:Xf:Tf, ELNKs)@L:X:T-->Kd)-out).
n_e_to_p_e1(abs(XLTs, Eb@Lb:Nb), L, N, abs(XLTs, ELNb-->Kb)@L:N-->Kb) :- !,
        dpush_portray_clause(n_e_to_p_e1(abs(XLTs, Eb@Lb:Nb), L, N, abs(XLTs, ELNb-->Kb)@L:N-->Kb)-in),
        n_e_to_p_e1(Eb, Lb, Nb, ELNb-->Kb),
        dpop_portray_clause(n_e_to_p_e1(abs(XLTs, Eb@Lb:Nb), L, N, abs(XLTs, ELNb-->Kb)@L:N-->Kb)-out).
n_e_to_p_e1(ite(E1@L1:X1:T1, E2@L2:N2, E3@L3:N3), L, N, ite(E1L1N1-->K1, E2L2N2-->K2, E3L3N3-->K3)@L:N-->((X1u=1 -> K2 ; K3), K1)) :- !,
        dpush_portray_clause(n_e_to_p_e1(ite(E1@L1:X1:T1, E2@L2:N2, E3@L3:N3), L, N, ite(E1L1N1-->K1, E2L2N2-->K2, E3L3N3-->K3)@L:N-->((X1u=1 -> K2 ; K3), K1))-in),
        uppercase_atom(X1, X1u),
        n_e_to_p_e1(E1, L1, X1:T1, E1L1N1-->K1),
        n_e_to_p_e1(E2, L2, N2, E2L2N2-->K2),
        n_e_to_p_e1(E3, L3, N3, E3L3N3-->K3),
        dpop_portray_clause(n_e_to_p_e1(ite(E1@L1:X1:T1, E2@L2:N2, E3@L3:N3), L, N, ite(E1L1N1-->K1, E2L2N2-->K2, E3L3N3-->K3)@L:N-->((X1u=1 -> K2 ; K3), K1))-out).
n_e_to_p_e1(let(YLyNy, ELN1, E2@L2:N2), L, N2, let(YLyNy, ELNK1, E2L2N2-->K2)@L:N2-->Kd) :- !,
        dpush_portray_clause(n_e_to_p_e1(let(YLyNy, ELN1, E2@L2:N2), L, N2, let(YLyNy, ELNK1, E2L2N2-->K2)@L:N2-->Kd)-in),
        n_e_to_p_e1(E2, L2, N2, E2L2N2-->K2),
        (   E1@L1:X1:T1 = ELN1 ->
            n_e_to_p_e1(E1, L1, X1:T1, ELNK1),
            _-->K1 = ELNK1,
            mk_conj((K2, K1), Kd)
        ;   ELNK1 = ELN1,
            Kd = K2
        ),
        dpop_portray_clause(n_e_to_p_e1(let(YLyNy, ELN1, E2@L2:N2), L, N2, let(YLyNy, ELNK1, E2L2N2-->K2)@L:N2-->Kd)-out).
n_e_to_p_e1(assert(Ec@Lc:Xc:Tc), L, N, assert(ELNc-->Kc)@L:N-->(Xcu=1, Kc)) :- !,
        dpush_portray_clause(n_e_to_p_e1(assert(Ec@Lc:Xc:Tc), L, N, assert(ELNc-->Kc)@L:N-->(Xcu=1, Kc))-in),
        n_e_to_p_e1(Ec, Lc, Xc:Tc, ELNc-->Kc),
        uppercase_atom(Xc, Xcu),
        dpop_portray_clause(n_e_to_p_e1(assert(Ec@Lc:Xc:Tc), L, N, assert(ELNc-->Kc)@L:N-->(Xcu=1, Kc))-out).
n_e_to_p_e1(assume(Ec@Lc:Xc:Tc), L, N, assume(ELNc-->Kc)@L:N-->(Xcu=1, Kc)) :- !,
        dpush_portray_clause(n_e_to_p_e1(assume(Ec@Lc:Xc:Tc), L, N, assume(ELNc-->Kc)@L:N-->(Xcu=1, Kc))-in),
        n_e_to_p_e1(Ec, Lc, Xc:Tc, ELNc-->Kc),
        uppercase_atom(Xc, Xcu),
        dpop_portray_clause(n_e_to_p_e1(assume(Ec@Lc:Xc:Tc), L, N, assume(ELNc-->Kc)@L:N-->(Xcu=1, Kc))-out).
n_e_to_p_e1(E, L, X:T, E@L:X:T-->Kd) :-
        dpush_portray_clause(n_e_to_p_e1(E, L, X:T, ELNK)-id-cst-in),
        (   E == not ->
            (V:bool -> R:bool) = T,
            maplist(uppercase_atom, [V, R], [Vu, Ru]),
            Kd = (Vu=0 -> Ru=1 ; Ru=0)
        ;   E == '&&' ->
            (A:bool -> _:(B:bool -> R:bool)) = T,
            maplist(uppercase_atom, [A, B, R], [Au, Bu, Ru]),
            Kd = (Au=1, Bu=1 -> Ru=1 ; Ru=0)
        ;   E == '||' ->
            (A:bool -> _:(B:bool -> R:bool)) = T,
            maplist(uppercase_atom, [A, B, R], [Au, Bu, Ru]),
            Kd = ((Au=1 ; Bu=1) -> Ru=1 ; Ru=0)
        ;   ml_const(E) ->
            (   function_type(T) ->
                formals(X:T, NFormals),
                maplist(name_of_type, NFormals, Formals),
                maplist(uppercase_atom, Formals, UFormals),
                (   ml_const_to_prolog_const(E, Ep) ->
                    Call =.. [Ep|UFormals]
                ;   Call =.. [E|UFormals]
                ),
                return(X:T, Y:R),
                uppercase_atom(Y, Yu),
                (   ( R == bool ; R == unit ) ->
                    Kd = (Call -> Yu=1 ; Yu=0)
                ;   Kd = (Yu=Call)
                )
            ;   uppercase_atom(X, Xu),
                (   ml_const_to_prolog_const(E, C) ->
                    Kd = (Xu=C)
                ;   Kd = (Xu=E)
                )
            )
        ;   ml_id(E) ->
            (   function_type(T) ->
                mk_summ_pred(E:T, Kd)
            ;   uppercase_atom(E, Eu),
                uppercase_atom(X, Xu),
                Kd = (Xu=Eu)
            )
        ),
        dpop_portray_clause(n_e_to_p_e1(E, L, X:T, ELNK)-id-cst-out).

/*
ml_const_to_prolog_const(?ML, ?Prolog)
*/
ml_const_to_prolog_const(unit, 1).
ml_const_to_prolog_const(true, 1).
ml_const_to_prolog_const(false, 0).
ml_const_to_prolog_const(<>, =\=).
ml_const_to_prolog_const(>=, >=).
ml_const_to_prolog_const(<=, =<).
ml_const_to_prolog_const('&&', ',').
ml_const_to_prolog_const('||', ';').

/*
return(+N, -R)
*/
return(X:T, R) :-
	(   compound(T), T = (_->T2) ->
            return(T2, R)
	;   R = X:T
	).



% **********************************************************************
% Procedure definitions of expressions

/*
p_e_to_p_d1(+E, +L, +N, +K, +Kd, +D, -Dd)
*/

p_e_to_p_d1(app(nondet@Lf:Nf, [unit@Lu:Nu]), L, X:T, K, (Xu='_'), D, D) :- !,
        dpush_portray_clause(p_e_to_p_d1(app(nondet@Lf:Nf, [unit@Lu:Nu]), L, X:T, K, (Xu='_'), D, D)-nondet-in),
        dpop_portray_clause(p_e_to_p_d1(app(nondet@Lf:Nf, [unit@Lu:Nu]), L, X:T, K, (Xu='_'), D, D)-nondet-out).
p_e_to_p_d1(app(ELNKf, ELNKs), L, N, K, Kd, D, Dd) :- !,
        dpush_portray_clause(p_e_to_p_d1(app(ELNKf, ELNKs), L, N, K, Kd, D, Dd)-in),
        (   (Ef@Lf:Nf-->Kf) = ELNKf ->
            throw('head of application is compound?'),
            p_e_to_p_d1(Ef, Lf, Nf, K, Kf, D, Ddf)
        ;   Ef@Lf:Nf = ELNKf,
            p_e_to_p_d1(Ef, Lf, Nf, K, true, D, Ddf)
        ),
        (   foreach(Ei@Li:Ni-->Kdi, ELNKs),
            fromto(Ddf, InD, OutD, Dd),
            param(K)
        do  p_e_to_p_d1(Ei, Li, Ni, K, Kdi, InD, OutD)
        ),
        dpop_portray_clause(p_e_to_p_d1(app(ELNKf, ELNKs), L, N, K, Kd, D, Dd)-out).
p_e_to_p_d1(abs(XLTs, Eb@Lb:Nb-->Kb), L, N, K, Kd, D, Dd) :- !,
        dpush_portray_clause(p_e_to_p_d1(abs(XLTs, Eb@Lb:Nb-->Kb), L, N, K, Kd, D, Dd)-in),
        p_e_to_p_d1(Eb, Lb, Nb, K, Kb, D, Dd),
        dpop_portray_clause(p_e_to_p_d1(abs(XLTs, Eb@Lb:Nb-->Kb), L, N, K, Kd, D, Dd)-out).
p_e_to_p_d1(ite(E1@L1:N1-->K1, E2@L2:N2-->K2, E3@L3:N3-->K3), L, N, K, Kd, D, Dd) :- !,
        dpush_portray_clause(p_e_to_p_d1(ite(E1@L1:N1-->K1, E2@L2:N2-->K2, E3@L3:N3-->K3), L, N, K, Kd, D, Dd)-in),
        p_e_to_p_d1(E1, L1, N1, K, K1, D, D1),
        p_e_to_p_d1(E1, L1, N1, (X1=1, K1, K), K1, D1, D2),
        p_e_to_p_d1(E1, L1, N1, (X1=0, K1, K), K1, D2, Dd),
        dpop_portray_clause(p_e_to_p_d1(ite(E1@L1:N1-->K1, E2@L2:N2-->K2, E3@L3:N3-->K3), L, N, K, Kd, D, Dd)-out).
p_e_to_p_d1(let(Y@Ly:Ny, ELNK1, E2@L2:N2-->K2), L, N, K, Kd, D, Dd) :- !,
        dpush_portray_clause(p_e_to_p_d1(let(Y@Ly:Ny, ELNK1, E2@L2:N2-->K2), L, N, K, Kd, D, Dd)-in),
        (   E1@L1:N1-->K1 = ELNK1 ->
            p_e_to_p_d1(E1, L1, N1, K, K1, D, D1),
            p_e_to_p_d1(E2, L2, N2, (K1, K), K2, D1, Dd)
        ;   avl_store(Y, D, (K, ELNK1), D1),
            p_e_to_p_d1(E2, L2, N2, K, K2, D1, Dd)
        ),
        dpop_portray_clause(p_e_to_p_d1(let(Y@Ly:Ny, ELNK1, E2@L2:N2-->K2), L, N, K, Kd, D, Dd)-out).
p_e_to_p_d1(assert(Ec@Lc:Nc-->Kc), L, N, K, Kd, D, Dd) :- !,
        dpush_portray_clause(p_e_to_p_d1(assert(Ec@Lc:Nc-->Kc), L, N, K, Kd, D, Dd)-in),
        p_e_to_p_d1(Ec, Lc, Nc, K, Kc, D, Dd),
        dpop_portray_clause(p_e_to_p_d1(assert(Ec@Lc:Nc-->Kc), L, N, K, Kd, D, Dd)-out).
p_e_to_p_d1(assume(Ec@Lc:Nc-->Kc), L, N, K, Kd, D, Dd) :- !,
        dpush_portray_clause(p_e_to_p_d1(assume(Ec@Lc:Nc-->Kc), L, N, K, Kd, D, Dd)-in),
        p_e_to_p_d1(Ec, Lc, Nc, K, Kc, D, Dd),
        dpop_portray_clause(p_e_to_p_d1(assume(Ec@Lc:Nc-->Kc), L, N, K, Kd, D, Dd)-out).
p_e_to_p_d1(E, L, N, K, Kd, D, D) :-
        dpush_portray_clause(p_e_to_p_d1(E, L, N, K, Kd, D, D)-in),
        dpop_portray_clause(p_e_to_p_d1(E, L, N, K, Kd, D, D)-out).



% **********************************************************************
% Summarization of path expressions

/*
p_e_to_c1(+E, +L, +N, +Env, +K, +Kd, +D, -S)
*/
p_e_to_c1(app(Ef@Lf:Xf:Tf, ELNKs), L, N, Env, K, Kd, D, S) :- !,
        dpush_portray_clause(p_e_to_c1(app(Ef@Lf:Xf:Tf, ELNKs), L, N, Env, K, Kd, D, S)-in),
        (   ml_const(Ef) ->
            S = []
        ;   ml_id(Ef) ->
            (   avl_fetch(Ef, D, (K0, PreELT0)) -> % Ef is let bound identifier
                % instantiate type of Ef
                copy_term(PreELT0, E0@L0:T0),
                unname_type(Ef:Tf, T0),
                % analyze definition of Ef under appropriate context
                t_e_to_c1(E0, L0, T0, Ef, Env, K0, D, N0, Kd0, S0),
                % construct summary constraint for Ef
                mk_ctx_pred(N0, Ctx0),
                mk_summ_pred(N0, Summ),
                mk_conj((Kd0, Ctx0, K0), SummRel),
                ord_add_element(S0, (Summ :- SummRel), S1)
            ;   S1=[]                              % Ef is formal parameter
            ),
            % construct parameter passing summary for call to Ef
            mk_ctx_pred(Ef:Tf, CtxEf),
            (   foreach(Ei@Li:Xi:Ti-->Ki, ELNKs),
                fromto(true, InKs, OutKs, Ks),
                foreach(Si, Ss),
                param(Env, K, D)
            do  (   function_type(Ti) ->
                    OutKs = InKs,
                    mk_ctx_pred(Xi:Ti, Ctxi),
                    mk_conj((Ctxi, K), CtxiK),
                    p_e_to_c1(Ei, Li, Xi:Ti, Env, CtxiK, Ki, D, Sprei),
                    mk_summ_pred(Xi:Ti, Summi),
                    mk_conj((Ki, Ctxi, K), SummReli),
                    ord_add_element(Sprei, (Summi :- SummReli), Si)
                ;   OutKs = (Ki, InKs),
                    p_e_to_c1(Ei, Li, Xi:Ti, Env, K, Ki, D, Si)
                )
            ),
            mk_conj((Ks, K), Actuals),
            ord_add_element(S1, (CtxEf :- Actuals), S2),
            ord_union([S2|Ss], S)
        ),
        dpop_portray_clause(p_e_to_c1(app(Ef@Lf:Xf:Tf, ELNKs), L, N, Env, K, Kd, D, S)-out).
p_e_to_c1(abs(XLTs, Eb@Lb:Nb-->Kb), L, X:T, Env, K, Kb, D, S) :- !,
        dpush_portray_clause(p_e_to_c1(abs(XLTs, Eb@Lb:Nb-->Kb), L, X:T, Env, K, Kb, D, S)-in),
        p_e_to_c1(Eb, Lb, Nb, Env, K, Kb, D, S),
        dpop_portray_clause(p_e_to_c1(abs(XLTs, Eb@Lb:Nb-->Kb), L, X:T, Env, K, Kb, D, S)-out).
p_e_to_c1(ite(E1@L1:X1:T1-->K1, E2@L2:N2-->K2, E3@L3:N3-->K3), L, X:T, Env, K, Kd, D, S) :- !,
        dpush_portray_clause(p_e_to_c1(ite(E1@L1:X1:T1-->K1, E2@L2:N2-->K2, E3@L3:N3-->K3), L, X:T, Env, K, Kd, D, S)-in),
        p_e_to_c1(E1, L1, X1:T1, Env, K, K1, D, S1),
        p_e_to_c1(E2, L2, N2, Env, (X1=1, K1, K), K2, D, S2),
        p_e_to_c1(E3, L3, N3, Env, (X1=0, K1, K), K3, D, S3),
        ord_union([S1, S2, S3], S),
        dpop_portray_clause(p_e_to_c1(ite(E1@L1:X1:T1-->K1, E2@L2:N2-->K2, E3@L3:N3-->K3), L, X:T, Env, K, Kd, D, S)-out).
p_e_to_c1(let(Y@Ly:Ny, ELNK1, E2@L2:N2-->K2), L, N2, Env, K, Kd, D, S) :- !,
        dpush_portray_clause(p_e_to_c1(let(Y@Ly:Ny, ELNK1, E2@L2:N2-->K2), L, N2, Env, K, Kd, D, S)-in),
        (   E1@L1:N1-->K1 = ELNK1 ->
            p_e_to_c1(E1, L1, N1, Env, K, K1, D, S1),
            p_e_to_c1(E2, L2, N2, Env, (K1, K), K2, D, S2),
            ord_union([S1, S2], S)
        ;   avl_store(Y, Env, Ny, Env1),
            p_e_to_c1(E2, L2, N2, Env1, K, K2, D, S)
        ),
        dpop_portray_clause(p_e_to_c1(let(Y@Ly:Ny, ELNK1, E2@L2:N2-->K2), L, N2, Env, K, Kd, D, S)-out).
p_e_to_c1(assert(Ec@Lc:Xc:Tc-->Kc), L, N, Env, K, Kd, D, S) :- !,
        dpush_portray_clause(p_e_to_c1(assert(Ec@Lc:Xc:Tc-->Kc), L, N, Env, K, Kd, D, S)-in),
        p_e_to_c1(Ec, Lc, Xc:Tc, Env, K, Kc, D, Sc),
        mk_conj((Kc, K), Body),
        uppercase_atom(Xc, Xcu),
        ord_union([[(Xcu=1 :- Body)], Sc], S),
        dpop_portray_clause(p_e_to_c1(assert(Ec@Lc:Xc:Tc-->Kc), L, N, Env, K, Kd, D, S)-out).
p_e_to_c1(assume(Ec@Lc:Nc-->Kc), L, N, Env, K, Kd, D, S) :- !,
        dpush_portray_clause(p_e_to_c1(assume(Ec@Lc:Nc-->Kc), L, N, Env, K, Kd, D, S)-in),
        p_e_to_c1(Ec, Lc, Nc, Env, K, Kc, D, S),
        dpop_portray_clause(p_e_to_c1(assume(Ec@Lc:Nc-->Kc), L, N, Env, K, Kd, D, S)-out).
p_e_to_c1(E, L, X:T, Env, K, Kd, D, S) :-
        dpush_portray_clause(p_e_to_c1(E, L, X:T, Env, K, Kd, D, S)-id-cst-in),
        (   ml_const(E) ->
            S = []
        ;   ml_id(E) ->
            (   function_type(T) ->
                mk_ctx_pred(X:T, CtxX),
                mk_ctx_pred(E:T, CtxE),
                S = [(CtxE :- CtxX)]
            ;   S = []
            )
        ),
        dpop_portray_clause(p_e_to_c1(E, L, X:T, Env, K, Kd, D, S)-id-cst-out).

/*
mk_summ_pred(+N, -Summ)
*/
mk_summ_pred(N, Summ) :-
        summ_sy(N, Sy),
        formals_return(N, NFormalsRet),
        include(nullary_named_type, NFormalsRet, NullNFormalsRet),
        maplist(name_of_type, NullNFormalsRet, NullFormalsRet),
        maplist(uppercase_atom, NullFormalsRet, NullUFormalsRet),
        Summ =.. [Sy|NullUFormalsRet].

/*
summ_sy(+N, -Sy)
*/
summ_sy(N, Sy) :-
        unname_type(N, T),
        N = X:_,
        format_atom('~w_~w', [X, T], Sy).

/*
mk_ctx_pred(+N, -Ctx)
*/
mk_ctx_pred(N, Ctx) :-
        ctx_sy(N, Sy),
        formals(N, NFormals),
        include(nullary_named_type, NFormals, NullNFormals),
        maplist(name_of_type, NullNFormals, NullFormals),
        maplist(uppercase_atom, NullFormals, NullUFormals),
        Ctx =.. [Sy|NullUFormals].

/*
ctx_sy(+N, -Sy)
*/
ctx_sy(N, Sy) :-
        unname_type(N, T),
        N = X:_,
        format_atom('ctx_~w_~w', [X, T], Sy).

/*
mk_conj(+Tuple, -Conj)
*/
mk_conj(T, C) :-
        flatten_tuple(T, Flat),
        simplify_formula(Flat, C).

/*
flatten_tuple(+K, -R)
*/
flatten_tuple(K, R) :-
        tuple2flatlist(K, L),
        list2tuple(L, R).

/*
simplify_formula(+K, -R)
*/
simplify_formula(K, R) :-
        (   compound(K), K = (A, B) ->
            simplify_formula(A, Ar),
            simplify_formula(B, Br),
            (   Ar == true ->
                R = Br
            ;   Br == true ->
                R = Ar
            ;   R = (Ar, Br)
            )
        ;   compound(K), K = (A ; B) ->
            simplify_formula(A, Ar),
            simplify_formula(B, Br),
            (   Ar == false ->
                R = Br
            ;   Br == false ->
                R = Ar
            ;   R = (Ar ; Br)
            )
        ;   R = K
        ).

/*
formals_return(+N, -FormalsRet)
*/
formals_return(X:T, [R|Rs]) :-
	(   compound(T), T = (R->T2) ->
            formals_return(T2, Rs)
	;   R = X:T,
	    Rs = []
	).

/*
unname_type(+N, -T)
*/
unname_type(_:T, R) :-
	(   compound(T), T = (N1->N2) ->
            unname_type(N1, R1),
            unname_type(N2, R2),
            R = (R1->R2)
	;   R = T
	).

/*
name_of_type(+N, -X)
*/
name_of_type(X:_, X).



% **********************************************************************
% Summarization procedure

typed_exp_to_constraints(E@L:T, S) :-
        t_e_to_c1(E, L, T, v, empty, true, empty, _N, _Kd, S).

/*
t_e_to_c1(+E, +L, +T, +X, +Env, +K, +D, -N, -Kd, -S)
*/
t_e_to_c1(E, L, T, X, Env, K, D, N, Kd, S) :-
        dpush_portray_clause(t_e_to_c1(E, L, T, X, Env, K, D, N, Kd, S)-in),
        t_e_to_n_e1(E, L, T, X, Env, En@L:N),
        lprint('\n'),
        lindent, lprint('* Named expression:\n'),
        lprint(En@L:N),
        lprint('\n'),
        n_e_to_p_e1(En, L, N, Ep@L:N-->Kd),
        lprint('\n'),
        lindent, lprint('* Path expression:\n'),
        lprint(Ep@L:N-->Kd),
        lprint('\n'),
        mk_ctx_pred(N, Ctx),
        p_e_to_p_d1(Ep, L, N, (Ctx, K), Kd, D, Dd),
        lprint('\n'),
        lindent, lprint('* Function definitions:\n'),
        lprint(Dd),
        lprint('\n'),
        lpush, push_pp,
        p_e_to_c1(Ep, L, N, Env, (Ctx, K), Kd, Dd, S),
        lpop, pop_pp,
        lprint('\n'),
        lindent, lprint('* Summary constraints:\n'),
        if_log((   foreach(C, S)
               do  print(C), nl
               )),
        dpop_portray_clause(t_e_to_c1(E, L, T, X, Env, K, D, N, Kd, S)-out).



% **********************************************************************
% Main

summarize(FileIn, FileOut) :-
% Read the expression
        open(FileIn, read, In),
        read(In, ELT),
        close(In),
        \+ (
             ELT == end_of_file,
             print('ERROR: the input file is empty\n'),
             halt(1)
           ),
% Check that the expression is well-formed
        (   bb_get(nowf, 1) ->
            true
        ;   (   wf_typed_exp(ELT) ->
                true
            ;   print('ERROR: the input expression is malformed\n'),
                halt(1)
            )
        ),
% Log the typed expression
        lprint('\n'),
        lprint('* Typed expression:\n'),
        lprint(ELT),
        lprint('\n'),
% Sumarize the typed expression
        typed_exp_to_constraints(ELT, S),
% Output the summary
        lprint('\n'),
        lprint('* Summary constraints:\n'),
        (   FileOut == no_file ->
            Out = user_output
        ;   open(FileOut, write, Out)
        ),
        (   foreach(C, S),
            param(Out)
        do  print(Out, C),
            print(Out, '\n')
        ),
        close(Out).



% **********************************************************************
% Init
:- start_pp.
