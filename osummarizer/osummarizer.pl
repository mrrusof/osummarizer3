:- use_module('terms.pl').
:- use_module('log.pl', [start_log/0,
                         lprint/1, lformat/2]).
:- use_module('debug.pl', [start_debug/0,
                           dpush_portray_clause/1, dpop_portray_clause/1,
                           dnl/0]).
:- use_module('ext/utils/misc.pl', [format_atom/3]).
:- use_module('ext/utils/list_utils.pl', [list2tuple/2,
                                          tuple2flatlist/2]).
:- use_module(library(avl), [avl_fetch/3,
                             avl_store/4]).
:- use_module(library(lists), [rev/2,
                               maplist/3]).
:- use_module(library(ordsets), [ord_union/2]).

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
wf_t_e(E, L, T) :- !,
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
        ;   ( base_type(T) ; type_var(T) )
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
% Pretty printing of typed expressions, named expressions,
% and constraints.

stop_pp :-
        retractall(portray(_)).
start_pp :-
        assert((
portray(Term) :-
        (   compound(Term) ->
            (   Term = (Head:-Body) ->
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
                ;   Term = _@_:_
                ) ->
                pp_e(Term, "")
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
        (   nullary_type(T) ->
            print(T)
        ;   write('('),
            print(T),
            write(')')
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

pp_e(ELN-->K, I) :- !,
        pp_e(ELN, I),
        write(' --> '),
        print(K).
pp_e(E@_:X:T, I) :- !,
        pp_e(E, I),
        write(':'),
        print(X:T).
pp_e(E@_:T, I) :- !,
        pp_e(E, I),
        write(':'),
        paren_t(T).
pp_e(app(Ef, Es), I) :- !,
        append(I, "  ", J),
        format('~s(\n', [I]),
        pp_e(Ef, J),
        (   foreach(Ei, Es),
            param(J)
        do  write('\n'),
            pp_e(Ei, J)
        ),
        format('\n~s)', [I]).
pp_e(abs(Xs, Eb), I) :- !,
        append(I, "  ", J),
        format('~s(fun\n', [I]),
        (   Xs = [Xh|Xr] ->
            pp_e(Xh, J),
            (   foreach(X, Xr),
                param(J)
            do  write('\n'),
                pp_e(X, J)
            )
        ;   Xs = [X] ->
            pp_e(X, J)
        ),
        format('\n~s->\n', [I]),
        pp_e(Eb, J),
        format('\n~s)', [I]).
pp_e(ite(E1, E2, E3), I) :- !,
        append(I, "  ", J),
        format('~s(if\n', [I]),
        pp_e(E1, J),
        format('\n~sthen\n', [I]),
        pp_e(E2, J),
        format('\n~selse\n', [I]),
        pp_e(E3, J),
        format('\n~s)', [I]).
pp_e(let(X, E1, E2), I) :- !,
        append(I, "  ", J),
        format('~s(let\n', [I]),
        pp_e(X, J),
        format('\n~s=\n', [I]),
        pp_e(E1, J),
        format('\n~sin\n', [I]),
        pp_e(E2, J),
        format('\n~s)', [I]).
pp_e(assert(Ec), I) :- !,
        append(I, "  ", J),
        (   compound(Ec),
            ( Ec = E@_:_:_-->_
            ; Ec = E@_:_:_
            ; Ec = E@_:_ ),
            ( E == true ; E == false ) ->
            format('~s(assert(\n', [I]),
            pp_e(Ec, J),
            format('\n~s))', [I])
        ;   format('~s(assert\n', [I]),
            pp_e(Ec, J),
            format('\n~s)', [I])
        ).
pp_e(assume(Ec), I) :- !,
        append(I, "  ", J),
        (   compound(Ec),
            ( Ec = E@_:_:_-->_
            ; Ec = E@_:_:_
            ; Ec = E@_:_ ),
            ( E == true ; E == false ) ->
            format('~s(assume(\n', [I]),
            pp_e(Ec, J),
            format('\n~s))', [I])
        ;   format('~s(assume\n', [I]),
            pp_e(Ec, J),
            format('\n~s)', [I])
        ).
pp_e(E, I) :- !,
        format('~s', [I]),
        (   pp_const_parenthesis(E) ->
            write('('),
            write(E),
            write(')')
        ;   string(E) ->
            format("\"~s\"", [E])
        ;   ( ml_const(E) ; ml_id(E) ) ->
            write(E)
        ).



% **********************************************************************
% Naming of typed expressions

/*
t_e_to_n_e(+ELT, -ELN)
*/
typed_exp_to_named_exp(E@L:T, ELN) :-
        t_e_to_n_e1(E, L, T, v, empty, ELN).

/*
t_e_to_n_e1(+E, +L, +T, +X, +Env, -ELN)
*/
t_e_to_n_e1(app(Ef@Lf:Tf, ELTs), L, T, X, Env, app(Efr@Lfr:Nfrr, Rs)@L:N) :- !,
        dpush_portray_clause(t_e_to_n_e1(app(Ef@Lf:Tf, ELTs), L, T, X, Env, app(Efr@Lfr:Nfrr, Rs)@L:N)-app-in),
        (   ml_const(Ef) ->
            ml_const_to_name(Ef, C),
            format_atom('~p_~p', [C, X], Xf)
        ;   ml_id(Ef) ->
            format_atom('~p_~p', [Ef, X], Xf)
        ;   format_atom('f_~p', [X], Xf)
        ),
        t_e_to_n_e1(Ef, Lf, Tf, Xf, Env, Efr@Lfr:Nfr),
        formals(Nfr, Formals),
        (   foreach(Ei@Li:Ti, ELTs),
            fromto(Formals, [Xi:_|Rest], Rest, _),
            foreach(Ri, Rs),
            param(Env)
        do  t_e_to_n_e1(Ei, Li, Ti, Xi, Env, Ri)
        ),
        length(ELTs, Count),
        rename_return(Count, X, Nfr, Nfrr),
        remove_formals(Count, Nfrr, N),
        dpop_portray_clause(t_e_to_n_e1(app(Ef@Lf:Tf, ELTs), L, T, X, Env, app(Efr@Lfr:Nfrr, Rs)@L:N)-app-out).
t_e_to_n_e1(abs(XLTs, Eb@Lb:Tb), L, T, X, Env, abs(XLNs, Ebr@Lbr:Nbr)@L:X:Npre) :- !,
        dpush_portray_clause(t_e_to_n_e1(abs(XLTs, Eb@Lb:Tb), L, T, X, Env, abs(XLNs, Ebr@Lbr:Nbr)@L:X:Npre)-abs-in),
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
        dpop_portray_clause(t_e_to_n_e1(abs(XLTs, Eb@Lb:Tb), L, T, X, Env, abs(XLNs, Ebr@Lbr:Nbr)@L:X:Npre)-abs-out).
t_e_to_n_e1(ite(E1@L1:T1, E2@L2:T2, E3@L3:T3), L, T, X, Env, ite(R1, R2, R3)@L:N) :- !,
        dpush_portray_clause(t_e_to_n_e1(ite(E1@L1:T1, E2@L2:T2, E3@L3:T3), L, T, X, Env, ite(R1, R2, R3)@L:N)-ite-in),
        format_atom('c_~p', [X], X1),
        t_e_to_n_e1(E1, L1, T1, X1, Env, R1),
        t_e_to_n_e1(E2, L2, T2, X,  Env, R2),
        t_e_to_n_e1(E3, L3, T3, X,  Env, R3),
        name_type(X, T, N),
        dpop_portray_clause(t_e_to_n_e1(ite(E1@L1:T1, E2@L2:T2, E3@L3:T3), L, T, X, Env, ite(R1, R2, R3)@L:N)-ite-out).
t_e_to_n_e1(let(Y@Ly:Ty, E1@L1:T1, E2@L2:T2), L, T, X, Env, let(Y@Ly:N1, E1rL1r:N1, E2rL2r:N2)@L:N2) :- !,
        dpush_portray_clause(t_e_to_n_e1(let(Y@Ly:Ty, E1@L1:T1, E2@L2:T2), L, T, X, Env, let(Y@Ly:N1, E1rL1r:N1, E2rL2r:N2)@L:N2)-let-in),
        t_e_to_n_e1(E1, L1, T1, Y, Env, E1rL1r:N1),
        avl_store(Y, Env, N1, InEnv),
        t_e_to_n_e1(E2, L2, T2, X, InEnv, E2rL2r:N2),
        dpop_portray_clause(t_e_to_n_e1(let(Y@Ly:Ty, E1@L1:T1, E2@L2:T2), L, T, X, Env, let(Y@Ly:N1, E1rL1r:N1, E2rL2r:N2)@L:N2)-let-out).
t_e_to_n_e1(assert(Ec@Lc:Tc), L, T, X, Env, assert(Rc)@L:N) :- !,
        dpush_portray_clause(t_e_to_n_e1(assert(Ec@Lc:Tc), L, T, X, Env, assert(Rc)@L:N)-assert-in),
        format_atom('ase_~p', [X], Xc),
        t_e_to_n_e1(Ec, Lc, Tc, Xc, Env, Rc),
        name_type(X, T, N),
        dpop_portray_clause(t_e_to_n_e1(assert(Ec@Lc:Tc), L, T, X, Env, assert(Rc)@L:N)-assert-out).
t_e_to_n_e1(assume(Ec@Lc:Tc), L, T, X, Env, assume(Rc)@L:N) :- !,
        dpush_portray_clause(t_e_to_n_e1(assume(Ec@Lc:Tc), L, T, X, Env, assume(Rc)@L:N)-assume-in),
        format_atom('asu_~p', [X], Xc),
        t_e_to_n_e1(Ec, Lc, Tc, Xc, Env, Rc),
        name_type(X, T, N),
        dpop_portray_clause(t_e_to_n_e1(assume(Ec@Lc:Tc), L, T, X, Env, assume(Rc)@L:N)-assume-out).
t_e_to_n_e1(E, L, T, X, Env, E@L:N) :- !,
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
        ;   R = Y:T
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
rename_return(+Pos, +X, +N, -NewN)
*/
rename_return(Pos, X, Y:T, NewN) :-
        (   Pos > 0 ->
            (   compound(T),
                T = (N1->N2) ->
                Posp is Pos - 1,
                rename_return(Posp, X, N2, NewN2),
                NewN = Y:(N1->NewN2)
            )
        ;   NewN = X:T
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
named_exp_to_path_exp(+ELN, -ELNK)
*/
named_exp_to_path_exp(E@L:N, ELN-->K) :-
        n_e_to_p_e1(E, L, N, ELN-->K),
        lformat('\n* Main path conjunct:\n~p\n', [K]).

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
                foreach(Ai, Actuals),
                fromto([], InKs, OutKs, Ks)
            do  n_e_to_p_e1(Ei, Li, Xi:Ti, ELNKi),
                (   ml_const(Ei) ->
                    (_-->(_=Ai)) = ELNKi,
                    OutKs = InKs
                ;   ml_id(Ei) ->
                    uppercase_atom(Ei, Ai),
                    OutKs = InKs
                ;   uppercase_atom(Xi, Ai),
                    (_-->Ki) = ELNKi,
                    OutKs = [Ki|InKs]
                )
            ),
            formals(X:T, NFormals),
            maplist(name_of_type, NFormals, Formals),
            maplist(uppercase_atom, Formals, UFormals),
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
n_e_to_p_e1(let(YLyN1, E1@L1:X1:T1, E2@L2:N2), L, N2, let(YLyN1, E1L1N1K1, E2L2N2-->K2)@L:N2-->Kd) :- !,
        dpush_portray_clause(n_e_to_p_e1(let(YLyN1, E1@L1:X1:T1, E2@L2:N2), L, N2, let(YLyN1, E1L1N1K1, E2L2N2-->K2)@L:N2-->Kd)-in),
        n_e_to_p_e1(E2, L2, N2, E2L2N2-->K2),
        (   function_type(T1) ->
            E1L1N1K1 = E1@L1:X1:T1,
            Kd = K2
        ;   n_e_to_p_e1(E1, L1, X1:T1, E1L1N1K1),
            (_-->K1) = (E1L1N1K1),
            mk_conj((K2, K1), Kd)
        ),
        dpop_portray_clause(n_e_to_p_e1(let(YLyN1, E1@L1:X1:T1, E2@L2:N2), L, N2, let(YLyN1, E1L1N1K1, E2L2N2-->K2)@L:N2-->Kd)-out).
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
n_e_to_p_e1(E, L, X:T, E@L:X:T-->Kd) :- !,
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
% Function definitions of expressions

path_exp_to_fun_defs(E@L:N-->K, D) :-
        p_e_to_f_d1(E, L, N, true, K, empty, D).

p_e_to_f_d1(app(ELNKf, ELNKs), L, N, K, Kd, D, Dd) :- !,
        dpush_portray_clause(p_e_to_f_d1(app(ELNKf, ELNKs), L, N, K, Kd, D, Dd)-in),
        (   (Ef@Lf:Nf-->Kf) = ELNKf ->
            p_e_to_f_d1(Ef, Lf, Nf, K, Kf, D, Ddf)
        ;   Ef@Lf:Nf = ELNKf,
            p_e_to_f_d1(Ef, Lf, Nf, K, true, D, Ddf)
        ),
        (   foreach(Ei@Li:Ni-->Kdi, ELNKs),
            fromto(Ddf, InD, OutD, Dd),
            param(K)
        do  p_e_to_f_d1(Ei, Li, Ni, K, Kdi, InD, OutD)
        ),
        dpop_portray_clause(p_e_to_f_d1(app(ELNKf, ELNKs), L, N, K, Kd, D, Dd)-out).
p_e_to_f_d1(abs(XLTs, Eb@Lb:Nb-->Kb), L, N, K, Kd, D, Dd) :- !,
        dpush_portray_clause(p_e_to_f_d1(abs(XLTs, Eb@Lb:Nb-->Kb), L, N, K, Kd, D, Dd)-in),
        p_e_to_f_d1(Eb, Lb, Nb, K, Kb, D, Dd),
        dpop_portray_clause(p_e_to_f_d1(abs(XLTs, Eb@Lb:Nb-->Kb), L, N, K, Kd, D, Dd)-out).
p_e_to_f_d1(ite(E1@L1:N1-->K1, E2@L2:N2-->K2, E3@L3:N3-->K3), L, N, K, Kd, D, Dd) :- !,
        dpush_portray_clause(p_e_to_f_d1(ite(E1@L1:N1-->K1, E2@L2:N2-->K2, E3@L3:N3-->K3), L, N, K, Kd, D, Dd)-in),
        p_e_to_f_d1(E1, L1, N1, K, K1, D, D1),
        p_e_to_f_d1(E1, L1, N1, (X1=1, K1, K), K1, D1, D2),
        p_e_to_f_d1(E1, L1, N1, (X1=0, K1, K), K1, D2, Dd),
        dpop_portray_clause(p_e_to_f_d1(ite(E1@L1:N1-->K1, E2@L2:N2-->K2, E3@L3:N3-->K3), L, N, K, Kd, D, Dd)-out).
p_e_to_f_d1(let(Y@Ly:N1, E1L1N1K1, E2@L2:N2-->K2), L, N, K, Kd, D, Dd) :- !,
        dpush_portray_clause(p_e_to_f_d1(let(Y@Ly:N1, E1@L1:X1:T1-->K1, E2@L2:N2-->K2), L, N, K, Kd, D, Dd)-in),
        (   (_@_:N1) = E1L1N1K1 ->
            mk_ctx_pred(N1, Ctx),
            avl_store(Y, D, (Ctx, E1L1N1K1), D1),
            p_e_to_f_d1(E2, L2, N2, K, K2, D1, Dd)
        ;   (E1@L1:X1:T1-->K1) = E1L1N1K1,
            p_e_to_f_d1(E1, L1, X1, K, K1, D, D1),
            p_e_to_f_d1(E1, L1, X1, (K1, K), K1, D1, Dd)
        ),
        dpop_portray_clause(p_e_to_f_d1(let(Y@Ly:N1, E1@L1:X1:T1-->K1, E2@L2:N2-->K2), L, N, K, Kd, D, Dd)-out).
p_e_to_f_d1(assert(Ec@Lc:Nc-->Kc), L, N, K, Kd, D, Dd) :- !,
        dpush_portray_clause(p_e_to_f_d1(assert(Ec@Lc:Nc-->Kc), L, N, K, Kd, D, Dd)-in),
        p_e_to_f_d1(Ec, Lc, Nc, K, Kc, D, Dd),
        dpop_portray_clause(p_e_to_f_d1(assert(Ec@Lc:Nc-->Kc), L, N, K, Kd, D, Dd)-out).
p_e_to_f_d1(assume(Ec@Lc:Nc-->Kc), L, N, K, Kd, D, Dd) :- !,
        dpush_portray_clause(p_e_to_f_d1(assume(Ec@Lc:Nc-->Kc), L, N, K, Kd, D, Dd)-in),
        p_e_to_f_d1(Ec, Lc, Nc, K, Kc, D, Dd),
        dpop_portray_clause(p_e_to_f_d1(assume(Ec@Lc:Nc-->Kc), L, N, K, Kd, D, Dd)-out).
p_e_to_f_d1(E, L, N, K, Kd, D, D) :- !,
        dpush_portray_clause(p_e_to_f_d1(E, L, N, K, Kd, D, D)-in),
        dpop_portray_clause(p_e_to_f_d1(E, L, N, K, Kd, D, D)-out).



% **********************************************************************
% Summarization of path expressions

/*
path_exp_to_constraints(+ELNK, -S)
*/
path_exp_to_constraints(E@L:N-->K, A, S) :-
        p_e_to_c1(E, L, N, true, K, A, S).

/*
p_e_to_c1(+E, +L, +N, +K, +Kd, +A, -S)
*/
p_e_to_c1(app(Ef@Lf:Xf:Tf, ELNKs), L, X:T, K, Kd, A, S) :- !,
        dpush_portray_clause(p_e_to_c1(app(Ef@Lf:Xf:Tf, ELNKs), L, X:T, K, Kd, A, S)-in),
        (   ml_const(Ef) ->
            S = []
        ;   ml_id(Ef) ->
            mk_ctx_pred(Ef:Tf, CtxEf),
            (   foreach(Ei@Li:Ni-->Ki, ELNKs),
                fromto(true, InKs, (Ki,InKs), Ks),
                foreach(Si, Ss),
                param(K, A)
            do  p_e_to_c1(Ei, Li, Ni, K, Ki, A, Si)
            ),
            mk_conj((Ks, K), Body),
            ord_union([[(CtxEf :- Body)]|Ss], S)
        ),
        dpop_portray_clause(p_e_to_c1(app(Ef@Lf:Xf:Tf, ELNKs), L, X:T, K, Kd, A, S)-out).
p_e_to_c1(abs(XLTs, Eb@Lb:Nb-->Kb), L, X:T, K, Kb, A, S) :- !,
        dpush_portray_clause(p_e_to_c1(abs(XLTs, Eb@Lb:Nb-->Kb), L, X:T, K, Kb, A, S)-in),
        p_e_to_c1(Eb, Lb, Nb, K, Kb, A, S),
        dpop_portray_clause(p_e_to_c1(abs(XLTs, Eb@Lb:Nb-->Kb), L, X:T, K, Kb, A, S)-out).
p_e_to_c1(ite(E1@L1:X1:T1-->K1, E2@L2:N2-->K2, E3@L3:N3-->K3), L, X:T, K, Kd, A, S) :- !,
        dpush_portray_clause(p_e_to_c1(ite(E1@L1:X1:T1-->K1, E2@L2:N2-->K2, E3@L3:N3-->K3), L, X:T, K, Kd, A, S)-in),
        p_e_to_c1(E1, L1, X1:T1, K, K1, A, S1),
        p_e_to_c1(E2, L2, N2, (X1=1, K1, K), K2, A, S2),
        p_e_to_c1(E3, L3, N3, (X1=0, K1, K), K3, A, S3),
        ord_union([S1, S2, S3], S),
        dpop_portray_clause(p_e_to_c1(ite(E1@L1:X1:T1-->K1, E2@L2:N2-->K2, E3@L3:N3-->K3), L, X:T, K, Kd, A, S)-out).
p_e_to_c1(let(X1Lx1Nx1, E1@L1:X1:T1-->K1, E2@L2:N2-->K2), L, N2, K, Kd, A, S) :- !,
        dpush_portray_clause(p_e_to_c1(let(X1Lx1Nx1, E1@L1:X1:T1-->K1, E2@L2:N2-->K2), L, N2, K, Kd, A, S)-in),
        (   function_type(T1) ->
            mk_ctx_pred(X1:T1, Ctx),
            p_e_to_c1(E1, L1, X1:T1, (Ctx, K), K1, A, S1),
            p_e_to_c1(E2, L2, N2, K, K2, A, S2),
            mk_summ_pred(X1:T1, Summ),
            mk_conj((K1, Ctx, K), Body),
            ord_union([[(Summ :- Body)], S1, S2], S)
        ;   p_e_to_c1(E1, L1, X1:T1, K, K1, A, S1),
            p_e_to_c1(E2, L2, N2, (K1, K), K2, A, S2),
            ord_union([S1, S2], S)
        ),
        dpop_portray_clause(p_e_to_c1(let(X1Lx1Nx1, E1@L1:X1:T1-->K1, E2@L2:N2-->K2), L, N2, K, Kd, A, S)-out).
p_e_to_c1(assert(Ec@Lc:Xc:Tc-->Kc), L, X:T, K, Kd, A, S) :- !,
        dpush_portray_clause(p_e_to_c1(assert(Ec@Lc:Xc:Tc-->Kc), L, X:T, K, Kd, A, S)-in),
        p_e_to_c1(Ec, Lc, Xc:Tc, K, Kc, A, Sc),
        mk_conj((Kc, K), Body),
        uppercase_atom(Xc, Xcu),
        ord_union([[(Xcu=1 :- Body)], Sc], S),
        dpop_portray_clause(p_e_to_c1(assert(Ec@Lc:Xc:Tc-->Kc), L, X:T, K, Kd, A, S)-out).
p_e_to_c1(assume(Ec@Lc:Nc-->Kc), L, X:T, K, Kd, A, S) :- !,
        dpush_portray_clause(p_e_to_c1(assume(Ec@Lc:Nc-->Kc), L, X:T, K, Kd, A, S)-in),
        p_e_to_c1(Ec, Lc, Nc, K, Kc, A, S),
        dpop_portray_clause(p_e_to_c1(assume(Ec@Lc:Nc-->Kc), L, X:T, K, Kd, A, S)-out).
p_e_to_c1(E, L, X:T, K, Kd, A, S) :- !,
        dpush_portray_clause(p_e_to_c1(E, L, X:T, K, Kd, A, S)-id-cst-in),
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
        dpop_portray_clause(p_e_to_c1(E, L, X:T, K, Kd, A, S)-id-cst-out).

/*
mk_summ_pred(+N, -Summ)
*/
mk_summ_pred(N, Summ) :-
        summ_sy(N, Sy),
        formals_return(N, NFormalsRet),
        maplist(name_of_type, NFormalsRet, FormalsRet),
        maplist(uppercase_atom, FormalsRet, UFormalsRet),
        Summ =.. [Sy|UFormalsRet].

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
        maplist(name_of_type, NFormals, Formals),
        maplist(uppercase_atom, Formals, UFormals),
        Ctx =.. [Sy|UFormals].

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
% Name the expression
        typed_exp_to_named_exp(ELT, ELN),
% Log the named expression
        lprint('\n'),
        lprint('* Named expression:\n'),
        lprint(ELN),
        lprint('\n'),
% Path conjuncts for expression
        named_exp_to_path_exp(ELN, ELNK),
% Log the path expression
        lprint('\n'),
        lprint('* Path expression:\n'),
        lprint(ELNK),
        lprint('\n'),
% Summarize the expression
        path_exp_to_constraints(ELNK, Ss),
% Output the summarized program
        lprint('\n'),
        lprint('* Summary constraints:\n'),
        (   FileOut == no_file ->
            Out = user_output
        ;   open(FileOut, write, Out)
        ),
        (   foreach(S, Ss),
            param(Out)
        do  print(Out, S),
            print(Out, '\n')
        ),
        close(Out).


% **********************************************************************
% Init
:- start_pp.
