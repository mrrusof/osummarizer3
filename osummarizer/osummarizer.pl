:- use_module('log.pl', [start_log/0,
                         lprint/1]).
:- use_module('debug.pl', [start_debug/0,
                           dpush_write/1, dpop_write/1,
                           dformat/2,
                           dnl/0]).
:- use_module('ext/utils/misc.pl', [format_atom/3]).
:- use_module(library(avl), [avl_fetch/3,
                             avl_store/4]).
:- use_module(library(lists), [rev/2,
                               maplist/3]).
:- use_module(library(ordsets), [list_to_ord_set/2,
                                 ord_union/3,
                                 ord_union/2,
                                 ord_add_element/3]).

:- multifile user:portray/1.

% Define operator @
% A@B:C == (A@B):C because current_op(N, _, :) ---> N = 550
:- op(540, xfy, @).


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

ml_const(C) :- ground(C), ( number(C) ; string(C) ; ml_const_pervasives(C) ; ml_const_path(C) ).
ml_const_path('List.nil').
ml_const_path('List.cons').
ml_const_path('List.map').
ml_const_path('List.length').
ml_const_path('Obj.magic').
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

function_type(T) :- compound(T), T = (_->_).
nullary_type(T) :- atom(T).
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
        dpush_write(wf_t_e(app(Ef@Lf:Tf, ELTs), L, T)-in), dnl,
        wf_l(L),
        wf_t(T),
        wf_t_e(Ef, Lf, Tf),
        ELTs = [_|_],
        (   foreach(Ei@Li:Ti, ELTs)
        do  wf_t_e(Ei, Li, Ti)
        ),
        dpop_write(wf_t_e(app(Ef@Lf:Tf, ELTs), L, T)-out), dnl.
wf_t_e(abs(XLTs, Eb@Lb:Tb), L, T) :- !,
        dpush_write(wf_t_e(abs(XLTs, Eb@Lb:Tb), L, T)-in), dnl,
        wf_l(L),
        wf_t(T),
        XLTs = [_|_],
        (   foreach(Xi@Li:Ti, XLTs)
        do  ml_id(Xi),
            wf_l(Li),
            wf_t(Ti)
        ),
        wf_t_e(Eb, Lb, Tb),
        dpop_write(wf_t_e(abs(XLTs, Eb@Lb:Tb), L, T)-out), dnl.
wf_t_e(ite(E1@L1:T1, E2@L2:T2, E3@L3:T3), L, T) :- !,
        dpush_write(wf_t_e(ite(E1@L1:T1, E2@L2:T2, E3@L3:T3), L, T)-in), dnl,
        wf_l(L),
        wf_t(T),
        T2 == T3,
        T == T3,
        wf_t_e(E1, L1, T1),
        wf_t_e(E2, L2, T2),
        wf_t_e(E3, L3, T3),
        dpop_write(wf_t_e(ite(E1@L1:T1, E2@L2:T2, E3@L3:T3), L, T)-out), dnl.
wf_t_e(let(X@Lx:Tx, E1@L1:T1, E2@L2:T2), L, T) :- !,
        dpush_write(wf_t_e(let(X@Lx:Tx, E1@L1:T1, E2@L2:T2), L, T)-in), dnl,
        wf_l(L),
        wf_t(T),
        ml_id(X),
        wf_l(Lx),
        wf_t(Tx),
        wf_t_e(E1, L1, T1),
        wf_t_e(E2, L2, T2),
        dpop_write(wf_t_e(let(X@Lx:Tx, E1@L1:T1, E2@L2:T2), L, T)-out), dnl.
wf_t_e(assert(Ec@Lc:Tc), L, T) :- !,
        dpush_write(wf_t_e(assert(Ec@Lc:Tc), L, T)-assert-in), dnl,
        wf_t_e(Ec, Lc, Tc),
        wf_l(L),
        T == unit,
        dpop_write(wf_t_e(assert(Ec@Lc:Tc), L, T)-assert-out), dnl.
wf_t_e(assume(Ec@Lc:Tc), L, T) :- !,
        dpush_write(wf_t_e(assume(Ec@Lc:Tc), L, T)-assume-in), dnl,
        wf_t_e(Ec, Lc, Tc),
        wf_l(L),
        T == unit,
        dpop_write(wf_t_e(assume(Ec@Lc:Tc), L, T)-assume-out), dnl.
wf_t_e(E, L, T) :- !,
        dpush_write(wf_t_e(E, L, T)-in), dnl,
        wf_l(L),
        wf_t(T),
        ( ml_const(E) ; ml_id(E) ),
        dpop_write(wf_t_e(E, L, T)-out), dnl.

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
            ;   T =.. [_|Ts],
                (   foreach(Ti, Ts)
                do  wf_t(Ti)
                )
            )
        ;   ( nullary_type(T) ; type_var(T) )
        ).


% **********************************************************************
% Pretty printing of typed expressions, named expressions,
% and constraints.

user:portray(Term) :-
        (   compound(Term) ->
            (   Term = (Head:-Body) ->
                print(Head),
                write(' :- '),
                print(Body),
                write('.')
            ;   Term =.. ['\\+'|[A]] ->
                format('\\+~p', [A])
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
            ;   Term = E@_:X:T ->
                pp_n_e(E, X, T, "")
            ;   Term = E@_:T ->
                pp_t_e(E, T, "")
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
        ).

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

/*
pp_t_e(+E, +T, +I)
*/
pp_t_e(app(Ef@_:Tf, ELTs), T, I) :- !,
        append(I, "  ", J),
        format('~s(\n', [I]),
        pp_t_e(Ef, Tf, J),
        (   foreach(Ei@_:Ti, ELTs),
            param(J)
        do  write('\n'),
            pp_t_e(Ei, Ti, J)
        ),
        format('\n~s):', [I]),
        paren_t(T).
pp_t_e(abs(XLTs, Eb@_:Tb), T, I) :- !,
        append(I, "  ", J),
        format('~s(fun\n', [I]),
        (   XLTs = [X@_:T] ->
            pp_t_e(X, T, J)
        ;   XLTs = [Xh@_:Th|XrLrTr] ->
            pp_t_e(Xh, Th, J),
            (   foreach(X@_:Tx, XrLrTr),
                param(J)
            do  write('\n'),
                pp_t_e(X, Tx, J)
            )
        ),
        format('\n~s->\n', [I]),
        pp_t_e(Eb, Tb, J),
        format('\n~s):', [I]),
        paren_t(T).
pp_t_e(ite(E1@_:T1, E2@_:T2, E3@_:T3), T, I) :- !,
        append(I, "  ", J),
        format('~s(if\n', [I]),
        pp_t_e(E1, T1, J),
        format('\n~sthen\n', [I]),
        pp_t_e(E2, T2, J),
        format('\n~selse\n', [I]),
        pp_t_e(E3, T3, J),
        format('\n~s):', [I]),
        paren_t(T).
pp_t_e(let(X@_:Tx, E1@_:T1, E2@_:T2), T, I) :- !,
        append(I, "  ", J),
        format('~s(let\n', [I]),
        pp_t_e(X, Tx, J),
        format('\n~s=\n', [I]),
        pp_t_e(E1, T1, J),
        format('\n~sin\n', [I]),
        pp_t_e(E2, T2, J),
        format('\n~s):', [I]),
        paren_t(T).
pp_t_e(assert(Ec@_:Tc), T, I) :- !,
        append(I, "  ", J),
        (   ( Ec == true ; Ec == false ) ->
            format('~s(assert(\n', [I])
        ;   format('~s(assert\n', [I])
        ),
        pp_t_e(Ec, Tc, J),
        (   ( Ec == true ; Ec == false ) ->
            format('\n~s)):', [I])
        ;   format('\n~s):', [I])
        ),
        paren_t(T).
pp_t_e(assume(Ec@_:Tc), T, I) :- !,
        append(I, "  ", J),
        (   ( Ec == true ; Ec == false ) ->
            format('~s(assume(\n', [I])
        ;   format('~s(assume\n', [I])
        ),
        pp_t_e(Ec, Tc, J),
        (   ( Ec == true ; Ec == false ) ->
            format('\n~s)):', [I])
        ;   format('\n~s):', [I])
        ),
        paren_t(T).
pp_t_e(E, T, I) :- !,
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
        write(':'),
        paren_t(T).

/*
pp_n_e(+E, +X, +T, +I)
*/
pp_n_e(app(Ef@_:Xf:Tf, ELNs), X, T, I) :- !,
        append(I, "  ", J),
        format('~s(\n', [I]),
        pp_n_e(Ef, Xf, Tf, J),
        (   foreach(Ei@_:Xi:Ti, ELNs),
            param(J)
        do  write('\n'),
            pp_n_e(Ei, Xi, Ti, J)
        ),
        format('\n~s):', [I]),
        print(X:T).
pp_n_e(abs(YLNs, Eb@_:Xb:Tb), X, T, I) :-
        append(I, "  ", J),
        format('~s(fun\n', [I]),
        (   YLNs = [Y@_:Xy:Ty] ->
            pp_n_e(Y, Xy, Ty, J)
        ;   YLNs = [Yh@_:Xh:Th|YrLrNr] ->
            pp_n_e(Yh, Xh, Th, J),
            (   foreach(Y@_:Xy:Ty, YrLrNr),
                param(J)
            do  write('\n'),
                pp_n_e(Y, Xy, Ty, J)
            )
        ),
        format('\n~s->\n', [I]),
        pp_n_e(Eb, Xb, Tb, J),
        format('\n~s):', [I]),
        print(X:T).
pp_n_e(ite(E1@_:X1:T1, E2@_:X2:T2, E3@_:X3:T3), X, T, I) :- !,
        append(I, "  ", J),
        format('~s(if\n', [I]),
        pp_n_e(E1, X1, T1, J),
        format('\n~sthen\n', [I]),
        pp_n_e(E2, X2, T2, J),
        format('\n~selse\n', [I]),
        pp_n_e(E3, X3, T3, J),
        format('\n~s):', [I]),
        print(X:T).
pp_n_e(let(Y@_:Xy:Tx, E1@_:X1:T1, E2@_:X2:T2), X, T, I) :- !,
        append(I, "  ", J),
        format('~s(let\n', [I]),
        pp_n_e(Y, Xy, Tx, J),
        format('\n~s=\n', [I]),
        pp_n_e(E1, X1, T1, J),
        format('\n~sin\n', [I]),
        pp_n_e(E2, X2, T2, J),
        format('\n~s):', [I]),
        print(X:T).
pp_n_e(assert(Ec@_:Xc:Tc), X, T, I) :- !,
        append(I, "  ", J),
        (   ( Ec == true ; Ec == false ) ->
            format('~s(assert(\n', [I])
        ;   format('~s(assert\n', [I])
        ),
        pp_n_e(Ec, Xc, Tc, J),
        (   ( Ec == true ; Ec == false ) ->
            format('\n~s)):', [I])
        ;   format('\n~s):', [I])
        ),
        print(X:T).
pp_n_e(assume(Ec@_:Xc:Tc), X, T, I) :- !,
        append(I, "  ", J),
        (   ( Ec == true ; Ec == false ) ->
            format('~s(assume(\n', [I])
        ;   format('~s(assume\n', [I])
        ),
        pp_n_e(Ec, Xc, Tc, J),
        (   ( Ec == true ; Ec == false ) ->
            format('\n~s)):', [I])
        ;   format('\n~s):', [I])
        ),
        print(X:T).
pp_n_e(E, X, T, I) :- !,
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
        write(':'),
        print(X:T).


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
        dpush_write(t_e_to_n_e1(app(Ef@Lf:Tf, ELTs), L, T, X, Env, app(Efr@Lfr:Nfrr, Rs)@L:N)-app-in), dnl,
        t_e_to_n_e1(Ef, Lf, Tf, X, Env, Efr@Lfr:Nfr),
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
        dpop_write(t_e_to_n_e1(app(Ef@Lf:Tf, ELTs), L, T, X, Env, app(Efr@Lfr:Nfrr, Rs)@L:N)-app-out), dnl.
t_e_to_n_e1(abs(XLTs, Eb@Lb:Tb), L, T, X, Env, abs(XLNs, Ebr@Lbr:Nbr)@L:X:Npre) :- !,
        dpush_write(t_e_to_n_e1(abs(XLTs, Eb@Lb:Tb), L, T, X, Env, abs(XLNs, Ebr@Lbr:Nbr)@L:X:Npre)-abs-in), dnl,
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
        dpop_write(t_e_to_n_e1(abs(XLTs, Eb@Lb:Tb), L, T, X, Env, abs(XLNs, Ebr@Lbr:Nbr)@L:X:Npre)-abs-out), dnl.
t_e_to_n_e1(ite(E1@L1:T1, E2@L2:T2, E3@L3:T3), L, T, X, Env, ite(R1, R2, R3)@L:N) :- !,
        dpush_write(t_e_to_n_e1(ite(E1@L1:T1, E2@L2:T2, E3@L3:T3), L, T, X, Env, ite(R1, R2, R3)@L:N)-ite-in),
        format_atom('c_~p', [X], X1),
        t_e_to_n_e1(E1, L1, T1, X1, Env, R1),
        t_e_to_n_e1(E2, L2, T2, X,  Env, R2),
        t_e_to_n_e1(E3, L3, T3, X,  Env, R3),
        name_type(X, T, N),
        dpop_write(t_e_to_n_e1(ite(E1@L1:T1, E2@L2:T2, E3@L3:T3), L, T, X, Env, ite(R1, R2, R3)@L:N)-ite-out).
t_e_to_n_e1(let(Y@Ly:Ty, E1@L1:T1, E2@L2:T2), L, T, X, Env, let(Y@Ly:N1, E1rL1r:N1, E2rL2r:N2)@L:N2) :- !,
        dpush_write(t_e_to_n_e1(let(Y@Ly:Ty, E1@L1:T1, E2@L2:T2), L, T, X, Env, let(Y@Ly:N1, E1rL1r:N1, E2rL2r:N2)@L:N2)-let-in), dnl,
        t_e_to_n_e1(E1, L1, T1, Y, Env, E1rL1r:N1),
        avl_store(Y, Env, N1, InEnv),
        t_e_to_n_e1(E2, L2, T1, X, InEnv, E2rL2r:N2),
        dpop_write(t_e_to_n_e1(let(Y@Ly:Ty, E1@L1:T1, E2@L2:T2), L, T, X, Env, let(Y@Ly:N1, E1rL1r:N1, E2rL2r:N2)@L:N2)-let-out), dnl.
t_e_to_n_e1(assert(Ec@Lc:Tc), L, T, X, Env, assert(Rc)@L:N) :- !,
        dpush_write(t_e_to_n_e1(assert(Ec@Lc:Tc), L, T, X, Env, assert(Rc)@L:N)-assert-in), dnl,
        format_atom('ase_~p', [X], Xc),
        t_e_to_n_e1(Ec, Lc, Tc, Xc, Env, Rc),
        name_type(X, T, N),
        dpop_write(t_e_to_n_e1(assert(Ec@Lc:Tc), L, T, X, Env, assert(Rc)@L:N)-assert-out), dnl.
t_e_to_n_e1(assume(Ec@Lc:Tc), L, T, X, Env, assume(Rc)@L:N) :- !,
        dpush_write(t_e_to_n_e1(assume(Ec@Lc:Tc), L, T, X, Env, assume(Rc)@L:N)-assume-in), dnl,
        format_atom('asu_~p', [X], Xc),
        t_e_to_n_e1(Ec, Lc, Tc, Xc, Env, Rc),
        name_type(X, T, N),
        dpop_write(t_e_to_n_e1(assume(Ec@Lc:Tc), L, T, X, Env, assume(Rc)@L:N)-assume-out), dnl.
t_e_to_n_e1(E, L, T, X, Env, E@L:N) :- !,
        dpush_write(t_e_to_n_e1(E, L, T, X, Env, E@L:N)-id-cst-in), dnl,
        (   ml_id(E) ->
            (   function_type(T) ->
                format_atom('~p_~p', [E, X], EX),
                name_type(EX, T, Nloc),
                avl_fetch(E, Env, Nenv),
                choose_names(Nenv, Nloc, N)
            ;   N = X:T
            )
        ;   ml_const(E) ->
            (   function_type(T) ->
                ml_const_to_name(E, C),
                format_atom('~p_~p', [C, X], CX),
                name_type(CX, T, N)
            ;   N = X:T
            )
        ),
        dpop_write(t_e_to_n_e1(E, L, T, X, Env, E@L:N)-id-cst-out), dnl.

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
remove_formals(Count, N, R) :-
*/
remove_formals(Count, N, R) :-
	(   Count > 0 ->
	    N = _:(_->N2),
	    Count1 is Count-1,
	    remove_formals(Count1, N2, R)
	;   R = N
	).

/*
ml_const_to_name(-Const, +Name)
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
ml_const_to_name(&&, and).
ml_const_to_name('||', or).
ml_const_to_name('Obj.magic', 'magic').


% **********************************************************************
% Summarization of named expressions

/*
named_exp_to_constraints(+E@L:N, -Ss)
*/
named_exp_to_constraints(E@L:N, S) :-
        n_e_to_c1(E, L, N, empty, true, DP, S),
        dformat('\nMain predicate: ~p\n\n', [DP]).

/*
n_e_to_c1(+E, +L, +N, +Env, +K, -DK, -S)
*/
n_e_to_c1(app(Ef@Lf:Nf, ELNs), L, N, Env, K, DK, S) :- !,
        dpush_write(n_e_to_c1(app(Ef@Lf:Nf, ELNs), L, N, Env, K, DK, S)-app-in), dnl,
        (   ml_id(Ef) ->                               % APP-IDENT
            n_e_to_c1(Ef, Lf, Nf, Env, K, DKf, Sf),
            (   foreach(Ei@Li:Ni, ELNs),
                fromto([], InS, OutS, Ss),
                fromto(empty, InEqs, OutEqs, Eqs),
                foreach(Xi=Vi, Ks),
                param(Env, K)
            do  n_e_to_c1(Ei, Li, Ni, Env, K, (Xi=Vi), Si),
                ord_union(InS, Si, OutS),
                avl_store(Xi, InEqs, Vi, OutEqs)
            ),
            mk_conj(Ks, Kctx),
            mk_ctx_cstr(Nf, Kctx, CtxCstr),
            ord_union(Sf, Ss, S1),
            ord_add_element(S1, CtxCstr, S),
            apply_equations(DKf, Eqs, DK)
        ;   ml_const(Ef) ->                            % APP-CONST-BOOL
            n_e_to_c1(Ef, Lf, Nf, Env, K, DKf, Sf),
            (   foreach(Ei@Li:Ni, ELNs),
                fromto([], InS, OutS, Ss),
                fromto(empty, InEqs, OutEqs, Eqs),
                param(Env, K)
            do  n_e_to_c1(Ei, Li, Ni, Env, K, (Xi=Vi), Si),
                ord_union(InS, Si, OutS),
                avl_store(Xi, InEqs, Vi, OutEqs)
            ),
            ord_union(Sf, Ss, S),
            apply_equations(DKf, Eqs, DK)
        ),
        dpop_write(n_e_to_c1(app(Ef@Lf:Nf, ELNs), L, N, Env, K, DK, S)-app-out), dnl.
n_e_to_c1(abs(XLNs, Eb@Lb:Nb), L, N, Env, K, true, S) :- !,
        dpush_write(n_e_to_c1(abs(XLNs, Eb@Lb:Nb), L, N, Env, K, true, S)-abs-in), dnl,
        n_e_to_c1(Eb, Lb, Nb, Env, K, DKb, Sb),
        mk_summ_cstr(N, DKb, SummCstr),
        ord_add_element(Sb, SummCstr, S),
        dpop_write(n_e_to_c1(abs(XLNs, Eb@Lb:Nb), L, N, Env, K, true, S)-abs-out), dnl.
n_e_to_c1(ite(E1@L1:N1, E2@L2:N2, E3@L3:N3), L, N, Env, K, DK, S) :- !,
        dpush_write(n_e_to_c1(ite(E1@L1:N1, E2@L2:N2, E3@L3:N3), L, N, Env, K, DK, S)-ite-in), dnl,
        n_e_to_c1(E1, L1, N1, Env, K, DK1, S1),
        n_e_to_c1(E2, L2, N2, Env, K, DK2, S2),
        n_e_to_c1(E3, L3, N3, Env, K, DK3, S3),
        ord_union([S1, S2, S3], S),
        DK = (DK1, DK2 ; \+DK1, DK3),
        dpop_write(n_e_to_c1(ite(E1@L1:N1, E2@L2:N2, E3@L3:N3), L, N, Env, K, DK, S)-ite-out), dnl.
n_e_to_c1(let(Y@Ly:Ny, E1@L1:N1, E2@L2:N2), L, N, Env, K, DK, S) :- !,
        dpush_write(n_e_to_c1(let(Y@Ly:Ny, E1@L1:N1, E2@L2:N2), L, N, Env, K, DK, S)-let-in), dnl,
        n_e_to_c1(E1, L1, N1, Env, K, DK1, S1),
        n_e_to_c1(E2, L2, N2, Env, K, DK2, S2),
        ord_union(S1, S2, S),
        mk_conj([DK1, DK2], DKpre),
        remove_true(DKpre, DK),
        dpop_write(n_e_to_c1(let(Y@Ly:Ny, E1@L1:N1, E2@L2:N2), L, N, Env, K, DK, S)-let-out), dnl.
n_e_to_c1(E, L, X:T, Env, K, DK, []) :- !,
        dpush_write(n_e_to_c1(E, L, X:T, Env, K, DK, [])-id-cst-in), dnl,
        (   ml_id(E) ->
            (   function_type(T) ->
                summ_sy(X:T, SummSy),
                return(X:T, _:R),
                (   R == bool ->
                    formals(X:T, NFormals),
                    maplist(type_name, NFormals, Formals),
                    maplist(uppercase_atom, Formals, UFormals),
                    DK =.. [SummSy|UFormals]
                ;   formals_return(X:T, NFormalsRet),
                    maplist(type_name, NFormalsRet, FormalsRet),
                    maplist(uppercase_atom, FormalsRet, UFormalsRet),
                    DK =.. [SummSy|UFormalsRet]
                )
            ;   uppercase_atom(X, Xu),
                uppercase_atom(E, Eu),
                DK = (Xu=Eu)
            )
        ;   ml_const(E) ->
            (   function_type(T) ->
                formals(X:T, NFormals),
                maplist(type_name, NFormals, Formals),
                maplist(uppercase_atom, Formals, UFormals),
                return(X:T, Y:R),
                (   R == bool ->
                    DK =.. [E|UFormals]
                ;   Call =.. [E|UFormals],
                    uppercase_atom(Y, Yu),
                    DK = (Yu=Call)
                )
            ;   T == bool ->
                DK = E
            ;   uppercase_atom(X, Xu),
                DK = (Xu=E)
            )
        ),
        dpop_write(n_e_to_c1(E, L, X:T, Env, K, DK, [])-id-cst-out), dnl.

/*
apply_equations(+K, +Eqs, -R)
*/
apply_equations(K, Eqs, R) :-
        K =.. [F|Xs],
        (   foreach(X, Xs),
            foreach(V, Vs),
            param(Eqs)
        do  (   avl_fetch(X, Eqs, V) ->
                true
            ;   V = X
            )
        ),
        R =.. [F|Vs].

/*
remove_true(+K, -R)
*/
remove_true(K, R) :-
        (   compound(K), K =.. [','|[A, B]] ->
            (   A == true ->
                R = B
            ;   B == true ->
                R = A
            ;   remove_true(A, A1),
                remove_true(B, B1),
                R = (A1, B1)
            )
        ;   R = K
        ).

/*
mk_summ_cstr(+N, +K, -SummCstr)
*/
mk_summ_cstr(N, K, (SummPred :- (K, CtxPred))) :-
        summ_sy(N, SSy),
        formals_return(N, NFormalsRet),
        maplist(type_name, NFormalsRet, FormalsRet),
        maplist(uppercase_atom, FormalsRet, UFormalsRet),
        SummPred =.. [SSy|UFormalsRet],
        ctx_sy(N, CSy),
        formals(N, NFormals),
        maplist(type_name, NFormals, Formals),
        maplist(uppercase_atom, Formals, UFormals),
        CtxPred =.. [CSy|UFormals].

/*
mk_ctx_cstr(+N, +K, -CtxCstr)
*/
mk_ctx_cstr(N, K, (CtxPred :- K)) :-
        ctx_sy(N, Sy),
        formals(N, NFormals),
        maplist(type_name, NFormals, Formals),
        maplist(uppercase_atom, Formals, UFormals),
        CtxPred =.. [Sy|UFormals].

/*
mk_conj(+Ks, -K)
*/
mk_conj(Ks, K) :-
        (   Ks == [] ->
            K = true
        ;   Ks = [K1|Kss] ->
            (   foreach(Ki, Kss),
                fromto(K1, InK, (InK, Ki), K)
            do  true
            )
        ).

/*
summ_sy(+N, -Sy)
*/
summ_sy(N, Sy) :-
        unname_type(N, T),
        N = X:_,
        format_atom('~w_~w', [X, T], Sy).

/*
ctx_sy(+N, -Sy)
*/
ctx_sy(N, Sy) :-
        unname_type(N, T),
        N = X:_,
        format_atom('ctx_~w_~w', [X, T], Sy).

/*
return(+N, -R)
*/
return(X:T, R) :-
	(   compound(T), T = (_->T2) ->
            return(T2, R)
	;   R = X:T
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
uppercase_atom(+A, -R)
*/
uppercase_atom(A, R) :-
        atom_codes(A, Ls),
        maplist(uppercase_code, Ls, Us),
        atom_codes(R, Us).
/*
uppercase_code(+L, -U)
*/
uppercase_code(Lc, Uc) :-
        atom_codes(a, [LLimit]),
        atom_codes(z, [ULimit]),
        (   LLimit=<Lc, Lc=<ULimit ->
            Uc is Lc - 32
        ;   Uc = Lc
        ).

/*
type_name(+N, -X)
*/
type_name(X:_, X).

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


% **********************************************************************
% Entry point

summarize(FileIn, _FileOut) :-
% Read the expression
        open(FileIn, read, In),
        read(In, ELT),
        close(In),
        \+ (
             ELT == end_of_file,
             print('ERROR: the input file is empty\n'),
             halt
           ),
% Check that the expression is well-formed
        (   bb_get(nowf, 1) ->
            true
        ;   (   wf_typed_exp(ELT) ->
                true
            ;   print('ERROR: the input expression is malformed\n'),
                halt
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
% Summarize the expression
        named_exp_to_constraints(ELN, Ss),
% Output the summarized program
        (   foreach(S, Ss)
        do  print(S),
            nl
        ).
