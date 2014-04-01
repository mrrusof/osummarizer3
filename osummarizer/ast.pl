:- module(ast, [ml_const/1,
                string/1,
                ml_id/1,
                base_type/1,
                named_base_type/1,
                function_type/1,
                type_var/1,
                wf_typed_exp/1,
                wf_t_e/3]).
:- use_module('debug.pl', [dpush_portray_clause/1,
                           dpop_portray_clause/1]).
:- use_module('mltypes.pl', [remove_formals_ty/3]).

:- op(540, xfy, @). % A@B:C == (A@B):C because current_op(N, _, :) ---> N = 550
:- op(560, xfy, -->). % A@B:C-->D == ((A@B):C)-->D because current_op(N, _, :) ---> N = 550

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
named_base_type(_:T) :- base_type(T).
function_type(T) :- compound(T), T = (_->_).
% nullary_type(T) :- ( base_type(T) -> true ; T =..['*'|_] ).
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
