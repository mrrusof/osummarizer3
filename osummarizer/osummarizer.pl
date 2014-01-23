% Modified: Thu Jan 23 17:45:41 CET 2014


% **********************************************************************
% Misc

:- use_module('log.pl', [lprint/1,
                         start_log/0,
                         lpush_format/2,
                         lpop_format/2]).

% Define operator @
% A@B:C == (A@B):C because current_op(N, _, :) ---> N = 550
:- op(540, xfy, @).


% **********************************************************************
% Syntax and Well-formedness of typed expressions

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

/*
Cases for typed expressions and corresponding examples

Nullary constant                :  c:i
Function constant               :  c:(i -> i)
Nullary identifier              :  x:i
Function identifier             :  x:(i -> i)
Application                     :  (+:(i -> i ->) 1:i 2:i):i
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

base_type(T) :- atom(T).
type_var(T) :- var(T).

/*

* Well-formedness of typed expressions

wf_typed_exp(+ELT)
*/
wf_typed_exp(E@L:T) :- wf_t_e(E, L, T).

/*
wf_t_e(+E, +L, +T)
*/
wf_t_e(ite(E1@L1:T1, E2@L2:T2, E3@L3:T3), L, T) :- !,
        lpush_format('wf_t_e.ite.in : ~p\n', [ite(E1@L1:T1, E2@L2:T2, E3@L3:T3)@L:T]),
        wf_l(L),
        wf_t(T),
        T2 == T3,
        T == T3,
        wf_t_e(E1, L1, T1),
        wf_t_e(E2, L2, T2),
        wf_t_e(E3, L3, T3),
        lpop_format('wf_t_e.ite.out: ~p\n', [ite(E1@L1:T1, E2@L2:T2, E3@L3:T3)@L:T]).
wf_t_e(abs(XLTs, Eb@Lb:Tb), L, T) :- !,
        lpush_format('wf_t_e.abs.in : ~p\n', [abs(XLTs, Eb@Lb:Tb)@L:T]),
        wf_l(L),
        wf_t(T),
        (   foreach(Xi@Li:Ti, XLTs)
        do  ml_id(Xi),
            wf_l(Li),
            wf_t(Ti)
        ),
        wf_t_e(Eb, Lb, Tb),
        lpop_format('wf_t_e.abs.out: ~p\n', [abs(XLTs, Eb@Lb:Tb)@L:T]).
wf_t_e(app(Ef@Lf:Tf, ELTs), L, T) :- !,
        lpush_format('wf_t_e.app.in : ~p\n', [app(Ef@Lf:Tf, ELTs)@L:T]),
        wf_l(L),
        wf_t(T),
        wf_t_e(Ef, Lf, Tf),
        (   foreach(Ei@Li:Ti, ELTs)
        do  wf_t_e(Ei, Li, Ti)
        ),
        lpop_format('wf_t_e.app.out: ~p\n', [app(Ef@Lf:Tf, ELTs)@L:T]).
wf_t_e(let(X@Lx:Tx, E1@L1:T1, E2@L2:T2), L, T) :- !,
        lpush_format('wf_t_e.let.in : ~p\n', [let(X@Lx:Tx, E1@L1:T1, E2@L2:T2)@L:T]),
        wf_l(L),
        wf_t(T),
        ml_id(X),
        wf_l(Lx),
        wf_t(Tx),
        wf_t_e(E1, L1, T1),
        wf_t_e(E2, L2, T2),
        lpop_format('wf_t_e.let.out: ~p\n', [let(X@Lx:Tx, E1@L1:T1, E2@L2:T2)@L:T]).
wf_t_e(E, L, T) :- !,
        lpush_format('wf_t_e.const/ident.in : ~p\n', [E@L:T]),
        wf_l(L),
        wf_t(T),
        ( ml_const(E) ; ml_id(E) ),
        lpop_format('wf_t_e.const/ident.out: ~p\n', [E@L:T]).

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
        ;   ( base_type(T) ; type_var(T) )
        ).


% **********************************************************************
% Pretty printing of typed expressions

% TODO


% **********************************************************************
% Pretty printing of named expressions

% TODO


% **********************************************************************
% Naming

% TODO


% **********************************************************************
% Summarizing

% TODO


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
            ;   print('ERROR: the input expression is malformed\n')
            )
        ).
% Pretty print the expression
% Name the expression
% Pretty print the named expression
% Summarize the expression
