:- use_module('log.pl', [lprint/1,
                         start_log/0,
                         lpush_format/2,
                         lpop_format/2]).

% Define operator @
% A@B:C == (A@B):C because current_op(N, _, :) ---> N = 550
:- op(540, xfy, @).


% **********************************************************************
% Syntax

/*
Expressions:
e ::= c
    | x
    | (e, ..., e)
    | if e then e else e
    | fun x ... x -> e
    | e e ... e
    | let x = e in e
    | let rec x = e and ... and x = e in e
    | match e with | p when e -> e ... | p when e -> e
p                  ::=  x |  app(x, [e, ..., e])
c                  ::=  number | true | false | pervasives
x                  ::=  atom
(e, ..., e)        ::=  tup([e, ..., e])
if e then e else e ::=  ite(e, e, e)
fun x ... x -> e   ::=  abs([x, ..., x], e)
e e ... e          ::=  app(e, [e, ..., e])
let x = e in e     ::=  let(x, e, e)
let rec x = e in e ::=  ltr([(x, e), ..., (x, e)], e)
match e with
 | p when e -> e
 ...
 | p when e -> e   ::=  mae(e, [(p, e, e), ..., (p, e, e)])
assert(e)          ::=  assert(e)
assume(e)          ::=  assume(e)

Types:
t ::= bool | int | 'a | t -> t | k(t, ..., t)
k ::= atom
'a ::= prolog var

Named types:
nt ::= x:bool | x:int | x:'a | x:nt -> x:nt | x:k(nt, ..., nt)
*/

ml_const(C) :- ground(C), ( ml_const_pervasives(C) ; ml_const_path(C) ).
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
ml_const_pervasives(C) :- ( number(C) -> true ; string(C) ).

string(C) :- ( foreach(N, C) do number(N) ).

ml_id(X) :- atom(X), \+ml_const(X).

base_type(T) :- atom(T).
type_var(T) :- var(T).


/*
Well-formedness of expressions

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
% Pretty printing

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
            (   wf_t_e(ELT) ->
                true
            ;   print('ERROR: the input expression is malformed\n')
            )
        ;   true
        ).
% Pretty print the expression
% Name the expression
% Pretty print the named expression
% Summarize the expression
