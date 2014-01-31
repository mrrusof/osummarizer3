% Modified: Wed Jan 29 15:48:54 CET 2014

:- use_module('log.pl', [start_log/0,
                         lprint/1]).
:- use_module('debug.pl', [start_debug/0,
                           dpush_write/1,
                           dpop_write/1,
                           dnl/0]).
:- use_module(library(avl), [avl_fetch/3,
                             avl_store/4]).
:- use_module('ext/utils/misc.pl', [format_atom/3]).

:- multifile user:portray/1.

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
% Pretty printing of typed and named expressions

user:portray(E@_:N:T) :-  pp_n_e(E, N, T, "").
user:portray(E@_:T) :-  pp_t_e(E, T, "").
user:portray(N:(T1->T2)) :-
        format('~p:(', [N]),
        print(T1),
        write(' -> '),
        print(T2),
        write(')').
user:portray((T1->T2)) :-
        paren_t(T1),
        write(' -> '),
        print(T2).

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
pp_n_e(+E, +N, +T, +I)
*/
pp_n_e(app(Ef@_:Nf:Tf, ELNs), N, T, I) :- !,
        append(I, "  ", J),
        format('~s(\n', [I]),
        pp_n_e(Ef, Nf, Tf, J),
        (   foreach(Ei@_:Ni:Ti, ELNs),
            param(J)
        do  write('\n'),
            pp_n_e(Ei, Ni, Ti, J)
        ),
        format('\n~s):', [I]),
        print(N:T).
pp_n_e(abs(XLNs, Eb@_:Nb:Tb), N, T, I) :-
        append(I, "  ", J),
        format('~s(fun\n', [I]),
        (   XLNs = [X@_:Nx:Tx] ->
            pp_n_e(X, Nx, Tx, J)
        ;   XLNs = [Xh@_:Nh:Th|XrLrNr] ->
            pp_n_e(Xh, Nh, Th, J),
            (   foreach(X@_:Nx:Tx, XrLrNr),
                param(J)
            do  write('\n'),
                pp_n_e(X, Nx, Tx, J)
            )
        ),
        format('\n~s->\n', [I]),
        pp_n_e(Eb, Nb, Tb, J),
        format('\n~s):', [I]),
        print(N:T).
pp_n_e(ite(E1@_:N1:T1, E2@_:N2:T2, E3@_:N3:T3), N, T, I) :- !,
        append(I, "  ", J),
        format('~s(if\n', [I]),
        pp_n_e(E1, N1, T1, J),
        format('\n~sthen\n', [I]),
        pp_n_e(E2, N2, T2, J),
        format('\n~selse\n', [I]),
        pp_n_e(E3, N3, T3, J),
        format('\n~s):', [I]),
        print(N:T).
pp_n_e(let(X@_:Nx:Tx, E1@_:N1:T1, E2@_:N2:T2), N, T, I) :- !,
        append(I, "  ", J),
        format('~s(let\n', [I]),
        pp_n_e(X, Nx, Tx, J),
        format('\n~s=\n', [I]),
        pp_n_e(E1, N1, T1, J),
        format('\n~sin\n', [I]),
        pp_n_e(E2, N2, T2, J),
        format('\n~s):', [I]),
        print(N:T).
pp_n_e(E, N, T, I) :- !,
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
        print(N:T).


% **********************************************************************
% Naming

/*
t_e_to_n_e(+ELT, -ELN)
*/
typed_exp_to_named_exp(E@L:T, ELNT) :-
        t_e_to_n_e1(E, L, T, _Fresh, empty, ELNT).

/*
choose_names(+NTenv, +NTloc, -NT)
*/
choose_names(X:S, Y:T, R) :-
        (   compound(S) ->
            S = (NS1->NS2),
            T = (NT1->NT2),
            choose_names(NS1, NT1, R1),
            choose_names(NS2, NT2, R2),
            R = X:(R1->R2)
        ;   R = Y:T
        ).

/*
t_e_to_n_e1(+E, +L, +T, +N, +Env, -ELN)
*/
t_e_to_n_e1(app(_Ef@_Lf:_Tf, _ELTs), _L, _T, _N, _Env, _E@_L:_NT) :-
        false.
t_e_to_n_e1(E, L, T, N, Env, E@L:NT) :-
        dpush_write(t_e_to_n_e1(E, L, T, N, Env, E@L:NT)-in), dnl,
        (   ml_id(E) ->
            (   function_type(T) ->
                format_atom('~p_~p', [E, N], EN),
                name_type(EN, T, NTloc),
                avl_fetch(E, Env, NTenv),
                choose_names(NTenv, NTloc, NT)
            ;   name_type(N, T, NT)
            )
        ;   string(E) ->
            format_atom('~s_~p', [E, N], EN),
            name_type(EN, T, NT)
        ;   ml_const(E) ->
            format_atom('~p_~p', [E, N], EN),
            name_type(EN, T, NT)
        ),
        dpop_write(t_e_to_n_e1(E, L, T, N, Env, E@L:NT)-out), dnl.

/*
name_type(+N, +T, -NT)
*/
name_type(N, T, N:R) :-
        (   function_type(T) ->
            T = (T1->T2),
            format_atom('~pa', [N], N1),
            format_atom('~pb', [N], N2),
            name_type(N1, T1, NT1),
            name_type(N2, T2, NT2),
            R = (NT1->NT2)
        ;   R = T
        ).


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
        ),
% Pretty print the expression
        lprint('\n'),
        lprint('* Typed expression:\n'),
        lprint(ELT),
        lprint('\n'),
% Name the expression
        typed_exp_to_named_exp(ELT, ELN),
% Pretty print the named expression
        lprint('\n'),
        lprint('* Named expression:\n'),
        lprint(ELN),
        lprint('\n').
% Summarize the expression
