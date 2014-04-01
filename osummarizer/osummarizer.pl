:- use_module('plterms.pl',              [uppercase_atom/2,
                                          mk_conj/2,
                                          flatten_tuple/2,
                                          simplify_formula/2]).
:- use_module('mltypes.pl',              [roots/3,
                                          formals/2,
                                          return/2,
                                          remove_formals_nty/3,
                                          unname_type/2,
                                          name_of_type/2]).
:- use_module('ast.pl',                  [ml_const/1,
                                          ml_id/1,
                                          named_base_type/1,
                                          function_type/1,
                                          wf_typed_exp/1]).
:- use_module('pp.pl',                   [start_pp/0,
                                          push_pp/0,
                                          pop_pp/0]).
:- use_module('log.pl',                  [start_log/0,
                                          if_log/1,
                                          lpush/0,
                                          lpop/0,
                                          lindent/0,
                                          lprint/1,
                                          lformat/2]).
:- use_module('debug.pl',                [start_debug/0,
                                          dpush_portray_clause/1,
                                          dpop_portray_clause/1,
                                          dnl/0]).
:- use_module('ext/utils/misc.pl',       [format_atom/3]).

:- use_module('ext/utils/list_utils.pl', [list2tuple/2]).
:- use_module(library(avl),              [avl_fetch/3,
                                          avl_store/4,
                                          avl_to_list/2]).
:- use_module(library(lists),            [rev/2,
                                          maplist/3,
                                          include/3]).
:- use_module(library(ordsets),          [ord_add_element/3,
                                          ord_union/2]).



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
        remove_formals_nty(Count, PreNf, _:Npre),
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
        dpush_portray_clause(n_e_to_p_e1(E, L, X:T, E@L:X:T-->Kd)-id-cst-in),
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
        dpop_portray_clause(n_e_to_p_e1(E, L, X:T, E@L:X:T-->Kd)-id-cst-out).

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



% **********************************************************************
% Procedure definitions of expressions

/*
p_e_to_p_d1(+E, +L, +N, +K, +Kd, +D, -Dd)
*/
p_e_to_p_d1(app(nondet@Lf:Nf, [unit@Lu:Nu]), L, X:T, K, Kd, D, D) :- !,
        dpush_portray_clause(p_e_to_p_d1(app(nondet@Lf:Nf, [unit@Lu:Nu]), L, X:T, K, Kd, D, D)-in),
        dpop_portray_clause(p_e_to_p_d1(app(nondet@Lf:Nf, [unit@Lu:Nu]), L, X:T, K, Kd, D, D)-out).
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
p_e_to_c1(app(nondet@Lf:Nf, [unit@Lu:Nu]), L, N, Env, K, Kd, D, []) :- !,
        dpush_portray_clause(p_e_to_c1(app(nondet@Lf:Nf, [unit@Lu:Nu]), L, N, Env, K, Kd, D, [])-in),
        dpop_portray_clause(p_e_to_c1(app(nondet@Lf:Nf, [unit@Lu:Nu]), L, N, Env, K, Kd, D, [])-out).
p_e_to_c1(app(Ef@Lf:Xf:Tf, ELNKs), L, N, Env, K, Kd, D, S) :- !,
        dpush_portray_clause(p_e_to_c1(app(Ef@Lf:Xf:Tf, ELNKs), L, N, Env, K, Kd, D, S)-in),
        (   ml_const(Ef) ->
            % construct parameter passing constraint for call to Ef
            (   foreach(Ei@Li:Xi:Ti-->Ki, ELNKs),
                foreach(Si, Ss),
                param(Env, K, D)
            do  p_e_to_c1(Ei, Li, Xi:Ti, Env, K, Ki, D, Si)
            ),
            ord_union(Ss, S)
        ;   ml_id(Ef) ->
            (   avl_fetch(Ef, D, (K0, PreELT0)) -> % Ef is let bound identifier
                % instantiate type of Ef
                copy_term(PreELT0, E0@L0:T0),
                unname_type(Ef:Tf, T0),
                % analyze definition of Ef under appropriate context
                t_e_to_c1(E0, L0, T0, Ef, Env, K0, D, N0, Kd0, S0),
                % construct summary constraint for Ef
                mk_ctx_pred(N0, Ctx0),
                mk_conj((Kd0, Ctx0, K0), SummRel),
                mk_summ_pred(N0, Summ),
                ord_add_element(S0, (Summ :- SummRel), Sf)
            ;   Sf=[]                              % Ef is formal parameter
            ),
            % construct parameter passing constraint for call to Ef
            (   foreach(Ei@Li:Xi:Ti-->Ki, ELNKs),
                fromto(true, InKs, OutKs, Ks),
                foreach(Si, Ss),
                param(Env, K, D)
            do  (   function_type(Ti) ->
                    OutKs = InKs,
                    mk_ctx_pred(Xi:Ti, Ctxi),
                    p_e_to_c1(Ei, Li, Xi:Ti, Env, (Ctxi, K), Ki, D, Si1),
                    mk_conj((Ki, Ctxi, K), SummReli),
                    mk_summ_pred(Xi:Ti, Summi),
                    ord_add_element(Si1, (Summi :- SummReli), Si)
                ;   OutKs = (Ki, InKs),
                    p_e_to_c1(Ei, Li, Xi:Ti, Env, K, Ki, D, Si)
                )
            ),
            mk_conj((Ks, K), Actuals),
            mk_ctx_pred(Ef:Tf, CtxEf),
            ord_add_element(Sf, (CtxEf :- Actuals), Sfp),
            ord_union([Sfp|Ss], S)
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
        include(named_base_type, NFormalsRet, NullNFormalsRet),
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
        include(named_base_type, NFormals, NullNFormals),
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



% **********************************************************************
% Summarization procedure

typed_exp_to_constraints(E@L:T, S) :-
        t_e_to_c1(E, L, T, v, empty, true, empty, _N, _Kd, S).

/*
t_e_to_c1(+E, +L, +T, +X, +Env, +K, +D, -N, -Kd, -S)
*/
t_e_to_c1(E, L, T, X, Env, K, D, N, Kd, S) :-
        dpush_portray_clause(t_e_to_c1(E, L, T, X, Env, K, D, N, Kd, S)-in),
        lprint('\n'),
        lindent, lprint('* Typed expression:\n'),
        lprint(E@L:T),
        lprint('\n'),
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
        (   function_type(T) ->
            mk_ctx_pred(N, Ctx)
        ;   Ctx = true
        ),
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
