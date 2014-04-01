:- module(plterms, [uppercase_atom/2,
                    pretty_ground_term/2,
                    mk_conj/2,
                    flatten_tuple/2,
                    simplify_formula/2]).
:- use_module('ext/utils/list_utils.pl', [list2tuple/2,
                                          tuple2flatlist/2]).
:- use_module(library(lists), [maplist/3]).
:- use_module(library(terms), [term_variables/2]).

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
pretty_ground_term(+Term, -Ground)
*/
pretty_ground_term(T1, T2) :-
        copy_term(T1, T2),
        term_variables(T2, Vars),
        (   foreach(V, Vars),
            count(N, 65, _)
        do  atom_codes(A, [N]),
            V = A
        ).

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
