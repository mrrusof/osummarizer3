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