:- module(mltypes, [roots/3,
                    formals/2,
                    formals_return/2,
                    return/2,
                    remove_formals_ty/3,
                    remove_formals_nty/3,
                    unname_type/2,
                    name_of_type/2]).

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
formals(+N, -Formals)
*/
formals(_:T, Formals) :-
        (   compound(T), T = (R->T2) ->
            formals(T2, Rs),
            Formals = [R|Rs]
        ;   Formals = []
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
return(+N, -R)
*/
return(X:T, R) :-
	(   compound(T), T = (_->T2) ->
            return(T2, R)
	;   R = X:T
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

/*
remove_formals_nty(+Count, +N, -R) :-
*/
remove_formals_nty(Count, N, R) :-
	(   Count > 0 ->
	    N = _:(_->N2),
	    Count1 is Count-1,
	    remove_formals_nty(Count1, N2, R)
	;   R = N
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
