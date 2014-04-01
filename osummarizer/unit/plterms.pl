:- ['../plterms.pl'].
:- multifile ut/2.

ut("uppercase_atom 1", uppercase_atom(str, 'STR') ).
ut("pretty_ground_term 1", pretty_ground_term(abs([g@l:(A->B), x@l:A],
                                                app(g@l:(A->B),
                                                    [x@l:A]
                                                   )@l:B
                                               )@l:((A->B)->A->B),
                                            abs([g@l:('A'->'B'), x@l:'A'],
                                                app(g@l:('A'->'B'),
                                                    [x@l:'A']
                                                   )@l:'B'
                                               )@l:(('A'->'B')->'A'->'B'))).
ut("mk_conj 1", mk_conj(((('BA_ADD_V'=2, 'A_ADD_V'=1), true), true), ('BA_ADD_V'=2, 'A_ADD_V'=1))).
ut("flatten_tuple 1", flatten_tuple((true, false), (true, false))).
ut("flatten_tuple 2", flatten_tuple((true, (false, true)), (true, false, true))).
ut("flatten_tuple 3", flatten_tuple((true, false), true), (true, false, true)).
ut("simplify_formula 1", simplify_formula(('X2'>1,'ctx_f1_int->unit'('X2'),true), ('X2'>1,'ctx_f1_int->unit'('X2'))) ).
ut("simplify_formula 2", simplify_formula(('X2'>1,true,'ctx_f1_int->unit'('X2')), ('X2'>1,'ctx_f1_int->unit'('X2'))) ).
ut("simplify_formula 3", simplify_formula((true,'X2'>1,'ctx_f1_int->unit'('X2')), ('X2'>1,'ctx_f1_int->unit'('X2'))) ).
ut("simplify_formula 4", simplify_formula(('BA_ADD_V'=2,'A_ADD_V'=1,true,true), ('BA_ADD_V'=2,'A_ADD_V'=1))).
ut("simplify_formula 5", simplify_formula(('BA_ADD_V'=2,'A_ADD_V'=1 ; false), ('BA_ADD_V'=2,'A_ADD_V'=1))).
ut("simplify_formula 6", simplify_formula(((false, true) ; 'BA_ADD_V'=2,'A_ADD_V'=1), ('BA_ADD_V'=2,'A_ADD_V'=1))).
