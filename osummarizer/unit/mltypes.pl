:- ['../mltypes.pl'].
:- multifile ut/2.

ut("roots 1", roots(1, f:(a_f:(aa_f:i -> ab_f:i) -> b_f:(ba_f:(baa_f:i -> bab_f:i) -> bb_f:i)), [f])).
ut("roots 2", roots(2, f:(a_f:(aa_f:i -> ab_f:i) -> b_f:(ba_f:(baa_f:i -> bab_f:i) -> bb_f:i)), [f, b_f])).
ut("roots 3", roots(2, f:(g:(x:int -> y:bool) -> h:(z:int -> u:bool)), [f, h])).
ut("formals 1", formals(f:(a_f:(aa_f:i -> ab_f:i) -> b_f:(ba_f:(baa_f:i -> bab_f:i) -> bb_f:i)),
                          [a_f:(aa_f:i -> ab_f:i),        ba_f:(baa_f:i -> bab_f:i)])).
ut("formals 2", formals(f:(g:(x:int -> y:bool) -> h:(z:int -> u:bool)),
                          [g:(x:int -> y:bool),      z:int] )).
ut("formals_return 1", formals_return(f:(a_f:(aa_f:i -> ab_f:i) -> b_f:(ba_f:(baa_f:i -> bab_f:i) -> bb_f:i)),
                                        [a_f:(aa_f:i -> ab_f:i),        ba_f:(baa_f:i -> bab_f:i),   bb_f:i]  )).
ut("formals_return 2", formals_return(f:(g:(x:int -> y:bool) -> h:(z:int -> u:bool)),
                                        [g:(x:int -> y:bool),      z:int,   u:bool]  )).
ut("return 1", return(f:(a_f:(aa_f:i -> ab_f:i) -> b_f:(ba_f:(baa_f:i -> bab_f:i) -> bb_f:i)), bb_f:i) ).
ut("return 2", return(f:(g:(x:int -> y:bool) -> h:(z:int -> u:bool)), u:bool) ).
ut("remove_formals_ty 1", remove_formals_ty(1, ((i -> i) -> (i -> i) -> i),
                                                           ((i -> i) -> i) )).
ut("remove_formals_nty 1", remove_formals_nty(1, f:(a_f:(aa_f:i -> ab_f:i) -> b_f:(ba_f:(baa_f:i -> bab_f:i) -> bb_f:i)),
                                                                              b_f:(ba_f:(baa_f:i -> bab_f:i) -> bb_f:i)  )).
ut("remove_formals_nty 2", remove_formals_nty(2, f:(a_f:(aa_f:i -> ab_f:i) -> b_f:(ba_f:(baa_f:i -> bab_f:i) -> bb_f:i)),
                                                                                                                bb_f:i   )).
ut("remove_formals_nty 3", remove_formals_nty(1, f:(g:(x:int -> y:bool) -> h:(z:int -> u:bool)),
                                                                           h:(z:int -> u:bool)  )).
ut("remove_formals_nty 4", remove_formals_nty(2, f:(g:(x:int -> y:bool) -> h:(z:int -> u:bool)),
                                                                                       u:bool   )).
ut("unname_type 1", unname_type(v1:(v11:(v111:i->v112:i)->v12:(v121:(v1211:i->v1212:i)->v122:i)),
                                   (    (     i->     i)->          (      i->      i)->     i ) )).
ut("name_of_type 1", name_of_type(v1:(v11:(v111:i->v112:i)->v12:(v121:(v1211:i->v1212:i)->v122:i)), v1)).
