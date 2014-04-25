:- use_module(library(codesio), [format_to_codes/3]).
:- set_prolog_flag(discontiguous_warnings, off).
:- multifile ut/2.

:- ['../debug.pl'].
:- ['../log.pl'].
:- ['../osummarizer.pl'].
:- ['../ast.pl'].
:- ['../pp.pl'].



% **********************************************************************
% Tests for modules
:- ['plterms.pl'].
:- ['mltypes.pl'].
:- ['ast.pl'].
:- ['pp.pl'].



% **********************************************************************
% Naming of typed expressions

ut("name type with function formals", (   X = ((i -> i) -> ((i -> i) -> i)),
                                          name_type(f, X, Y),
                                          Y == f:(a_f:(aa_f:i -> ab_f:i) -> b_f:(ba_f:(baa_f:i -> bab_f:i) -> bb_f:i)) )).
ut("name type with type variable formals", (   X = (i -> ((i -> B) -> B)),
                                               name_type(f, X, Y),
                                               Y == f:(a_f:i -> b_f:(ba_f:(baa_f:i -> bab_f:B) -> bb_f:B)) )).
ut("choose_names 1", choose_names(inc  :(x      :int -> r     :int),
                                  inc_x:(a_inc_x:int -> b_inc_x:int),
                                  inc  :(a_inc_x:int -> b_inc_x:int) )).
ut("choose_names 2", choose_names(max1:(x2:int -> f1_max1:(y3:int -> ret_max1:int)),
                                  max1_v:(a_max1_v:int -> f1_max1:(ba_max1_v:int -> bb_max1_v:int)),
                                  max1:(a_max1_v:int -> f1_max1:(ba_max1_v:int -> bb_max1_v:int))   )).
ut("choose_names 3", choose_names(app  :(g      :(a_g     :A  ->b_g     :B                             )->f1_app :(x       :A  ->ret_app :B                             )),
                                  app_v:(a_app_v:(aa_app_v:int->ab_app_v:(aba_app_v:int->abb_app_v:int))->b_app_v:(ba_app_v:int->bb_app_v:(bba_app_v:int->bbb_app_v:int))),
                                  app  :(g      :(aa_app_v:int->b_g     :(aba_app_v:int->abb_app_v:int))->f1_app :(ba_app_v:int->ret_app :(bba_app_v:int->bbb_app_v:int))))).



% **********************************************************************
% THE REST

% **********************************************************************
% Naming of typed expressions

ut("naming id x", t_e_to_n_e1(x, loc('max.ml', 0, 0, 0, 0, 0, 0), int, r, empty,
                              x@loc('max.ml', 0, 0, 0, 0, 0, 0):r:int )).
ut("naming id inc", (   avl_store(inc, empty, inc:(x:int -> r:int), Env),
                        t_e_to_n_e1(inc, loc('max.ml', 0, 0, 0, 0, 0, 0), (int -> int), x, Env,
                                    inc@loc('max.ml', 0, 0, 0, 0, 0, 0):x:(a_x:int -> b_x:int)) )).
ut("naming id max1", (   avl_store(max1, empty, max1:(x2:int -> f1_max1:(y3:int -> ret_max1:int)), Env),
                         t_e_to_n_e1(max1, loc('max.ml', 0, 0, 0, 0, 0, 0), (int -> int -> int), v, Env,
                                     max1@loc('max.ml', 0, 0, 0, 0, 0, 0):v:(a_v:int -> f1_max1:(ba_v:int -> bb_v:int))) )).
ut("naming app +", (   E@L:T = app(
                                   '+'@loc('max.ml', 0, 0, 0, 0, 0, 0):(int -> int -> int),
                                   [1@loc('max.ml', 0, 0, 0, 0, 0, 0):int,
                                    2@loc('max.ml', 0, 0, 0, 0, 0, 0):int]
                                  )@loc('max.ml', 0, 0, 0, 0, 0, 0):bool,
                       t_e_to_n_e1(E, L, T, ret, empty,
                                   app(
                                       '+'@loc('max.ml', 0, 0, 0, 0, 0, 0):'plus_ret':('a_plus_ret':int -> 'b_plus_ret':('ba_plus_ret':int -> ret:int)),
                                       [1@loc('max.ml', 0, 0, 0, 0, 0, 0):'a_plus_ret':int,
                                        2@loc('max.ml', 0, 0, 0, 0, 0, 0):'ba_plus_ret':int]
                                      )@loc('max.ml', 0, 0, 0, 0, 0, 0):ret:int) )).
ut("naming app >", (   E@L:T = app(
                                   '>'@loc('max.ml', 0, 0, 0, 0, 0, 0):(int -> int -> bool),
                                   [x2@loc('max.ml', 0, 0, 0, 0, 0, 0):int,
                                    y3@loc('max.ml', 0, 0, 0, 0, 0, 0):int]
                                  )@loc('max.ml', 0, 0, 0, 0, 0, 0):bool,
                       t_e_to_n_e1(E, L, T, c_ret_max1, empty,
                                   app(
                                       '>'@loc('max.ml', 0, 0, 0, 0, 0, 0):'gt_c_ret_max1':('a_gt_c_ret_max1':int -> 'b_gt_c_ret_max1':('ba_gt_c_ret_max1':int -> c_ret_max1:bool)),
                                       [x2@loc('max.ml', 0, 0, 0, 0, 0, 0):'a_gt_c_ret_max1':int,
                                        y3@loc('max.ml', 0, 0, 0, 0, 0, 0):'ba_gt_c_ret_max1':int]
                                      )@loc('max.ml', 0, 0, 0, 0, 0, 0):c_ret_max1:bool) )).
ut("naming abs id", (   E@L:T = abs([x@loc('id.ml', 0, 0, 0, 0, 0, 0):int],
                                    x@loc('id.ml', 0, 0, 0, 0, 0, 0):int
                                   )@loc('id.ml', 0, 0, 0, 0, 0, 0):(int -> int),
                        t_e_to_n_e1(E, L, T, id, empty,
                                    abs([x@loc('id.ml', 0, 0, 0, 0, 0, 0):x:int],
                                        x@loc('id.ml', 0, 0, 0, 0, 0, 0):ret_id:int
                                       )@loc('id.ml', 0, 0, 0, 0, 0, 0):id:(x:int -> ret_id:int)) )).
ut("naming abs snd", (   E@L:T = abs([x@loc('snd.ml', 0, 0, 0, 0, 0, 0):int, y@loc('snd.ml', 0, 0, 0, 0, 0, 0):int],
                                     y@loc('snd.ml', 0, 0, 0, 0, 0, 0):int
                                    )@loc('snd.ml', 0, 0, 0, 0, 0, 0):(int -> int -> int),
                         t_e_to_n_e1(E, L, T, snd, empty,
                                     abs([x@loc('snd.ml', 0, 0, 0, 0, 0, 0):x:int, y@loc('snd.ml', 0, 0, 0, 0, 0, 0):y:int],
                                         y@loc('snd.ml', 0, 0, 0, 0, 0, 0):ret_snd:int
                                        )@loc('snd.ml', 0, 0, 0, 0, 0, 0):snd:(x:int -> f1_snd:(y:int -> ret_snd:int))) )).
ut("naming ite", (   E@L:T = ite(true@loc('max.ml', 0, 0, 0, 0, 0, 0):bool,
                                 x@loc('max.ml', 0, 0, 0, 0, 0, 0):int,
                                 y@loc('max.ml', 0, 0, 0, 0, 0, 0):int
                                )@loc('max.ml', 0, 0, 0, 0, 0, 0):int,
                     t_e_to_n_e1(E, L, T, ret, empty,
                                 ite(true@loc('max.ml', 0, 0, 0, 0, 0, 0):c_ret:bool,
                                     x@loc('max.ml', 0, 0, 0, 0, 0, 0):ret:int,
                                     y@loc('max.ml', 0, 0, 0, 0, 0, 0):ret:int
                                    )@loc('max.ml', 0, 0, 0, 0, 0, 0):ret:int) )).
ut("naming let", (   E@L:T = let(x@loc('max.ml', 0, 0, 0, 0, 0, 0):int,
                                 1@loc('max.ml', 0, 0, 0, 0, 0, 0):int,
                                 x@loc('max.ml', 0, 0, 0, 0, 0, 0):int
                                )@loc('max.ml', 0, 0, 0, 0, 0, 0):int,
                     t_e_to_n_e1(E, L, T, v, empty,
                                 let(x@loc('max.ml', 0, 0, 0, 0, 0, 0):x:int,
                                     1@loc('max.ml', 0, 0, 0, 0, 0, 0):x:int,
                                     x@loc('max.ml', 0, 0, 0, 0, 0, 0):v:int
                                    )@loc('max.ml', 0, 0, 0, 0, 0, 0):v:int) )).
ut("naming assert true", (   E@L:T = assert(
                                            true@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):bool
                                           )@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):unit,
                             t_e_to_n_e1(E, L, T, ret_f1, empty,
                                         assert(
                                                true@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):ase_ret_f1:bool
                                               )@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):ret_f1:unit) )).
ut("naming assert gt", (   E@L:T = assert(app('>'@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):(int -> int -> bool),
                                              ['x2'@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):int,
                                               0@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):int]
                                             )@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):bool
                                         )@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):unit,
                           t_e_to_n_e1(E, L, T, ret_f1, empty,
                                       assert(app('>'@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):gt_ase_ret_f1:(a_gt_ase_ret_f1:int -> b_gt_ase_ret_f1:(ba_gt_ase_ret_f1:int -> ase_ret_f1:bool)),
                                                  ['x2'@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):a_gt_ase_ret_f1:int,
                                                   0@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):ba_gt_ase_ret_f1:int]
                                                 )@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):ase_ret_f1:bool
                                             )@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):ret_f1:unit) )).
ut("naming assume true", (   E@L:T = assume(
                                            true@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):bool
                                           )@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):unit,
                             t_e_to_n_e1(E, L, T, '_3', empty,
                                         assume(
                                                true@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):asu__3:bool
                                               )@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):'_3':unit) )).
ut("naming assume gt", (   E@L:T = assume(app('>'@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):(int -> int -> bool),
                                              ['x2'@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):int,
                                               1@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):int]
                                             )@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):bool
                                         )@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):unit,
                           t_e_to_n_e1(E, L, T, '_3', empty,
                                       assume(app(> @loc('assume_assert.ml',0,0,0,0,0,0):gt_asu__3:(a_gt_asu__3:int->b_gt_asu__3:(ba_gt_asu__3:int->asu__3:bool)),
                                                  [x2@loc('assume_assert.ml',0,0,0,0,0,0):a_gt_asu__3:int,
                                                   1@loc('assume_assert.ml',0,0,0,0,0,0):ba_gt_asu__3:int]
                                                 )@loc('assume_assert.ml',0,0,0,0,0,0):asu__3:bool
                                             )@loc('assume_assert.ml',0,0,0,0,0,0):'_3':unit) )).
ut("naming max int", (   E@L:T = let('max1'@loc('max.ml', 0, 0, 0, 0, 0, 0):(int -> int -> int),
                                     abs(['x2'@loc('max.ml', 0, 0, 0, 0, 0, 0):int,
                                          'y3'@loc('max.ml', 0, 0, 0, 0, 0, 0):int
                                          ],
                                         ite(app('>'@loc('max.ml', 0, 0, 0, 0, 0, 0):(int -> int -> bool),
                                                 ['x2'@loc('max.ml', 0, 0, 0, 0, 0, 0):int,
                                                  'y3'@loc('max.ml', 0, 0, 0, 0, 0, 0):int
                                                  ]
                                                )@loc('max.ml', 0, 0, 0, 0, 0, 0):bool,
                                             'x2'@loc('max.ml', 0, 0, 0, 0, 0, 0):int,
                                             'y3'@loc('max.ml', 0, 0, 0, 0, 0, 0):int
                                             )@loc('max.ml', 0, 0, 0, 0, 0, 0):int
                                        )@loc('max.ml', 0, 0, 0, 0, 0, 0):(int -> int -> int),
                                     app('max1'@loc('max.ml', 0, 0, 0, 0, 0, 0):(int -> int -> int),
                                         [3@loc('max.ml', 0, 0, 0, 0, 0, 0):int,
                                          1@loc('max.ml', 0, 0, 0, 0, 0, 0):int
                                         ]
                                        )@loc('max.ml', 0, 0, 0, 0, 0, 0):int
                                    )@loc('max.ml', 0, 0, 0, 0, 0, 0):int,
                         t_e_to_n_e1(E, L, T, v, empty,
                                     let('max1'@loc('max.ml', 0, 0, 0, 0, 0, 0):max1:(x2:int -> f1_max1:(y3:int -> ret_max1:int)),
                                         abs(['x2'@loc('max.ml', 0, 0, 0, 0, 0, 0):int,
                                              'y3'@loc('max.ml', 0, 0, 0, 0, 0, 0):int
                                              ],
                                             ite(app('>'@loc('max.ml', 0, 0, 0, 0, 0, 0):(int -> int -> bool),
                                                     ['x2'@loc('max.ml', 0, 0, 0, 0, 0, 0):int,
                                                      'y3'@loc('max.ml', 0, 0, 0, 0, 0, 0):int
                                                      ]
                                                    )@loc('max.ml', 0, 0, 0, 0, 0, 0):bool,
                                                 'x2'@loc('max.ml', 0, 0, 0, 0, 0, 0):int,
                                                 'y3'@loc('max.ml', 0, 0, 0, 0, 0, 0):int
                                                 )@loc('max.ml', 0, 0, 0, 0, 0, 0):int
                                            )@loc('max.ml', 0, 0, 0, 0, 0, 0):(int -> int -> int),
                                         app('max1'@loc('max.ml', 0, 0, 0, 0, 0, 0):max1_v:(a_max1_v:int -> f1_max1:(ba_max1_v:int -> v:int)),
                                             [3@loc('max.ml', 0, 0, 0, 0, 0, 0):a_max1_v:int,
                                              1@loc('max.ml', 0, 0, 0, 0, 0, 0):ba_max1_v:int]
                                            )@loc('max.ml', 0, 0, 0, 0, 0, 0):v:int
                                        )@loc('max.ml', 0, 0, 0, 0, 0, 0):v:int) )).



% **********************************************************************
% | c                                                      Constant

% unit:v:unit --> V=1
ut("naming const          ()", t_e_to_n_e1(unit, loc('c.ml', 0, 0, 0, 0, 0, 0), unit, v, empty, unit@loc('c.ml', 0, 0, 0, 0, 0, 0):v:unit)).
ut("Negative naming const ()", \+ t_e_to_n_e1(unit, loc('c.ml', 0, 0, 0, 0, 0, 0), unit, v, empty, unit@loc('c.ml', 0, 0, 0, 0, 0, 0):unit:v)).
ut("path   const          ()", n_e_to_p_e1(unit, loc('c.ml', 0, 0, 0, 0, 0, 0), v:unit, unit@loc('c.ml', 0, 0, 0, 0, 0, 0):v:unit-->('V'=1))).
ut("Negative path 1 const ()", \+ n_e_to_p_e1(unit, loc('c.ml', 0, 0, 0, 0, 0, 0), v:unit, unit@loc('c.ml', 0, 0, 0, 0, 0, 0):v:unit-->1)).
ut("Negative path 2 const ()", \+ n_e_to_p_e1(unit, loc('c.ml', 0, 0, 0, 0, 0, 0), v:unit, unit@loc('c.ml', 0, 0, 0, 0, 0, 0):v:unit-->true)).
ut("summ   const          ()", p_e_to_c1(unit, loc('c.ml', 0, 0, 0, 0, 0, 0), v:unit, empty, true, 'V'=1, empty, [])).
ut("Negative summ   const ()", \+ p_e_to_c1(unit, loc('c.ml', 0, 0, 0, 0, 0, 0), v:unit, empty, true, 'V'=1, empty, [_])).

ut("PP typed const        ()", pp(unit@loc('c.ml', 0, 0, 0, 0, 0, 0):unit, "unit:unit")).
ut("PP named const        ()", pp(unit@loc('c.ml', 0, 0, 0, 0, 0, 0):v:unit, "unit:v:unit")).
ut("PP path  const        ()", pp(unit@loc('c.ml', 0, 0, 0, 0, 0, 0):v:unit-->'V'=1, "unit:v:unit --> V=1")).

% true:v:bool --> V=1
ut("naming const true", t_e_to_n_e1(true, loc('c.ml', 0, 0, 0, 0, 0, 0), bool, v, empty, true@loc('c.ml', 0, 0, 0, 0, 0, 0):v:bool)).
ut("path   const true", n_e_to_p_e1(true, loc('c.ml', 0, 0, 0, 0, 0, 0), v:bool, true@loc('c.ml', 0, 0, 0, 0, 0, 0):v:bool-->('V'=1))).
ut("Negative path 1 const true", \+ n_e_to_p_e1(true, loc('c.ml', 0, 0, 0, 0, 0, 0), v:bool, true@loc('c.ml', 0, 0, 0, 0, 0, 0):v:bool-->('V'=true))).
ut("Negative path 2 const true", \+ n_e_to_p_e1(true, loc('c.ml', 0, 0, 0, 0, 0, 0), v:bool, true@loc('c.ml', 0, 0, 0, 0, 0, 0):v:bool-->true)).
ut("Negative path 3 const true", \+ n_e_to_p_e1(true, loc('c.ml', 0, 0, 0, 0, 0, 0), v:bool, true@loc('c.ml', 0, 0, 0, 0, 0, 0):v:bool-->1)).
ut("summ   const true", p_e_to_c1(true, loc('c.ml', 0, 0, 0, 0, 0, 0), v:bool, empty, true, 'V'=1, empty, [])).

ut("PP typed const true", pp(true@loc('c.ml', 0, 0, 0, 0, 0, 0):bool, "true:bool")).
ut("PP named const true", pp(true@loc('c.ml', 0, 0, 0, 0, 0, 0):v:bool, "true:v:bool")).
ut("PP path  const true", pp(true@loc('c.ml', 0, 0, 0, 0, 0, 0):v:bool-->'V'=1, "true:v:bool --> V=1")).

% 10:v:int --> V=10
ut("naming const 10", t_e_to_n_e1(10, loc('c.ml', 0, 0, 0, 0, 0, 0), int, v, empty, 10@loc('c.ml', 0, 0, 0, 0, 0, 0):v:int)).
ut("path   const 10", n_e_to_p_e1(10, loc('c.ml', 0, 0, 0, 0, 0, 0), v:int, 10@loc('c.ml', 0, 0, 0, 0, 0, 0):v:int-->('V'=10))).
ut("summ   const 10", p_e_to_c1(10, loc('c.ml', 0, 0, 0, 0, 0, 0), v:int, empty, true, 'V'=10, empty, [])).

% "hola":str:string --> STR="hola"
ut("naming const \"hola\"", t_e_to_n_e1("hola", loc('c.ml', 0, 0, 0, 0, 0, 0), string, str, empty, "hola"@loc('c.ml', 0, 0, 0, 0, 0, 0):str:string)).
ut("path   const \"hola\"", n_e_to_p_e1("hola", loc('c.ml', 0, 0, 0, 0, 0, 0), str:string, "hola"@loc('c.ml', 0, 0, 0, 0, 0, 0):str:string-->('STR'="hola"))).
ut("summ   const \"hola\"", p_e_to_c1("hola", loc('c.ml', 0, 0, 0, 0, 0, 0), v:int, empty, true, 'STR'="hola", empty, [])).

% (+):plus_v:(a_plus_v:int -> b_plus_v:(ba_plus_v:int -> bb_plus_v:int)) --> BB_PLUS_V=A_PLUS_V+BA_PLUS_V
ut("naming          const +", t_e_to_n_e1('+', loc('c.ml', 0, 0, 0, 0, 0, 0), (int -> int -> int), plus_v, empty, '+'@loc('c.ml', 0, 0, 0, 0, 0, 0):plus_v:(a_plus_v:int -> b_plus_v:(ba_plus_v:int -> bb_plus_v:int)))).
ut("path            const +", n_e_to_p_e1('+', loc('c.ml', 0, 0, 0, 0, 0, 0), plus_v:(a_plus_v:int -> b_plus_v:(ba_plus_v:int -> bb_plus_v:int)), '+'@loc('c.ml', 0, 0, 0, 0, 0, 0):plus_v:(a_plus_v:int -> b_plus_v:(ba_plus_v:int -> bb_plus_v:int))-->('BB_PLUS_V'='A_PLUS_V'+'BA_PLUS_V'))).
ut("Negative path 1 const +", \+ n_e_to_p_e1('+', loc('c.ml', 0, 0, 0, 0, 0, 0), plus_v:(a_plus_v:int -> b_plus_v:(ba_plus_v:int -> bb_plus_v:int)), '+'@loc('c.ml', 0, 0, 0, 0, 0, 0):plus_v:(a_plus_v:int -> b_plus_v:(ba_plus_v:int -> bb_plus_v:int))-->true)).
ut("summ            const +", p_e_to_c1('+', loc('c.ml', 0, 0, 0, 0, 0, 0), plus_v:(a_plus_v:int -> b_plus_v:(ba_plus_v:int -> bb_plus_v:int)), empty, true, ('BB_PLUS_V'='A_PLUS_V'+'BA_PLUS_V'), empty, [])).
ut("Negative summ   const +", \+ p_e_to_c1('+', loc('c.ml', 0, 0, 0, 0, 0, 0), v:(a_plus_v:int -> b_plus_v:(ba_plus_v:int -> bb_plus_v:int)), empty, true, ('BB_PLUS_V'='A_PLUS_V'+'BA_PLUS_V'), empty, [_])).

ut("PP typed const +", pp('+'@loc('c.ml', 0, 0, 0, 0, 0, 0):(int -> int -> int), "(+):(int -> int -> int)")).
ut("PP named const +", pp('+'@loc('c.ml', 0, 0, 0, 0, 0, 0):plus_v:(a_plus_v:int -> b_plus_v:(ba_plus_v:int -> bb_plus_v:int)), "(+):plus_v:(a_plus_v:int -> b_plus_v:(ba_plus_v:int -> bb_plus_v:int))")).
ut("PP path  const +", pp('+'@loc('c.ml', 0, 0, 0, 0, 0, 0):plus_v:(a_plus_v:int -> b_plus_v:(ba_plus_v:int -> bb_plus_v:int))-->('BB_PLUS_V'='A_PLUS_V'+'BA_PLUS_V'), "(+):plus_v:(a_plus_v:int -> b_plus_v:(ba_plus_v:int -> bb_plus_v:int)) --> BB_PLUS_V=A_PLUS_V+BA_PLUS_V")).

% (>):gt_v:(a_gt_v:int -> b_gt_v:(ba_gt_v:int -> bb_gt_v:bool)) --> A_GT_V>BA_GT_V
ut("naming          const (>):(int->int->int)", t_e_to_n_e1('>', loc('c.ml', 0, 0, 0, 0, 0, 0), (int -> int -> bool), gt_v, empty, '>'@loc('c.ml', 0, 0, 0, 0, 0, 0):gt_v:(a_gt_v:int -> b_gt_v:(ba_gt_v:int -> bb_gt_v:bool)))).
ut("path            const (>):(int->int->int)", n_e_to_p_e1('>', loc('c.ml', 0, 0, 0, 0, 0, 0), gt_v:(a_gt_v:int -> b_gt_v:(ba_gt_v:int -> bb_gt_v:bool)), '>'@loc('c.ml', 0, 0, 0, 0, 0, 0):gt_v:(a_gt_v:int -> b_gt_v:(ba_gt_v:int -> bb_gt_v:bool))-->('A_GT_V'>'BA_GT_V' -> 'BB_GT_V'=1 ; 'BB_GT_V'=0))).
ut("Negative path 1 const (>):(int->int->int)", \+ n_e_to_p_e1('>', loc('c.ml', 0, 0, 0, 0, 0, 0), gt_v:(a_gt_v:int -> b_gt_v:(ba_gt_v:int -> bb_gt_v:bool)), '>'@loc('c.ml', 0, 0, 0, 0, 0, 0):gt_v:(a_gt_v:int -> b_gt_v:(ba_gt_v:int -> bb_gt_v:bool))-->('A_GT_V'>'BA_GT_V'))).
ut("Negative path 2 const (>):(int->int->int)", \+ n_e_to_p_e1('>', loc('c.ml', 0, 0, 0, 0, 0, 0), gt_v:(a_gt_v:int -> b_gt_v:(ba_gt_v:int -> bb_gt_v:bool)), '>'@loc('c.ml', 0, 0, 0, 0, 0, 0):gt_v:(a_gt_v:int -> b_gt_v:(ba_gt_v:int -> bb_gt_v:bool))-->true)).
ut("summ            const (>):(int->int->int)", p_e_to_c1('>', loc('c.ml', 0, 0, 0, 0, 0, 0), gt_v:(a_gt_v:int -> b_v:(ba_gt_v:int -> bb_gt_v:bool)), empty, true, ('A_GT_V'>'BA_GT_V' -> 'BB_GT_V'=1 ; 'BB_GT_V'=0), empty, [])).

ut("PP typed        const (>):(int->int->int)", pp('>'@loc('c.ml', 0, 0, 0, 0, 0, 0):(int -> int -> bool), "(>):(int -> int -> bool)")).
ut("PP named        const (>):(int->int->int)", pp('>'@loc('c.ml', 0, 0, 0, 0, 0, 0):gt_v:(a_gt_v:int -> b_gt_v:(ba_gt_v:int -> bb_gt_v:bool)), "(>):gt_v:(a_gt_v:int -> b_gt_v:(ba_gt_v:int -> bb_gt_v:bool))")).
ut("PP path         const (>):(int->int->int)", pp('>'@loc('c.ml', 0, 0, 0, 0, 0, 0):gt_v:(a_gt_v:int -> b_gt_v:(ba_gt_v:int -> bb_gt_v:bool))-->('A_GT_V'>'BA_GT_V' -> 'BB_GT_V'=1 ; 'BB_GT_V'=0), "(>):gt_v:(a_gt_v:int -> b_gt_v:(ba_gt_v:int -> bb_gt_v:bool)) --> (A_GT_V>BA_GT_V -> BB_GT_V=1 ; BB_GT_V=0)")).

ut("naming          const (&&):(bool->bool->bool)", t_e_to_n_e1('&&', l, (bool -> bool -> bool), v, empty, '&&'@l:v:(a_v:bool->b_v:(ba_v:bool->bb_v:bool)))).
ut("path            const (&&):(bool->bool->bool)", n_e_to_p_e1('&&', l, v:(a_v:bool->b_v:(ba_v:bool->bb_v:bool)), '&&'@l:v:(a_v:bool->b_v:(ba_v:bool->bb_v:bool))-->('A_V'=1,'BA_V'=1->'BB_V'=1;'BB_V'=0))).

ut("naming          const (||):(bool->bool->bool)", t_e_to_n_e1('||', l, (bool -> bool -> bool), v, empty, '||'@l:v:(a_v:bool->b_v:(ba_v:bool->bb_v:bool)))).
ut("path            const (||):(bool->bool->bool)", n_e_to_p_e1('||', l, v:(a_v:bool->b_v:(ba_v:bool->bb_v:bool)), '||'@l:v:(a_v:bool->b_v:(ba_v:bool->bb_v:bool))-->(('A_V'=1 ; 'BA_V'=1)->'BB_V'=1;'BB_V'=0))).

ut("naming          const not:(bool->bool)", t_e_to_n_e1(not, l, (bool -> bool), v, empty, not@l:v:(a_v:bool->b_v:bool))).
ut("path            const not:(bool->bool)", n_e_to_p_e1(not, l, v:(a_v:bool->b_v:bool), not@l:v:(a_v:bool->b_v:bool) --> ('A_V'=0 -> 'B_V'=1 ; 'B_V'=0))).




% **********************************************************************
% | x                                                      Identifier

% x:v:unit --> V=X
ut("naming          x:unit", t_e_to_n_e1(x, loc('x.ml', 0, 0, 0, 0, 0, 0), unit, v, empty, x@loc('x.ml', 0, 0, 0, 0, 0, 0):v:unit)).
ut("Negative naming x:unit", \+ t_e_to_n_e1(x, loc('x.ml', 0, 0, 0, 0, 0, 0), unit, v, empty, x@loc('x.ml', 0, 0, 0, 0, 0, 0):x:unit)).
ut("path            x:unit", n_e_to_p_e1(x, loc('x.ml', 0, 0, 0, 0, 0, 0), v:unit, x@loc('x.ml', 0, 0, 0, 0, 0, 0):v:unit-->('V'='X'))).
ut("Negative path 1 x:unit", \+ n_e_to_p_e1(x, loc('x.ml', 0, 0, 0, 0, 0, 0), v:unit, x@loc('x.ml', 0, 0, 0, 0, 0, 0):v:unit-->true)).
ut("Negative path 2 x:unit", \+ n_e_to_p_e1(x, loc('x.ml', 0, 0, 0, 0, 0, 0), v:unit, (x@loc('x.ml', 0, 0, 0, 0, 0, 0):v:unit-->'V')='X')).
ut("Negative path 3 x:unit", \+ n_e_to_p_e1(x, loc('x.ml', 0, 0, 0, 0, 0, 0), v:unit, x@loc('x.ml', 0, 0, 0, 0, 0, 0):v:unit-->('X'='V'))).
ut("summ            x:unit", p_e_to_c1(x, loc('x.ml', 0, 0, 0, 0, 0, 0), v:unit, empty, true, 'V'='X', empty, [])).
ut("Negative summ   x:unit", \+ p_e_to_c1(x, loc('x.ml', 0, 0, 0, 0, 0, 0), v:unit, empty, true, 'V'='X', empty, [_])).

ut("PP typed        x:unit", pp(x@loc('x.ml', 0, 0, 0, 0, 0, 0):unit, "x:unit")).
ut("PP named        x:unit", pp(x@loc('x.ml', 0, 0, 0, 0, 0, 0):v:unit, "x:v:unit")).
ut("PP path         x:unit", pp(x@loc('x.ml', 0, 0, 0, 0, 0, 0):v:unit-->('V'='X'), "x:v:unit --> V=X")).

% x:v:bool --> V=X
ut("naming          x:bool", t_e_to_n_e1(x, loc('x.ml', 0, 0, 0, 0, 0, 0), bool, v, empty, x@loc('x.ml', 0, 0, 0, 0, 0, 0):v:bool)).
ut("Negative naming x:bool", \+ t_e_to_n_e1(x, loc('x.ml', 0, 0, 0, 0, 0, 0), bool, v, empty, x@loc('x.ml', 0, 0, 0, 0, 0, 0):x:bool)).
ut("path            x:bool", n_e_to_p_e1(x, loc('x.ml', 0, 0, 0, 0, 0, 0), v:bool, x@loc('x.ml', 0, 0, 0, 0, 0, 0):v:bool-->('V'='X'))).
ut("Negative path 1 x:bool", \+ n_e_to_p_e1(x, loc('x.ml', 0, 0, 0, 0, 0, 0), v:bool, x@loc('x.ml', 0, 0, 0, 0, 0, 0):v:bool-->true)).
ut("Negative path 2 x:bool", \+ n_e_to_p_e1(x, loc('x.ml', 0, 0, 0, 0, 0, 0), v:bool, x@loc('x.ml', 0, 0, 0, 0, 0, 0):v:bool-->'V'='X')).
ut("Negative path 3 x:bool", \+ n_e_to_p_e1(x, loc('x.ml', 0, 0, 0, 0, 0, 0), v:bool, x@loc('x.ml', 0, 0, 0, 0, 0, 0):v:bool-->'X'='V')).
ut("summ            x:bool", p_e_to_c1(x, loc('x.ml', 0, 0, 0, 0, 0, 0), v:bool, empty, true, 'V'='X', empty, [])).
ut("Negative summ   x:bool", \+ p_e_to_c1(x, loc('x.ml', 0, 0, 0, 0, 0, 0), v:bool, empty, true, 'V'='X', empty, [_])).

% x:v:int --> V=X
ut("naming          x:int", t_e_to_n_e1(x, loc('x.ml', 0, 0, 0, 0, 0, 0), int, v, empty, x@loc('x.ml', 0, 0, 0, 0, 0, 0):v:int)).
ut("Negative naming x:int", \+ t_e_to_n_e1(x, loc('x.ml', 0, 0, 0, 0, 0, 0), int, v, empty, x@loc('x.ml', 0, 0, 0, 0, 0, 0):x:int)).
ut("path            x:int", n_e_to_p_e1(x, loc('x.ml', 0, 0, 0, 0, 0, 0), v:int, x@loc('x.ml', 0, 0, 0, 0, 0, 0):v:int-->('V'='X'))).
ut("Negative path 1 x:int", \+ n_e_to_p_e1(x, loc('x.ml', 0, 0, 0, 0, 0, 0), v:int, x@loc('x.ml', 0, 0, 0, 0, 0, 0):v:int-->true)).
ut("Negative path 2 x:int", \+ n_e_to_p_e1(x, loc('x.ml', 0, 0, 0, 0, 0, 0), v:int, x@loc('x.ml', 0, 0, 0, 0, 0, 0):v:int-->'V'='X')).
ut("Negative path 3 x:int", \+ n_e_to_p_e1(x, loc('x.ml', 0, 0, 0, 0, 0, 0), v:int, x@loc('x.ml', 0, 0, 0, 0, 0, 0):v:int-->'X'='V')).
ut("summ            x:int", p_e_to_c1(x, loc('x.ml', 0, 0, 0, 0, 0, 0), v:int, empty, true, 'V'='X', empty, [])).
ut("Negative summ   x:int", \+ p_e_to_c1(x, loc('x.ml', 0, 0, 0, 0, 0, 0), v:int, empty, true, 'V'='X', empty, [_])).

% add:v:(a_add_v:int -> b_add:(ba_add_v:int -> bb_add_v:int)) --> 'add_int->int->int'(A_ADD_V, BA_ADD_V, BB_ADD_V)
ut("naming 1        add:(int->int->int)", t_e_to_n_e1(add, loc('x.ml', 0, 0, 0, 0, 0, 0), (int->int->int), add_v, node(add, add:(a_add:int -> b_add:(ba_add:int -> bb_add:int)), 0, empty, empty), add@loc('x.ml', 0, 0, 0, 0, 0, 0):add_v:(a_add_v:int->b_add:(ba_add_v:int->bb_add_v:int)))).
ut("naming 2        add:(int->int->int)", t_e_to_n_e1(add, loc('x.ml', 0, 0, 0, 0, 0, 0), (int->int->int), f, node(add, add:(a_add:int -> b_add:(ba_add:int -> bb_add:int)), 0, empty, empty), add@loc('x.ml', 0, 0, 0, 0, 0, 0):f:(a_f:int->b_add:(ba_f:int->bb_f:int)))).
ut("Negative naming add:(int->int->int)", \+ t_e_to_n_e1(add, loc('x.ml', 0, 0, 0, 0, 0, 0), (int->int->int), v, empty, add@loc('x.ml', 0, 0, 0, 0, 0, 0):add_v:(a_add_v:int->b_add:(ba_add_v:int->bb_add_v:int)))).
ut("path            add:(int->int->int)", n_e_to_p_e1(add, loc('x.ml', 0, 0, 0, 0, 0, 0), f:(a_f:int->b_add:(ba_f:int->bb_f:int)), add@loc('x.ml', 0, 0, 0, 0, 0, 0):f:(a_f:int->b_add:(ba_f:int->bb_f:int))-->'add_int->int->int'('A_F', 'BA_F', 'BB_F'))).
ut("Negative path 1 add:(int->int->int)", \+ n_e_to_p_e1(add, loc('x.ml', 0, 0, 0, 0, 0, 0), f:(a_f:int->b_add:(ba_f:int->bb_f:int)), add@loc('x.ml', 0, 0, 0, 0, 0, 0):f:(a_f:int->b_add:(ba_f:int->bb_f:int))-->true)).
ut("Negative path 2 add:(int->int->int)", \+ n_e_to_p_e1(add, loc('x.ml', 0, 0, 0, 0, 0, 0), f:(a_f:int->b_add:(ba_f:int->bb_f:int)), add@loc('x.ml', 0, 0, 0, 0, 0, 0):f:(a_f:int->b_add:(ba_f:int->bb_f:int))-->('F'='ADD'))).
ut("summ            add:(int->int->int)", p_e_to_c1(add, loc('x.ml', 0, 0, 0, 0, 0, 0), f:(a_f:int->b_add:(ba_f:int->bb_f:int)), empty, true, 'add_int->int->int'('A_F', 'BA_F', 'BB_F'), empty, [('ctx_add_int->int->int'('A_F', 'BA_F') :- 'ctx_f_int->int->int'('A_F', 'BA_F'))])).
ut("Negative summ   add:(int->int->int)", \+ p_e_to_c1(add, loc('x.ml', 0, 0, 0, 0, 0, 0), f:(a_f:int->b_add:(ba_f:int->bb_f:int)), empty, true, 'add_int->int->int'('A_F', 'BA_F', 'BB_F'), empty, [])).

ut("PP typed        add:(int->int->int)", pp(add@loc('x.ml', 0, 0, 0, 0, 0, 0):(int->int->int), "add:(int -> int -> int)")).
ut("PP named        add:(int->int->int)", pp(add@loc('x.ml', 0, 0, 0, 0, 0, 0):f:(a_f:int->b_add:(ba_f:int->bb_f:int)), "add:f:(a_f:int -> b_add:(ba_f:int -> bb_f:int))")).
ut("PP path         add:(int->int->int)", pp(add@loc('x.ml', 0, 0, 0, 0, 0, 0):f:(a_f:int->b_add:(ba_f:int->bb_f:int))-->'add_int->int->int'('A_F', 'BA_F', 'BB_F'), "add:f:(a_f:int -> b_add:(ba_f:int -> bb_f:int)) --> 'add_int->int->int'(A_F, BA_F, BB_F)")).

% comp:g:(a_g:int->b_comp:(ba_g:int->bb_g:bool)) --> 'comp_int->int->bool'(A_G, BA_G, BB_G)
ut("naming          comp:(int->int->bool)", t_e_to_n_e1(comp, l, (int->int->bool), g, node(comp, comp:(a_comp:int -> b_comp:(ba_comp:int -> bb_comp:bool)), 0, empty, empty), comp@l:g:(a_g:int->b_comp:(ba_g:int->bb_g:bool)))).
ut("Negative naming comp:(int->int->bool)", \+ t_e_to_n_e1(comp, l, (int->int->bool), v, empty, comp@l:comp_g:(a_g:int->b_comp:(ba_g:int->bb_g:bool)))).
ut("path            comp:(int->int->bool)", n_e_to_p_e1(comp, l, g:(a_g:int->b_comp:(ba_g:int->bb_g:bool)), comp@l:g:(a_g:int->b_comp:(ba_g:int->bb_g:bool))-->'comp_int->int->bool'('A_G', 'BA_G', 'BB_G'))).
ut("Negative path 1 comp:(int->int->bool)", \+ n_e_to_p_e1(comp, l, g:(a_g:int->b_comp:(ba_g:int->bb_g:bool)), comp@l:g:(a_g:int->b_comp:(ba_g:int->bb_g:bool))-->true)).
ut("Negative path 2 comp:(int->int->bool)", \+ n_e_to_p_e1(comp, l, g:(a_g:int->b_comp:(ba_g:int->bb_g:bool)), comp@l:g:(a_g:int->b_comp:(ba_g:int->bb_g:bool))-->('G'='COMP'))).
ut("Negative path 3 comp:(int->int->bool)", \+ n_e_to_p_e1(comp, l, g:(a_g:int->b_comp:(ba_g:int->bb_g:bool)), comp@l:g:(a_g:int->b_comp:(ba_g:int->bb_g:bool))-->'comp_int->int->bool'('A_G', 'BA_G'))).
ut("summ            comp:(int->int->bool)", p_e_to_c1(comp, l, g:(a_g:int->b_comp:(ba_g:int->bb_g:bool)), empty, true, 'comp_int->int->int'('A_G', 'BA_G', 'BB_G'), empty, [('ctx_comp_int->int->bool'('A_G', 'BA_G') :- 'ctx_g_int->int->bool'('A_G', 'BA_G'))])).
ut("Negative summ   comp:(int->int->bool)", \+ p_e_to_c1(comp, l, g:(a_g:int->b_comp:(ba_g:int->bb_g:bool)), empty, true, 'comp_int->int->int'('A_G', 'BA_G', 'BB_G'), empty, [])).



% **********************************************************************
% | c                                                      Constant
% | e e ... e                                              Application

ut("naming          1+2", t_e_to_n_e1(app((+)@l2:(int->int->int), [1@l3:int, 2@l4:int]), l1, int, v, empty,
                             app((+)@l2:plus_v:(a_plus_v:int -> b_plus_v:(ba_plus_v:int -> v:int)),
                                 [1@l3:a_plus_v:int,
                                  2@l4:ba_plus_v:int]
                                )@l1:v:int)).
ut("path            1+2", n_e_to_p_e1(app((+)@l2:plus_v:(a_plus_v:int -> b_plus_v:(ba_plus_v:int -> v:int)), [1@l3:a_plus_v:int, 2@l4:ba_plus_v:int]), l1, v:int,
                             app((+)@l2:plus_v:(a_plus_v:int -> b_plus_v:(ba_plus_v:int -> v:int)),
                                 [1@l3:a_plus_v:int --> ('A_PLUS_V'=1),
                                  2@l4:ba_plus_v:int --> ('BA_PLUS_V'=2)]
                                )@l1:v:int --> ('V'=1+2))).
ut("summ            1+2", p_e_to_c1(app((+)@l2:plus_v:(a_plus_v:int -> b_plus_v:(ba_plus_v:int -> v:int)),
                               [1@l3:a_plus_v:int --> ('A_PLUS_V'=1),
                                2@l4:ba_plus_v:int --> ('BA_PLUS_V'=2)]
                              ), l1, v:int, empty, true, ('V'=1+2), empty, [])).
ut("Negative summ   1+2", \+ p_e_to_c1(app((+)@l2:plus_v:(a_plus_v:int -> b_plus_v:(ba_plus_v:int -> v:int)),
                               [1@l3:a_plus_v:int --> ('A_PLUS_V'=1),
                                2@l4:ba_plus_v:int --> ('BA_PLUS_V'=2)]
                              ), l1, v:int, empty, true, ('V'=1+2), empty, [_])).

ut("PP typed        1+2", pp(app('+'@l2:(int -> int -> bool), [1@l3:int, 2@l4:int])@l1:bool, "(\n  (+):(int -> int -> bool)\n  1:int\n  2:int\n):bool")).

ut("naming           1>2", t_e_to_n_e1(app((>)@l2:(int->int->bool), [1@l3:int, 2@l4:int]), l1, bool, v, empty, app((>)@l2:gt_v:(a_gt_v:int -> b_gt_v:(ba_gt_v:int -> v:bool)), [1@l3:a_gt_v:int, 2@l4:ba_gt_v:int])@l1:v:bool)).
ut("path             1>2", n_e_to_p_e1(app((>)@l2:gt_v:(a_gt_v:int -> b_gt_v:(ba_gt_v:int -> v:bool)), [1@l3:a_gt_v:int, 2@l4:ba_gt_v:int]), l1, v:bool,
                                       app((>)@l2:gt_v:(a_gt_v:int -> b_gt_v:(ba_gt_v:int -> v:bool)),
                                           [1@l3:a_gt_v:int --> ('A_GT_V'=1),
                                            2@l4:ba_gt_v:int --> ('BA_GT_V'=2)]
                                          )@l1:v:bool --> (1>2 -> 'V'=1 ; 'V'=0))).
ut("Negative path 1  1>2", \+ n_e_to_p_e1(app((>)@l2:gt_v:(a_gt_v:int -> b_gt_v:(ba_gt_v:int -> v:bool)), [1@l3:a_gt_v:int, 2@l4:ba_gt_v:int]), l1, v:bool,
                                          app((>)@l2:gt_v:(a_gt_v:int -> b_gt_v:(ba_gt_v:int -> v:bool)),
                                              [1@l3:a_gt_v:int --> ('A_GT_V'=1),
                                               2@l4:ba_gt_v:int --> ('BA_GT_V'=2)]
                                             )@l1:v:bool --> (1>2))).
ut("summ             1>2", p_e_to_c1(app((>)@l2:gt_v:(a_gt_v:int -> b_gt_v:(ba_gt_v:int -> v:bool)),
                                         [1@l3:a_gt_v:int --> ('A_GT_V'=1),
                                          2@l4:ba_gt_v:int --> ('BA_GT_V'=2)]
                                        ), l1, v:bool, empty, true, (1>2 -> 'V'=1 ; 'V'=0), empty, [])).
ut("Negative summ 1  1>2", \+ p_e_to_c1(app((>)@l2:gt_v:(a_gt_v:int -> b_gt_v:(ba_gt_v:int -> v:bool)),
                                            [1@l3:a_gt_v:int --> ('A_GT_V'=1),
                                             2@l4:ba_gt_v:int --> ('BA_GT_V'=2)]
                                           ), l1, v:bool, empty, true, (1>2 -> 'V'=1 ; 'V'=0), empty, [_])).
ut("Negative summ 2  1>2", \+ p_e_to_c1(app((>)@l2:gt_v:(a_gt_v:int -> b_gt_v:(ba_gt_v:int -> v:bool)),
                                            [1@l3:a_gt_v:int --> ('A_GT_V'=1),
                                             2@l4:ba_gt_v:int --> ('BA_GT_V'=2)]
                                           ), l1, v:bool, empty, true, (1>2 -> 'V'=1 ; 'V'=0), empty, [_|_])).

ut("naming           (1+2)+3", t_e_to_n_e1(app((+)@l2:(int->int->int), [app((+)@l4:(int->int->int), [1@l5:int, 2@l6:int])@l3:int, 3@l7:int]), l1, int, v, empty,
                                           app((+)@l2:plus_v:(a_plus_v:int->b_plus_v:(ba_plus_v:int->v:int)),
                                               [app((+)@l4:plus_a_plus_v:(a_plus_a_plus_v:int->b_plus_a_plus_v:(ba_plus_a_plus_v:int->a_plus_v:int)),
                                                    [1@l5:a_plus_a_plus_v:int,
                                                     2@l6:ba_plus_a_plus_v:int]
                                                   )@l3:a_plus_v:int,
                                                3@l7:ba_plus_v:int]
                                              )@l1:v:int)).
ut("path             (1+2)+3", n_e_to_p_e1(app((+)@l2:plus_v:(a_plus_v:int->b_plus_v:(ba_plus_v:int->v:int)),
                                               [app((+)@l4:plus_a_plus_v:(a_plus_a_plus_v:int->b_plus_a_plus_v:(ba_plus_a_plus_v:int->a_plus_v:int)),
                                                    [1@l5:a_plus_a_plus_v:int,
                                                     2@l6:ba_plus_a_plus_v:int]
                                                   )@l3:a_plus_v:int,
                                                3@l7:ba_plus_v:int]
                                              ), l1, v:int,
                                           app((+)@l2:plus_v:(a_plus_v:int->b_plus_v:(ba_plus_v:int->v:int)),
                                               [app((+)@l4:plus_a_plus_v:(a_plus_a_plus_v:int->b_plus_a_plus_v:(ba_plus_a_plus_v:int->a_plus_v:int)),
                                                    [1@l5:a_plus_a_plus_v:int --> ('A_PLUS_A_PLUS_V'=1),
                                                     2@l6:ba_plus_a_plus_v:int --> ('BA_PLUS_A_PLUS_V'=2)]
                                                   )@l3:a_plus_v:int --> ('A_PLUS_V'=1+2),
                                                3@l7:ba_plus_v:int --> ('BA_PLUS_V'=3)]
                                              )@l1:v:int --> ('V'='A_PLUS_V'+3, 'A_PLUS_V'=1+2))).
ut("summ             (1+2)+3", p_e_to_c1(app((+)@l2:plus_v:(a_plus_v:int->b_plus_v:(ba_plus_v:int->v:int)),
                                             [app((+)@l4:plus_a_plus_v:(a_plus_a_plus_v:int->b_plus_a_plus_v:(ba_plus_a_plus_v:int->a_plus_v:int)),
                                                  [1@l5:a_plus_a_plus_v:int --> ('A_PLUS_A_PLUS_V'=1),
                                                   2@l6:ba_plus_a_plus_v:int --> ('BA_PLUS_A_PLUS_V'=2)]
                                                 )@l3:a_plus_v:int --> ('A_PLUS_V'=1+2),
                                              3@l7:ba_plus_v:int --> ('BA_PLUS_V'=3)]
                                            ), l1, v:int, empty, true, ('V'='A_PLUS_V'+3, 'A_PLUS_V'=1+2), empty, [])).
ut("Negative summ 1  (1+2)+3", \+ p_e_to_c1(app((+)@l2:plus_v:(a_plus_v:int->b_plus_v:(ba_plus_v:int->v:int)),
                                                [app((+)@l4:plus_a_plus_v:(a_plus_a_plus_v:int->b_plus_a_plus_v:(ba_plus_a_plus_v:int->a_plus_v:int)),
                                                     [1@l5:a_plus_a_plus_v:int --> ('A_PLUS_A_PLUS_V'=1),
                                                      2@l6:ba_plus_a_plus_v:int --> ('BA_PLUS_A_PLUS_V'=2)]
                                                    )@l3:a_plus_v:int --> ('A_PLUS_V'=1+2),
                                                 3@l7:ba_plus_v:int --> ('BA_PLUS_V'=3)]
                                               ), l1, v:int, empty, true, ('V'='A_PLUS_V'+3, 'A_PLUS_V'=1+2), empty, [_])).
ut("Negative summ 2  (1+2)+3", \+ p_e_to_c1(app((+)@l2:plus_v:(a_plus_v:int->b_plus_v:(ba_plus_v:int->v:int)),
                                                [app((+)@l4:plus_a_plus_v:(a_plus_a_plus_v:int->b_plus_a_plus_v:(ba_plus_a_plus_v:int->a_plus_v:int)),
                                                     [1@l5:a_plus_a_plus_v:int --> ('A_PLUS_A_PLUS_V'=1),
                                                      2@l6:ba_plus_a_plus_v:int --> ('BA_PLUS_A_PLUS_V'=2)]
                                                    )@l3:a_plus_v:int --> ('A_PLUS_V'=1+2),
                                                 3@l7:ba_plus_v:int --> ('BA_PLUS_V'=3)]
                                               ), l1, v:int, empty, true, ('V'='A_PLUS_V'+3, 'A_PLUS_V'=1+2), empty, [_|_])).

ut("PP path          (1+2)+3", pp(app((+)@l2:plus_v:(a_plus_v:int->b_plus_v:(ba_plus_v:int->v:int)),
                                      [app((+)@l4:plus_a_plus_v:(a_plus_a_plus_v:int->b_plus_a_plus_v:(ba_plus_a_plus_v:int->a_plus_v:int)),
                                           [1@l5:a_plus_a_plus_v:int --> ('A_PLUS_A_PLUS_V'=1),
                                            2@l6:ba_plus_a_plus_v:int --> ('BA_PLUS_A_PLUS_V'=2)]
                                          )@l3:a_plus_v:int --> ('A_PLUS_V'=1+2),
                                       3@l7:ba_plus_v:int --> ('BA_PLUS_V'=3)]
                                     )@l1:v:int --> ('V'='A_PLUS_V'+3, 'A_PLUS_V'=1+2),
                                  "(\n  (+):plus_v:(a_plus_v:int -> b_plus_v:(ba_plus_v:int -> v:int))\n  (\n    (+):plus_a_plus_v:(a_plus_a_plus_v:int -> b_plus_a_plus_v:(ba_plus_a_plus_v:int -> a_plus_v:int))\n    1:a_plus_a_plus_v:int --> A_PLUS_A_PLUS_V=1\n    2:ba_plus_a_plus_v:int --> BA_PLUS_A_PLUS_V=2\n  ):a_plus_v:int --> A_PLUS_V=1+2\n  3:ba_plus_v:int --> BA_PLUS_V=3\n):v:int --> V=A_PLUS_V+3, A_PLUS_V=1+2")).

ut("naming           (1+2)=3", t_e_to_n_e1(app((=)@l2:(int->int->bool),
                                      [app((+)@l4:(int->int->int),
                                           [1@l5:int,
                                            2@l6:int]
                                          )@l3:int,
                                       3@l7:int]
                                     ), l1, bool, v, empty,
                                  app((=)@l2:eq_v:(a_eq_v:int->b_eq_v:(ba_eq_v:int->v:bool)),
                                      [app((+)@l4:plus_a_eq_v:(a_plus_a_eq_v:int -> b_plus_a_eq_v:(ba_plus_a_eq_v:int -> a_eq_v:int)),
                                           [1@l5:a_plus_a_eq_v:int,
                                            2@l6:ba_plus_a_eq_v:int]
                                          )@l3:a_eq_v:int,
                                       3@l7:ba_eq_v:int]
                                     )@l1:v:bool)).
ut("path             (1+2)=3", n_e_to_p_e1(app((=)@l2:eq_v:(a_eq_v:int->b_eq_v:(ba_eq_v:int->v:bool)),
                                      [app((+)@l4:plus_a_eq_v:(a_plus_a_eq_v:int -> b_plus_a_eq_v:(ba_plus_a_eq_v:int -> a_eq_v:int)),
                                          [1@l5:a_plus_a_eq_v:int,
                                           2@l6:ba_plus_a_eq_v:int]
                                         )@l3:a_eq_v:int,
                                      3@l7:ba_eq_v:int]
                                  ), l1, v:bool,
                                  app((=)@l2:eq_v:(a_eq_v:int->b_eq_v:(ba_eq_v:int->v:bool)),
                                      [app((+)@l4:plus_a_eq_v:(a_plus_a_eq_v:int -> b_plus_a_eq_v:(ba_plus_a_eq_v:int -> a_eq_v:int)),
                                           [1@l5:a_plus_a_eq_v:int --> ('A_PLUS_A_EQ_V'=1),
                                            2@l6:ba_plus_a_eq_v:int --> ('BA_PLUS_A_EQ_V'=2)]
                                          )@l3:a_eq_v:int --> ('A_EQ_V'=1+2),
                                       3@l7:ba_eq_v:int --> ('BA_EQ_V'=3)]
                                     )@l1:v:bool --> (('A_EQ_V'=3 -> 'V'=1 ; 'V'=0), 'A_EQ_V'=1+2))).
ut("summ             (1+2)=3", p_e_to_c1(app((=)@l2:eq_v:(a_eq_v:int->b_eq_v:(ba_eq_v:int->v:bool)),
                                      [app((+)@l4:plus_a_eq_v:(a_plus_a_eq_v:int -> b_plus_a_eq_v:(ba_plus_a_eq_v:int -> a_eq_v:int)),
                                           [1@l5:a_plus_a_eq_v:int --> ('A_PLUS_A_EQ_V'=1),
                                            2@l6:ba_plus_a_eq_v:int --> ('BA_PLUS_A_EQ_V'=2)]
                                          )@l3:a_eq_v:int --> ('A_EQ_V'=1+2),
                                       3@l7:ba_eq_v:int --> ('BA_EQ_V'=3)]
                                     ), l1, v:bool, empty, true, (('A_EQ_V'=3 -> 'V'=1 ; 'V'=0), 'A_EQ_V'=1+2), empty, [])).
ut("Negative summ 1  (1+2)=3", \+ p_e_to_c1(app((=)@l2:eq_v:(a_eq_v:int->b_eq_v:(ba_eq_v:int->v:bool)),
                                      [app((+)@l4:plus_a_eq_v:(a_plus_a_eq_v:int -> b_plus_a_eq_v:(ba_plus_a_eq_v:int -> a_eq_v:int)),
                                           [1@l5:a_plus_a_eq_v:int --> ('A_PLUS_A_EQ_V'=1),
                                            2@l6:ba_plus_a_eq_v:int --> ('BA_PLUS_A_EQ_V'=2)]
                                          )@l3:a_eq_v:int --> ('A_EQ_V'=1+2),
                                       3@l7:ba_eq_v:int --> ('BA_EQ_V'=3)]
                                     ), l1, v:bool, empty, true, (('A_EQ_V'=3 -> 'V'=1 ; 'V'=0), 'A_EQ_V'=1+2), empty, [_])).
ut("Negative summ 2  (1+2)=3", \+ p_e_to_c1(app((=)@l2:eq_v:(a_eq_v:int->b_eq_v:(ba_eq_v:int->v:bool)),
                                      [app((+)@l4:plus_a_eq_v:(a_plus_a_eq_v:int -> b_plus_a_eq_v:(ba_plus_a_eq_v:int -> a_eq_v:int)),
                                           [1@l5:a_plus_a_eq_v:int --> ('A_PLUS_A_EQ_V'=1),
                                            2@l6:ba_plus_a_eq_v:int --> ('BA_PLUS_A_EQ_V'=2)]
                                          )@l3:a_eq_v:int --> ('A_EQ_V'=1+2),
                                       3@l7:ba_eq_v:int --> ('BA_EQ_V'=3)]
                                     ), l1, v:bool, empty, true, (('A_EQ_V'=3 -> 'V'=1 ; 'V'=0), 'A_EQ_V'=1+2), empty, [_|_])).

ut("naming          (+) 1", t_e_to_n_e1(app((+)@l2:(int->int->int), [1@l3:int]), l1, (int->int), v, empty, app((+)@l2:plus_v:(a_plus_v:int -> v:(ba_plus_v:int -> bb_plus_v:int)), [1@l3:a_plus_v:int])@l1:v:(ba_plus_v:int -> bb_plus_v:int))).
ut("path            (+) 1", n_e_to_p_e1(app((+)@l2:plus_v:(a_plus_v:int -> v:(ba_plus_v:int -> bb_plus_v:int)),
                                   [1@l3:a_plus_v:int]
                                  ), l1, v:(ba_plus_v:int -> bb_plus_v:int),
                               app((+)@l2:plus_v:(a_plus_v:int -> v:(ba_plus_v:int -> bb_plus_v:int)),
                                   [1@l3:a_plus_v:int --> ('A_PLUS_V'=1)]
                                  )@l1:v:(ba_plus_v:int -> bb_plus_v:int)-->('BB_PLUS_V'=1+'BA_PLUS_V'))).
ut("summ            (+) 1", p_e_to_c1(app((+)@l2:plus_v:(a_plus_v:int -> v:(ba_plus_v:int -> bb_plus_v:int)),
                                 [1@l3:a_plus_v:int --> ('A_PLUS_V'=1)]
                                ), l1, v:(ba_plus_v:int -> bb_plus_v:int), empty, true, 'BB_PLUS_V'=1+'BA_PLUS_V', empty, [])).
ut("Negative summ 1 (+) 1", \+ p_e_to_c1(app((+)@l2:plus_v:(a_plus_v:int -> v:(ba_plus_v:int -> bb_plus_v:int)),
                                 [1@l3:a_plus_v:int --> ('A_PLUS_V'=1)]
                                ), l1, v:(ba_plus_v:int -> bb_plus_v:int), empty, true, 'BB_PLUS_V'=1+'BA_PLUS_V', empty, [_])).
ut("Negative summ 2 (+) 1", \+ p_e_to_c1(app((+)@l2:plus_v:(a_plus_v:int -> v:(ba_plus_v:int -> bb_plus_v:int)),
                                 [1@l3:a_plus_v:int --> ('A_PLUS_V'=1)]
                                ), l1, v:(ba_plus_v:int -> bb_plus_v:int), empty, true, 'BB_PLUS_V'=1+'BA_PLUS_V', empty, [_|_])).

ut("PP path         (+) 1", pp(app((+)@l2:plus_v:(a_plus_v:int -> v:(ba_plus_v:int -> bb_plus_v:int)),
                                   [1@l3:a_plus_v:int]
                                  )@l1:v:(ba_plus_v:int -> bb_plus_v:int)-->('BB_PLUS_V'=1+'BA_PLUS_V'),
                        "(\n  (+):plus_v:(a_plus_v:int -> v:(ba_plus_v:int -> bb_plus_v:int))\n  1:a_plus_v:int\n):v:(ba_plus_v:int -> bb_plus_v:int) --> BB_PLUS_V=1+BA_PLUS_V")).

ut("naming          (<) true", t_e_to_n_e1(app((<)@l2:(bool->bool->bool), [true@l3:bool]), l1, (bool->bool), v, empty, app((<)@l2:lt_v:(a_lt_v:bool->v:(ba_lt_v:bool->bb_lt_v:bool)), [true@l3:a_lt_v:bool])@l1:v:(ba_lt_v:bool -> bb_lt_v:bool))).
ut("path            (<) true", n_e_to_p_e1(app((<)@l2:lt_v:(a_lt_v:bool->v:(ba_lt_v:bool->bb_lt_v:bool)), [true@l3:a_lt_v:bool]), l1, v:(ba_lt_v:bool -> bb_lt_v:bool),
                                  app((<)@l2:lt_v:(a_lt_v:bool->v:(ba_lt_v:bool->bb_lt_v:bool)),
                                      [true@l3:a_lt_v:bool --> ('A_LT_V'=1)]
                                     )@l1:v:(ba_lt_v:bool -> bb_lt_v:bool)-->(1<'BA_LT_V' -> 'BB_LT_V'=1 ; 'BB_LT_V'=0))).
ut("summ            (<) true", p_e_to_c1(app((<)@l2:lt_v:(a_lt_v:bool->v:(ba_lt_v:bool->bb_lt_v:bool)),
                                    [true@l3:a_lt_v:bool --> ('A_LT_V'=1)]
                                   ), l1, v:(ba_lt_v:bool -> bb_lt_v:bool), empty, true, (1<'BA_LT_V' -> 'BB_LT_V'=1 ; 'BB_LT_V'=0), empty, [])).
ut("Negative summ 1 (<) true", \+ p_e_to_c1(app((<)@l2:lt_v:(a_lt_v:bool->v:(ba_lt_v:bool->bb_lt_v:bool)),
                                    [true@l3:a_lt_v:bool --> ('A_LT_V'=1)]
                                   ), l1, v:(ba_lt_v:bool -> bb_lt_v:bool), empty, true, (1<'BA_LT_V' -> 'BB_LT_V'=1 ; 'BB_LT_V'=0), empty, [_])).
ut("Negative summ 2 (<) true", \+ p_e_to_c1(app((<)@l2:lt_v:(a_lt_v:bool->v:(ba_lt_v:bool->bb_lt_v:bool)),
                                    [true@l3:a_lt_v:bool --> ('A_LT_V'=1)]
                                   ), l1, v:(ba_lt_v:bool -> bb_lt_v:bool), empty, true, (1<'BA_LT_V' -> 'BB_LT_V'=1 ; 'BB_LT_V'=0), empty, [_|_])).

ut("PP path  (<) 1", pp(app((<)@l2:lt_v:(a_lt_v:bool->v:(ba_lt_v:bool->bb_lt_v:bool)),
                            [true@l3:a_lt_v:bool --> ('A_LT_V'=1)]
                           )@l1:v:(ba_lt_v:bool -> bb_lt_v:bool) --> (1<'BA_LT_V'),
                        "(\n  (<):lt_v:(a_lt_v:bool -> v:(ba_lt_v:bool -> bb_lt_v:bool))\n  true:a_lt_v:bool --> A_LT_V=1\n):v:(ba_lt_v:bool -> bb_lt_v:bool) --> 1<BA_LT_V")).

ut("naming false && true", t_e_to_n_e1(app('&&'@l:(bool -> bool -> bool),
                                           [false@l:bool,
                                            true@l:bool]
                                          ), l, bool, v, empty,
                                       app(&& @l:and_v:(a_and_v:bool->b_and_v:(ba_and_v:bool->v:bool)),
                                           [false@l:a_and_v:bool,
                                            true@l:ba_and_v:bool]
                                          )@l:v:bool)).
ut("path   false && true", n_e_to_p_e1(app(&& @l:and_v:(a_and_v:bool->b_and_v:(ba_and_v:bool->v:bool)),
                                           [false@l:a_and_v:bool,
                                            true@l:ba_and_v:bool]
                                          ), l, v:bool,
                                       app(&& @l:and_v:(a_and_v:bool->b_and_v:(ba_and_v:bool->v:bool)),
                                           [false@l:a_and_v:bool --> ('A_AND_V'=0),
                                            true@l:ba_and_v:bool --> ('BA_AND_V'=1)]
                                          )@l:v:bool --> (('A_AND_V'=1, 'BA_AND_V'=1 -> 'V'=1 ; 'V'=0), 'BA_AND_V'=1, 'A_AND_V'=0))).
ut("summ   false && true", p_e_to_c1(app(&& @l:and_v:(a_and_v:bool->b_and_v:(ba_and_v:bool->v:bool)),
                                         [false@l:a_and_v:bool --> ('A_AND_V'=0),
                                          true@l:ba_and_v:bool --> ('BA_AND_V'=1)]
                                        ), l, v:bool, empty, true, (('A_AND_V'=1, 'BA_AND_V'=1 -> 'V'=1 ; 'V'=0), 'BA_AND_V'=1, 'A_AND_V'=0),
                                     empty, [])).

ut("path   (&&) false", n_e_to_p_e1(app(&& @l:and_v:(a_and_v:bool->v:(ba_and_v:bool->bb_and_v:bool)),
                                        [false@l:a_and_v:bool]
                                       ), l, v:(ba_and_v:bool->bb_and_v:bool),
                                    app(&& @l:and_v:(a_and_v:bool->v:(ba_and_v:bool->bb_and_v:bool)),
                                        [false@l:a_and_v:bool-->('A_AND_V'=0)]
                                       )@l:v:(ba_and_v:bool->bb_and_v:bool)-->(('A_AND_V'=1,'BA_AND_V'=1->'BB_AND_V'=1;'BB_AND_V'=0),'A_AND_V'=0))).
ut("summ   (&&) false", p_e_to_c1(app(&& @l:and_v:(a_and_v:bool->v:(ba_and_v:bool->bb_and_v:bool)),
                                      [false@l:a_and_v:bool-->('A_AND_V'=0)]
                                     ), l, v:(ba_and_v:bool->bb_and_v:bool), empty, true, (('A_AND_V'=1,'BA_AND_V'=1->'BB_AND_V'=1;'BB_AND_V'=0),'A_AND_V'=0),
                                  empty, [])).

ut("path   false || true", n_e_to_p_e1(app('||' @l:and_v:(a_or_v:bool->b_or_v:(ba_or_v:bool->v:bool)),
                                           [false@l:a_or_v:bool,
                                            true@l:ba_or_v:bool]
                                          ), l, v:bool,
                                       app('||' @l:and_v:(a_or_v:bool->b_or_v:(ba_or_v:bool->v:bool)),
                                           [false@l:a_or_v:bool --> ('A_OR_V'=0),
                                            true@l:ba_or_v:bool --> ('BA_OR_V'=1)]
                                          )@l:v:bool --> ((('A_OR_V'=1 ; 'BA_OR_V'=1) -> 'V'=1 ; 'V'=0), 'BA_OR_V'=1, 'A_OR_V'=0))).
ut("summ   false || true", p_e_to_c1(app('||' @l:and_v:(a_or_v:bool->b_or_v:(ba_or_v:bool->v:bool)),
                                         [false@l:a_or_v:bool --> ('A_OR_V'=0),
                                          true@l:ba_or_v:bool --> ('BA_OR_V'=1)]
                                        ), l, v:bool, empty, true, ((('A_OR_V'=1 ; 'BA_OR_V'=1) -> 'V'=1 ; 'V'=0), 'BA_OR_V'=1, 'A_OR_V'=0),
                                     empty, [])).

ut("path   (||) false", n_e_to_p_e1(app('||'@l:or_v:(a_or_v:bool->v:(ba_or_v:bool->bb_or_v:bool)),
                                        [false@l:a_or_v:bool]
                                       ), l, v:(ba_or_v:bool->bb_or_v:bool),
                                    app('||'@l:or_v:(a_or_v:bool->v:(ba_or_v:bool->bb_or_v:bool)),
                                        [false@l:a_or_v:bool-->('A_OR_V'=0)]
                                       )@l:v:(ba_or_v:bool->bb_or_v:bool)-->((('A_OR_V'=1 ; 'BA_OR_V'=1)->'BB_OR_V'=1;'BB_OR_V'=0),'A_OR_V'=0))).

ut("naming not true", t_e_to_n_e1(app(not@l:(bool -> bool),
                                      [true@l:bool]
                                     ), l, bool, v, empty,
                                  app(not@l:not_v:(a_not_v:bool->v:bool),
                                      [true@l:a_not_v:bool]
                                     )@l:v:bool)).
ut("path   not true", n_e_to_p_e1(app(not@l:not_v:(a_not_v:bool->v:bool),
                                      [true@l:a_not_v:bool]
                                     ), l, v:bool,
                                  app(not@l:not_v:(a_not_v:bool->v:bool),
                                      [true@l:a_not_v:bool --> ('A_NOT_V'=1)]
                                     )@l:v:bool --> (('A_NOT_V'=0 -> 'V'=1 ; 'V'=0), 'A_NOT_V'=1))).



% **********************************************************************
% | x                                                      Identifier
% | c                                                      Constant
% | e e ... e                                              Application

ut("naming   full app (add 1 2):int", t_e_to_n_e1(app(add@l2:(int->int->int), [1@l3:int, 2@l4:int]), l1, int, v,
                                                  node(add, add:(a_add:int -> b_add:(ba_add:int -> bb_add:int)), 0, empty, empty),
                                                  app(add@l2:add_v:(a_add_v:int->b_add:(ba_add_v:int->v:int)),
                                                      [1@l3:a_add_v:int,
                                                       2@l4:ba_add_v:int]
                                                     )@l1:v:int)).
ut("path     full app (add 1 2):int", (   t_e_to_n_e1(app(add@l2:(int->int->int), [1@l3:int, 2@l4:int]), l1, int, v,
                                                      node(add, add:(a_add:int -> b_add:(ba_add:int -> bb_add:int)), 0, empty, empty),
                                                      En@L:N),
                                          n_e_to_p_e1(En, L, N,
                                                      app(add@l2:add_v:(a_add_v:int->b_add:(ba_add_v:int->v:int)),
                                                          [1@l3:a_add_v:int --> ('A_ADD_V'=1),
                                                           2@l4:ba_add_v:int --> ('BA_ADD_V'=2)]
                                                         )@l1:v:int --> 'add_int->int->int'(1, 2, 'V')))).
ut("procs    full app (add 1 2):int", (   t_e_to_n_e1(app(add@l2:(int->int->int), [1@l3:int, 2@l4:int]), l1, int, v,
                                                      node(add, add:(a_add:int -> b_add:(ba_add:int -> bb_add:int)), 0, empty, empty),
                                                      En@L:N),
                                          n_e_to_p_e1(En, L, N, Ep@L:N-->K),
                                          p_e_to_p_d1(Ep, L, N, true, K, node(add,(true,(+)@l:(int->int->int)),0,empty,empty), node(add,(true,(+)@l:(int->int->int)),0,empty,empty)))).
ut("summ     full app (add 1 2):int", (   t_e_to_n_e1(app(add@l2:(int->int->int), [1@l3:int, 2@l4:int]), l1, int, v,
                                                      node(add, add:(a_add:int -> b_add:(ba_add:int -> bb_add:int)), 0, empty, empty),
                                                      En@L:N),
                                          n_e_to_p_e1(En, L, N, Ep@L:N-->K),
                                          p_e_to_c1(Ep, L, N, empty, true, K, node(add,(true,(+)@l:(int->int->int)),0,empty,empty),
                                                    [('ctx_add_int->int->int'('A_ADD_V', 'BA_ADD_V') :- ('BA_ADD_V'=2, 'A_ADD_V'=1)),
                                                     ('add_int->int->int'('A_ADD', 'BA_ADD', 'BB_ADD') :- ('BB_ADD'='A_ADD'+'BA_ADD', 'ctx_add_int->int->int'('A_ADD', 'BA_ADD')))]))).

ut("Negative WF   partial app (add 1):(int->int)", \+ wf_t_e(app(add@l2:(int->int->int), [1@l3:int]), l1, int)).
ut("naming        partial app (add 1):(int->int)", t_e_to_n_e1(app(add@l2:(int->int->int),
                                                                   [1@l3:int]
                                                                  ), l1, (int->int), v,
                                                               node(add, add:(a_add:int -> b_add:(ba_add:int -> bb_add:int)), 0, empty, empty),
                                                               app(add@l2:add_v:(a_add_v:int->v:(ba_add_v:int->bb_add_v:int)),
                                                                   [1@l3:a_add_v:int]
                                                                  )@l1:v:(ba_add_v:int->bb_add_v:int))).
ut("path          partial app (add 1):(int->int)", (   t_e_to_n_e1(app(add@l2:(int->int->int),
                                                                       [1@l3:int]
                                                                      ), l1, (int->int), v,
                                                                   node(add, add:(a_add:int -> b_add:(ba_add:int -> bb_add:int)), 0, empty, empty), En@L:N),
                                                       n_e_to_p_e1(En, L, N,
                                                                   app(add@l2:add_v:(a_add_v:int->v:(ba_add_v:int->bb_add_v:int)),
                                                                       [1@l3:a_add_v:int --> ('A_ADD_V'=1)]
                                                                      )@l1:v:(ba_add_v:int->bb_add_v:int) --> 'add_int->int->int'(1, 'BA_ADD_V', 'BB_ADD_V')))).
ut("summ          partial app (add 1):(int->int)", (   t_e_to_n_e1(app(add@l2:(int->int->int),
                                                                       [1@l3:int]
                                                                      ), l1, (int->int), v,
                                                                   node(add, add:(a_add:int -> b_add:(ba_add:int -> bb_add:int)), 0, empty, empty), En@L:N),
                                                       n_e_to_p_e1(En, L, N, Ep@L:N-->K),
                                                       p_e_to_c1(Ep, L, N, empty, true, K, node(add,(true,(+)@l:(int->int->int)),0,empty,empty),
                                                                 [('ctx_add_int->int->int'('A_ADD_V', 'BA_ADD_V') :- 'A_ADD_V'=1),
                                                                  ('add_int->int->int'('A_ADD', 'BA_ADD', 'BB_ADD') :- ('BB_ADD'='A_ADD'+'BA_ADD', 'ctx_add_int->int->int'('A_ADD', 'BA_ADD')))]))).

ut("naming   nested app (incr (incr 1)):int", t_e_to_n_e1(app(incr@l:(int->int),
                                                              [app(incr@l:(int->int),
                                                                   [1@l:int]
                                                                  )@l:int]
                                                             ), l, int, r,
                                                          node(incr,incr:(ba_plus_incr:int -> bb_plus_incr:int),0,empty,empty),
                                                          app(incr@l:incr_r:(a_incr_r:int->r:int),
                                                              [app(incr@l:incr_a_incr_r:(a_incr_a_incr_r:int->a_incr_r:int),
                                                                   [1@l:a_incr_a_incr_r:int]
                                                                  )@l:a_incr_r:int]
                                                             )@l:r:int)).
ut("path     nested app (incr (incr 1)):int", (   t_e_to_n_e1(app(incr@l:(int->int),
                                                                  [app(incr@l:(int->int),
                                                                       [1@l:int]
                                                                      )@l:int]
                                                                 ), l, int, r,
                                                              node(incr,incr:(ba_plus_incr:int -> bb_plus_incr:int),0,empty,empty), En@L:N),
                                                  n_e_to_p_e1(En, L, N,
                                                              app(incr@l:incr_r:(a_incr_r:int->r:int),
                                                                  [app(incr@l:incr_a_incr_r:(a_incr_a_incr_r:int->a_incr_r:int),
                                                                       [1@l:a_incr_a_incr_r:int --> ('A_INCR_A_INCR_R'=1)]
                                                                      )@l:a_incr_r:int --> 'incr_int->int'(1, 'A_INCR_R')]
                                                                 )@l:r:int -->  ('incr_int->int'('A_INCR_R', 'R'), 'incr_int->int'(1, 'A_INCR_R'))))).
ut("summ     nested app (incr (incr 1)):int", (   t_e_to_n_e1(app(incr@l:(int->int),
                                                                  [app(incr@l:(int->int),
                                                                       [1@l:int]
                                                                      )@l:int]
                                                                 ), l, int, r,
                                                              node(incr,incr:(ba_plus_incr:int -> bb_plus_incr:int),0,empty,empty), En@L:N),
                                                  n_e_to_p_e1(En, L, N, Ep@L:N-->K),
                                                  p_e_to_c1(Ep, L, N, empty, true, K, node(incr,(true,app((+)@l2:(int->int->int),[1@l3:int])@l1:(int->int)),0,empty,empty),
                                                            [('ctx_incr_int->int'('A_INCR_A_INCR_R'):-'A_INCR_A_INCR_R'=1),
                                                             ('ctx_incr_int->int'('A_INCR_R'):-'incr_int->int'(1,'A_INCR_R')),
                                                             ('incr_int->int'('BA_PLUS_INCR', 'BB_PLUS_INCR') :- ('BB_PLUS_INCR'=1+'BA_PLUS_INCR', 'ctx_incr_int->int'('BA_PLUS_INCR')))]))).

ut("naming   nested app (incr (1+2)):int", t_e_to_n_e1(app(incr@l:(int->int),
                                                           [app((+)@l:(int->int->int),
                                                                [1@l:int,
                                                                 2@l:int]
                                                               )@l:int]
                                                          ), l, int, r,
                                                       node(incr,incr:(ba_plus_incr:int -> bb_plus_incr:int),0,empty,empty),
                                                       app(incr@l:incr_r:(a_incr_r:int->r:int),
                                                           [app((+)@l:plus_a_incr_r:(a_plus_a_incr_r:int->b_plus_a_incr_r:(ba_plus_a_incr_r:int->a_incr_r:int)),
                                                                [1@l:a_plus_a_incr_r:int,
                                                                 2@l:ba_plus_a_incr_r:int]
                                                               )@l:a_incr_r:int]
                                                          )@l:r:int)).
ut("path     nested app (incr (1+2)):int", (   t_e_to_n_e1(app(incr@l:(int->int),
                                                               [app((+)@l:(int->int->int),
                                                                    [1@l:int,
                                                                     2@l:int]
                                                                   )@l:int]
                                                              ), l, int, r,
                                                           node(incr,incr:(ba_plus_incr:int -> bb_plus_incr:int),0,empty,empty), En@L:N),
                                               n_e_to_p_e1(En, L, N,
                                                           app(incr@l:incr_r:(a_incr_r:int->r:int),
                                                               [app((+)@l:plus_a_incr_r:(a_plus_a_incr_r:int->b_plus_a_incr_r:(ba_plus_a_incr_r:int->a_incr_r:int)),
                                                                    [1@l:a_plus_a_incr_r:int --> ('A_PLUS_A_INCR_R'=1),
                                                                     2@l:ba_plus_a_incr_r:int --> ('BA_PLUS_A_INCR_R'=2)]
                                                                   )@l:a_incr_r:int --> ('A_INCR_R'=1+2)]
                                                              )@l:r:int --> ('incr_int->int'('A_INCR_R', 'R'), 'A_INCR_R'=1+2)))).
ut("summ     nested app (incr (1+2)):int", (   t_e_to_n_e1(app(incr@l:(int->int),
                                                               [app((+)@l:(int->int->int),
                                                                    [1@l:int,
                                                                     2@l:int]
                                                                   )@l:int]
                                                              ), l, int, r,
                                                           node(incr,incr:(ba_plus_incr:int -> bb_plus_incr:int),0,empty,empty), En@L:N),
                                               n_e_to_p_e1(En, L, N, Ep@L:N-->K),
                                               p_e_to_c1(Ep, L, N, empty, true, K, node(incr,(true,app((+)@l2:(int->int->int),[1@l3:int])@l1:(int->int)),0,empty,empty),
                                                         [('ctx_incr_int->int'('A_INCR_R') :- 'A_INCR_R'=1+2),
                                                          ('incr_int->int'('BA_PLUS_INCR', 'BB_PLUS_INCR') :- ('BB_PLUS_INCR'=1+'BA_PLUS_INCR', 'ctx_incr_int->int'('BA_PLUS_INCR')))]))).

ut("path     nested app true && comp 1 2", n_e_to_p_e1(app(&& @l:and_v:(a_and_v:bool->b_and_v:(ba_and_v:bool->v:bool)),
                                                           [true@l:a_and_v:bool,
                                                            app(comp@l:comp_ba_and_v:(a_comp_ba_and_v:int->b_comp:(ba_comp_ba_and_v:int->ba_and_v:bool)),
                                                                [1@l:a_comp_ba_and_v:int,
                                                                 2@l:ba_comp_ba_and_v:int]
                                                               )@l:ba_and_v:bool]
                                                          ), l, v:bool,
                                                       app(&& @l:and_v:(a_and_v:bool->b_and_v:(ba_and_v:bool->v:bool)),
                                                           [true@l:a_and_v:bool --> ('A_AND_V'=1),
                                                            app(comp@l:comp_ba_and_v:(a_comp_ba_and_v:int->b_comp:(ba_comp_ba_and_v:int->ba_and_v:bool)),
                                                                [1@l:a_comp_ba_and_v:int --> ('A_COMP_BA_AND_V'=1),
                                                                 2@l:ba_comp_ba_and_v:int --> ('BA_COMP_BA_AND_V'=2)]
                                                               )@l:ba_and_v:bool --> ('comp_int->int->bool'(1, 2, 'BA_AND_V'))]
                                                          )@l:v:bool --> (('A_AND_V'=1, 'BA_AND_V'=1 -> 'V'=1 ; 'V'=0), 'comp_int->int->bool'(1, 2, 'BA_AND_V'), 'A_AND_V'=1))).


% **********************************************************************
% | c                                                      Constant
% | e e ... e                                              Application
% | if e then e else e                                     If

ut("naming   if true then 1 else 0", t_e_to_n_e1(ite(true@l2:bool, 1@l2:int, 0@l3:int), l1, int, res, empty, ite(true@l2:c_res:bool, 1@l2:res:int, 0@l3:res:int)@l1:res:int)).
ut("path     if true then 1 else 0", n_e_to_p_e1(ite(true@l2:c_res:bool, 1@l2:res:int, 0@l3:res:int), l1, res:int,
                                                 ite(true@l2:c_res:bool --> ('C_RES'=1),
                                                     1@l2:res:int --> ('RES'=1),
                                                     0@l3:res:int --> ('RES'=0)
                                                    )@l1:res:int --> (('C_RES'=1 -> 'RES'=1 ; 'RES'=0), 'C_RES'=1))).
ut("summ     if true then 1 else 0", p_e_to_c1(ite(true@l2:c_res:bool --> ('C_RES'=1),
                                                   1@l2:res:int --> ('RES'=1),
                                                   0@l3:res:int --> ('RES'=0)
                                                  ), l1, res:int, empty, true, (('C_RES'=1 -> 'RES'=1 ; 'RES'=0), 'C_RES'=1), empty, [])).

ut("PP path  if true then 1 else 0", pp(ite(true@l2:c_res:bool --> ('C_RES'=1),
                                                   1@l2:res:int --> ('RES'=1),
                                                   0@l3:res:int --> ('RES'=0)
                                                  )@l1:res:int --> (('C_RES'=1 -> 'RES'=1 ; 'RES'=0), 'C_RES'=1),
                                        "(if\n  true:c_res:bool --> C_RES=1\nthen\n  1:res:int --> RES=1\nelse\n  0:res:int --> RES=0\n):res:int --> (C_RES=1 -> RES=1 ; RES=0), C_RES=1")).

ut("naming          if true then false else true", t_e_to_n_e1(ite(true@l2:bool, false@l2:bool, true@l3:bool), l1, bool, res, empty, ite(true@l2:c_res:bool, false@l2:res:bool, true@l3:res:bool)@l1:res:bool)).
ut("path            if true then false else true", n_e_to_p_e1(ite(true@l2:c_res:bool, false@l2:res:bool, true@l3:res:bool), l1, res:bool,
                                                      ite(true@l2:c_res:bool --> ('C_RES'=1),
                                                          false@l2:res:bool --> ('RES'=0),
                                                          true@l3:res:bool --> ('RES'=1)
                                                         )@l1:res:bool --> (('C_RES'=1 -> 'RES'=0 ; 'RES'=1), 'C_RES'=1))).
ut("Negative path 1 if true then false else true", \+ n_e_to_p_e1(ite(true@l2:c_res:bool, false@l2:res:bool, true@l3:res:bool), l1, res:bool,
                                                                    ite(true@l2:c_res:bool --> true,
                                                                        false@l2:res:bool --> false,
                                                                        true@l3:res:bool --> true
                                                                       )@l1:res:bool --> _)).
ut("Negative path 2 if true then false else true", \+ n_e_to_p_e1(ite(true@l2:c_res:bool, false@l2:res:bool, true@l3:res:bool), l1, res:bool,
                                                                    ite(true@l2:c_res:bool --> true,
                                                                        false@l2:res:bool --> _,
                                                                        true@l3:res:bool --> _
                                                                       )@l1:res:bool --> (true -> false ; true))).
ut("Negative path 3 if true then false else true", \+ n_e_to_p_e1(ite(true@l2:c_res:bool, false@l2:res:bool, true@l3:res:bool), l1, res:bool,
                                                      ite(true@l2:c_res:bool --> true,
                                                          false@l2:res:bool --> ('RES'=0),
                                                          true@l3:res:bool --> ('RES'=1)
                                                         )@l1:res:bool --> (true -> 'RES'=0 ; 'RES'=1))).
ut("summ            if true then false else true", p_e_to_c1(ite(true@l2:c_res:bool --> ('C_RES'=1),
                                                                 false@l2:res:bool --> ('RES'=0),
                                                                 true@l3:res:bool --> ('RES'=1)
                                                                ), l1, res:bool, empty, true, (true -> 'RES'=0 ; 'RES'=1), empty, [])).

ut("PP path         if true then false else true", pp(ite(true@l2:c_res:bool --> ('C_RES'=1),
                                                          false@l2:res:bool --> ('RES'=0),
                                                          true@l3:res:bool --> ('RES'=1)
                                                         )@l1:res:bool --> (('C_RES'=1 -> 'RES'=0 ; 'RES'=1), 'C_RES'=1),
                                                      "(if\n  true:c_res:bool --> C_RES=1\nthen\n  false:res:bool --> RES=0\nelse\n  true:res:bool --> RES=1\n):res:bool --> (C_RES=1 -> RES=0 ; RES=1), C_RES=1")).

/*
(if
  false:c_f:bool --> C_F=1 ==> {}
then
  (+):f:(a_f:int -> b_f:(ba_f:int -> bb_f:int)) --> BB_F=A_F+BA_F ==> {}
else
  (-):f:(a_f:int -> b_f:(ba_f:int -> bb_f:int)) --> BB_F=A_F-BA_F ==> {}
):f:(a_f:int -> b_f:(ba_f:int -> bb_f:int)) --> ( C_F=1 -> BB_F=A_F+BA_F ; BB_F=A_F-BA_F ), C_F=1 ==> {}
*/
ut("naming          if false then (+) else (-)", t_e_to_n_e1(ite(false@l2:bool, (+)@l2:(int->int->int), (-)@l3:(int->int->int)), l1, (int->int->int), f, empty,
                                                             ite(false@l2:c_f:bool,
                                                                 (+)@l2:f:(a_f:int -> b_f:(ba_f:int -> bb_f:int)),
                                                                 (-)@l3:f:(a_f:int -> b_f:(ba_f:int -> bb_f:int))
                                                                )@l1:f:(a_f:int -> b_f:(ba_f:int -> bb_f:int)))).
ut("path            if false then (+) else (-)", n_e_to_p_e1(ite(false@l2:c_f:bool,
                                                                 (+)@l2:f:(a_f:int -> b_f:(ba_f:int -> bb_f:int)),
                                                                 (-)@l3:f:(a_f:int -> b_f:(ba_f:int -> bb_f:int))
                                                                ), l1, f:(a_f:int -> b_f:(ba_f:int -> bb_f:int)),
                                                             ite(false@l2:c_f:bool --> ('C_F'=0),
                                                                 (+)@l2:f:(a_f:int -> b_f:(ba_f:int -> bb_f:int)) --> ('BB_F'='A_F'+'BA_F'),
                                                                 (-)@l3:f:(a_f:int -> b_f:(ba_f:int -> bb_f:int)) --> ('BB_F'='A_F'-'BA_F')
                                                                )@l1:f:(a_f:int -> b_f:(ba_f:int -> bb_f:int)) --> ( ('C_F'=1 -> 'BB_F'='A_F'+'BA_F' ; 'BB_F'='A_F'-'BA_F'), 'C_F'=0 ))).
ut("summ            if false then (+) else (-)", p_e_to_c1(ite(false@l2:c_f:bool --> ('C_F'=0),
                                                               (+)@l2:f:(a_f:int -> b_f:(ba_f:int -> bb_f:int)) --> ('BB_F'='A_F'+'BA_F'),
                                                               (-)@l3:f:(a_f:int -> b_f:(ba_f:int -> bb_f:int)) --> ('BB_F'='A_F'-'BA_F')
                                                              ), l1, f:(a_f:int -> b_f:(ba_f:int -> bb_f:int)), empty, true, ( ('C_F'=1 -> 'BB_F'='A_F'+'BA_F' ; 'BB_F'='A_F'-'BA_F'), 'C_F'=0 ), empty,
                                                           [])).
ut("Negative summ 1 if false then (+) else (-)", \+ p_e_to_c1(ite(false@l2:c_f:bool --> ('C_F'=0),
                                                                  (+)@l2:f:(a_f:int -> b_f:(ba_f:int -> bb_f:int)) --> ('BB_F'='A_F'+'BA_F'),
                                                                  (-)@l3:f:(a_f:int -> b_f:(ba_f:int -> bb_f:int)) --> ('BB_F'='A_F'-'BA_F')
                                                                 ), l1, f:(a_f:int -> b_f:(ba_f:int -> bb_f:int)), empty, true, ( ('C_F'=1 -> 'BB_F'='A_F'+'BA_F' ; 'BB_F'='A_F'-'BA_F'), 'C_F'=0 ), empty,
                                                              [_])).

ut("PP path         if false then (+) else (-)", pp(ite(false@l2:c_f:bool --> ('C_F'=0),
                                                                 (+)@l2:f:(a_f:int -> b_f:(ba_f:int -> bb_f:int)) --> ('BB_F'='A_F'+'BA_F'),
                                                                 (-)@l3:f:(a_f:int -> b_f:(ba_f:int -> bb_f:int)) --> ('BB_F'='A_F'-'BA_F')
                                                                )@l1:f:(a_f:int -> b_f:(ba_f:int -> bb_f:int)) --> ( ('C_F'=1 -> 'BB_F'='A_F'+'BA_F' ; 'BB_F'='A_F'-'BA_F'), 'C_F'=0 ),
                                              "(if\n  false:c_f:bool --> C_F=0\nthen\n  (+):f:(a_f:int -> b_f:(ba_f:int -> bb_f:int)) --> BB_F=A_F+BA_F\nelse\n  (-):f:(a_f:int -> b_f:(ba_f:int -> bb_f:int)) --> BB_F=A_F-BA_F\n):f:(a_f:int -> b_f:(ba_f:int -> bb_f:int)) --> (C_F=1 -> BB_F=A_F+BA_F ; BB_F=A_F-BA_F), C_F=0")).

/*
(if
  (
    (>):gt_c_v:(a_gt_c_v:int -> b_gt_c_v:(ba_gt_c_v:int -> c_v:bool))
    1:a_gt_c_v:int                                                    --> A_GT_C_V=1                                   ==> {}
    2:ba_gt_c_v:int                                                   --> BA_GT_C_V=2                                  ==> {}
  ):c_v:bool                                                          --> (1>2 -> C_V=1 ; C_V=0)                       ==> {}
then
  false:v:bool                                                        --> V=0                                          ==> {}
else
  true:v:bool                                                         --> V=1                                          ==> {}
):v:bool                                                              --> (C_V=1 -> V=0 ; V=1), (1>2 -> C_V=1 ; C_V=0) ==> {}
*/
ut("path            if 1>2 then false else true", n_e_to_p_e1(ite(app((>)@l2:gt_c_v:(a_gt_c_v:int->b_gt_c_v:(ba_gt_c_v:int->c_v:bool)),
                                                                      [1@l3:a_gt_c_v:int,
                                                                       2@l4:ba_gt_c_v:int]
                                                                     )@l2:c_v:bool,
                                                                  false@l3:v:bool,
                                                                  true@l4:v:bool
                                                                 ), l1, v:bool,
                                                              ite(app((>)@l2:gt_c_v:(a_gt_c_v:int->b_gt_c_v:(ba_gt_c_v:int->c_v:bool)),
                                                                      [1@l3:a_gt_c_v:int --> ('A_GT_C_V'=1),
                                                                       2@l4:ba_gt_c_v:int --> ('BA_GT_C_V'=2)]
                                                                     )@l2:c_v:bool --> (1>2 -> 'C_V'=1 ; 'C_V'=0),
                                                                  false@l3:v:bool --> ('V'=0),
                                                                  true@l4:v:bool --> ('V'=1)
                                                                 )@l1:v:bool --> (('C_V'=1 -> 'V'=0 ; 'V'=1), (1>2 -> 'C_V'=1 ; 'C_V'=0)))).
ut("summ            if 1>2 then false else true", p_e_to_c1(ite(app((>)@l2:gt_c_v:(a_gt_c_v:int->b_gt_c_v:(ba_gt_c_v:int->c_v:bool)),
                                                                    [1@l3:a_gt_c_v:int --> ('A_GT_C_V'=1),
                                                                     2@l4:ba_gt_c_v:int --> ('BA_GT_C_V'=2)]
                                                                   )@l2:c_v:bool --> (1>2 -> 'C_V'=1 ; 'C_V'=0),
                                                                false@l3:v:bool --> ('V'=0),
                                                                true@l4:v:bool --> ('V'=1)
                                                               ), l1, v:bool, empty, true, (('C_V'=1 -> 'V'=0 ; 'V'=1), (1>2 -> 'C_V'=1 ; 'C_V'=0)), empty,
                                                           [])).
ut("Negative summ 1 if 1>2 then false else true", \+ p_e_to_c1(ite(app((>)@l2:gt_c_v:(a_gt_c_v:int->b_gt_c_v:(ba_gt_c_v:int->c_v:bool)),
                                                                    [1@l3:a_gt_c_v:int --> ('A_GT_C_V'=1),
                                                                     2@l4:ba_gt_c_v:int --> ('BA_GT_C_V'=2)]
                                                                   )@l2:c_v:bool --> (1>2 -> 'C_V'=1 ; 'C_V'=0),
                                                                false@l3:v:bool --> ('V'=0),
                                                                true@l4:v:bool --> ('V'=1)
                                                               ), l1, v:bool, empty, true, (('C_V'=1 -> 'V'=0 ; 'V'=1), (1>2 -> 'C_V'=1 ; 'C_V'=0)), empty,
                                                           [_])).
ut("Negative summ 1 if 1>2 then false else true", \+ p_e_to_c1(ite(app((>)@l2:gt_c_v:(a_gt_c_v:int->b_gt_c_v:(ba_gt_c_v:int->c_v:bool)),
                                                                    [1@l3:a_gt_c_v:int --> ('A_GT_C_V'=1),
                                                                     2@l4:ba_gt_c_v:int --> ('BA_GT_C_V'=2)]
                                                                   )@l2:c_v:bool --> (1>2 -> 'C_V'=1 ; 'C_V'=0),
                                                                false@l3:v:bool --> ('V'=0),
                                                                true@l4:v:bool --> ('V'=1)
                                                               ), l1, v:bool, empty, true, (('C_V'=1 -> 'V'=0 ; 'V'=1), (1>2 -> 'C_V'=1 ; 'C_V'=0)), empty,
                                                           [_|_])).



% **********************************************************************
% | x                                                      Identifier
% | c                                                      Constant
% | e e ... e                                              Application
% | fun x ... x -> e                                       Abstraction
% | let x = e in e                                         Let

/*
(let
  x:x:int                                                      ==> {}
=
  1:x:int                               --> X=1                ==> {}
in
  ():v:unit                             --> V=1                ==> {}
):v:unit                                --> V=1, X=1           ==> {}
*/
ut("naming   let x = 1 in ()", t_e_to_n_e1(let(x@l2:int, 1@l3:int, unit@l4:unit), l1, unit, v, empty,
                                           let(x@l2:x:int, 1@l3:x:int, unit@l4:v:unit)@l1:v:unit)).
ut("path     let x = 1 in ()", n_e_to_p_e1(let(x@l2:x:int, 1@l3:x:int, unit@l4:v:unit), l1, v:unit,
                                           let(x@l2:x:int,
                                               1@l3:x:int --> ('X'=1),
                                               unit@l4:v:unit --> ('V'=1)
                                              )@l1:v:unit --> ('V'=1, 'X'=1))).
ut("summ     let x = 1 in ()", p_e_to_c1(let(x@l2:x:int,
                                             1@l3:x:int --> ('X'=1),
                                             unit@l4:v:unit --> ('V'=1)
                                            ), l1, v:unit, empty, true, ('V'=1, 'X'=1), empty, [])).

/*
let
  y:y:int                                                      ==> {}
=
  (1 + 2):y:int                         --> Y=1+2              ==> {}
in
  ():v:unit                             --> V=1                ==> {}
):v:unit                                --> V=1, Y=1+2         ==> {}
*/
ut("naming   let y = 1+2 in ()", t_e_to_n_e1(let(y@l2:int, app((+)@l4:(int->int->int), [1@l5:int, 2@l6:int])@l3:int, unit@lout:unit), l1, unit, v, empty,
                                             let(y@l2:y:int,
                                                 app((+)@l4:plus_y:(a_plus_y:int->b_plus_y:(ba_plus_y:int->y:int)),
                                                     [1@l5:a_plus_y:int,
                                                      2@l6:ba_plus_y:int]
                                                    )@l3:y:int,
                                                 unit@lout:v:unit
                                                )@l1:v:unit)).
ut("path     let y = 1+2 in ()", n_e_to_p_e1(let(y@l2:y:int,
                                                 app((+)@l4:plus_y:(a_plus_y:int->b_plus_y:(ba_plus_y:int->y:int)),
                                                     [1@l5:a_plus_y:int,
                                                      2@l6:ba_plus_y:int]
                                                    )@l3:y:int,
                                                 unit@lout:v:unit
                                                ), l1, v:unit,
                                             let(y@l2:y:int,
                                                 app((+)@l4:plus_y:(a_plus_y:int->b_plus_y:(ba_plus_y:int->y:int)),
                                                     [1@l5:a_plus_y:int --> ('A_PLUS_Y'=1),
                                                      2@l6:ba_plus_y:int --> ('BA_PLUS_Y'=2)]
                                                    )@l3:y:int --> ('Y'=1+2),
                                                 unit@lout:v:unit --> ('V'=1)
                                                )@l1:v:unit --> ('V'=1, 'Y'=1+2))).
ut("summ     let y = 1+2 in ()", p_e_to_c1(let(y@l2:y:int,
                                               app((+)@l4:plus_y:(a_plus_y:int->b_plus_y:(ba_plus_y:int->y:int)),
                                                   [1@l5:a_plus_y:int --> ('A_PLUS_Y'=1),
                                                    2@l6:ba_plus_y:int --> ('BA_PLUS_Y'=2)]
                                                  )@l3:y:int --> ('Y'=1+2),
                                               unit@lout:v:unit --> ('V'=1)
                                              ), l1, v:unit, empty, true, ('V'=1, 'Y'=1+2), empty, [])).

/*
let
  z:z:int                                            ==> {}
=
  (add 1 2):z:int         --> add_i->i->i(1,2,Z)     ==> { ctx_add_i->i->i(X,Y) :- X=1, Y=2 }
in
  ():v:unit               --> V=1                    ==> {}
):v:unit                  --> V=1,add_i->i->i(1,2,Z) ==> {}
*/
ut("naming   let z = add 1 2 in ()", t_e_to_n_e1(let(z@l2:int,
                                                     app(add@l4:(int->int->int),
                                                         [1@l5:int,
                                                          2@l6:int]
                                                        )@l3:int,
                                                     unit@l7:unit), l1, unit, v,
                                                 node(add, add:(a_add:int -> b_add:(ba_add:int -> bb_add:int)), 0, empty, empty),
                                                 let(z@l2:z:int,
                                                     app(add@l4:add_z:(a_add_z:int->b_add:(ba_add_z:int->z:int)),
                                                         [1@l5:a_add_z:int,
                                                          2@l6:ba_add_z:int]
                                                        )@l3:z:int,
                                                     unit@l7:v:unit
                                                    )@l1:v:unit)).
ut("path     let z = add 1 2 in ()", (   t_e_to_n_e1(let(z@l2:int,
                                                     app(add@l4:(int->int->int),
                                                         [1@l5:int,
                                                          2@l6:int]
                                                        )@l3:int,
                                                     unit@l7:unit), l1, unit, v,
                                                 node(add, add:(a_add:int -> b_add:(ba_add:int -> bb_add:int)), 0, empty, empty), En@L:N),
                                         n_e_to_p_e1(En, L, N,
                                                     let(z@l2:z:int,
                                                         app(add@l4:add_z:(a_add_z:int->b_add:(ba_add_z:int->z:int)),
                                                             [1@l5:a_add_z:int-->('A_ADD_Z'=1),
                                                              2@l6:ba_add_z:int-->('BA_ADD_Z'=2)]
                                                            )@l3:z:int-->'add_int->int->int'(1,2,'Z'),
                                                         unit@l7:v:unit-->('V'=1)
                                                        )@l1:v:unit-->('V'=1,'add_int->int->int'(1,2,'Z'))))).
ut("procs    let z = add 1 2 in ()", (   t_e_to_n_e1(let(z@l2:int,
                                                     app(add@l4:(int->int->int),
                                                         [1@l5:int,
                                                          2@l6:int]
                                                        )@l3:int,
                                                     unit@l7:unit), l1, unit, v,
                                                 node(add, add:(a_add:int -> b_add:(ba_add:int -> bb_add:int)), 0, empty, empty), En@L:N),
                                         n_e_to_p_e1(En, L, N, Ep@L:N-->K),
                                         p_e_to_p_d1(Ep, L, N, true, K, empty, empty))).
ut("summ     let z = add 1 2 in ()", (   t_e_to_n_e1(let(z@l2:int,
                                                     app(add@l4:(int->int->int),
                                                         [1@l5:int,
                                                          2@l6:int]
                                                        )@l3:int,
                                                     unit@l7:unit), l1, unit, v,
                                                 node(add, add:(a_add:int -> b_add:(ba_add:int -> bb_add:int)), 0, empty, empty), En@L:N),
                                         n_e_to_p_e1(En, L, N, Ep@L:N-->K),
                                         p_e_to_c1(Ep, L, N, empty, true, K, node(add,(true,(+)@l:(int->int->int)),0,empty,empty),
                                                   [('ctx_add_int->int->int'('A_ADD_Z', 'BA_ADD_Z') :- ('BA_ADD_Z'=2, 'A_ADD_Z'=1)),
                                                    ('add_int->int->int'('A_ADD', 'BA_ADD', 'BB_ADD') :- ('BB_ADD'='A_ADD'+'BA_ADD', 'ctx_add_int->int->int'('A_ADD', 'BA_ADD')))]))).

/*
let
  plus:plus:(x:i->y:i->r:i)       ++> plus:=< true, +:plus:(x:i->y:i->r:i) >
=
  +:plus:(x:i->y:i->r:i)
in
  ():v:unit               --> V=1                                            ==> {}
):v:unit                  --> V=1                                            ==> {}
*/
ut("naming   let plus = (+) in ()", t_e_to_n_e1(let(plus@l:(int->int->int),
                                                    (+)@l:(int->int->int),
                                                    unit@l:unit
                                                   ), l, unit, v, empty,
                                                let(plus@l:plus:(a_plus:int->b_plus:(ba_plus:int->bb_plus:int)),
                                                    (+)@l:(int->int->int),
                                                    unit@l:v:unit
                                                   )@l:v:unit)).
ut("path     let plus = (+) in ()", (   t_e_to_n_e1(let(plus@l:(int->int->int),
                                                    (+)@l:(int->int->int),
                                                    unit@l:unit
                                                   ), l, unit, v, empty, En@L:N),
                                        n_e_to_p_e1(En, L, N,
                                                    let(plus@l:plus:(a_plus:int->b_plus:(ba_plus:int->bb_plus:int)),
                                                        (+)@l:(int->int->int),
                                                        unit@l:v:unit --> ('V'=1)
                                                       )@l:v:unit --> ('V'=1)))).
ut("procs    let plus = (+) in ()", (   t_e_to_n_e1(let(plus@l:(int->int->int),
                                                    (+)@l:(int->int->int),
                                                    unit@l:unit
                                                   ), l, unit, v, empty, En@L:N),
                                        n_e_to_p_e1(En, L, N, Ep@L:N-->K),
                                        p_e_to_p_d1(Ep, L, N, true, K, empty, node(plus,(true,(+)@l:(int->int->int)),0,empty,empty)))).
ut("summ     let plus = (+) in ()", (   n_e_to_p_e1(let(plus@l:plus:(a_plus:int->b_plus:(ba_plus:int->bb_plus:int)),
                                                        (+)@l:plus:(a_plus:int->b_plus:(ba_plus:int->bb_plus:int)),
                                                        unit@l:v:unit
                                                       ), l, v:unit, Ep@L:N-->K),
                                        p_e_to_p_d1(Ep, L, N, true, K, empty, A),
                                        p_e_to_c1(Ep, L, N, empty, true, K, A, []))).


/*
(let
  g:g:(a_g:int -> b_add:(ba_g:int -> bb_g:int))
=
  add:g:(a_g:int -> b_add:(ba_g:int -> bb_g:int))
in
  unit:v:unit                                     --> V=1                                  ==> {}
):v:unit                                          --> V=1                                  ==> {}
*/
% ut("naming   let g = f in ()", false).
% ut("path     let g = f in ()", false).
ut("naming   let g = add in ()", t_e_to_n_e1(let(g@l2:(int->int->int),
                                                 add@l3:(int->int->int),
                                                 unit@l4:unit
                                                ), l1, unit, v, node(add, add:(a_add:int -> b_add:(ba_add:int -> bb_add:int)), 0, empty, empty),
                                             let(g@l2:g:(a_g:int->b_add:(ba_g:int->bb_g:int)),
                                                 add@l3:(int->int->int),
                                                 unit@l4:v:unit
                                                )@l1:v:unit)).
ut("path     let g = add in ()", (   t_e_to_n_e1(let(g@l2:(int->int->int),
                                                 add@l3:(int->int->int),
                                                 unit@l4:unit
                                                ), l1, unit, v, node(add, add:(a_add:int -> b_add:(ba_add:int -> bb_add:int)), 0, empty, empty), En@L:N),
                                     n_e_to_p_e1(En, L, N,
                                                 let(g@l2:g:(a_g:int->b_add:(ba_g:int->bb_g:int)),
                                                     add@l3:(int->int->int),
                                                     unit@l4:v:unit-->('V'=1)
                                                    )@l1:v:unit --> ('V'=1)))).
ut("summ     let g = add in ()", (   t_e_to_n_e1(let(g@l2:(int->int->int),
                                                 add@l3:(int->int->int),
                                                 unit@l4:unit
                                                ), l1, unit, v, node(add, add:(a_add:int -> b_add:(ba_add:int -> bb_add:int)), 0, empty, empty), En@L:N),
                                     n_e_to_p_e1(En, L, N, Ep@L:N-->K),
                                     p_e_to_c1(Ep, L, N, empty, true, K, empty, []))).

/*
(let
  g:g:(ba_add_g:int -> bb_add_g:int)
=
  (
    add:add_g:(a_add_g:int -> g:(ba_add_g:int -> bb_add_g:int))
    1:a_add_g:int
  ):g:(ba_add_g:int -> bb_add_g:int)
in
  unit:v:unit                                     --> V=1                                        ==> {}
):v:unit                                          --> V=1                                        ==> {}
*/
ut("naming   let g = add 1 in ()", t_e_to_n_e1(let(g@l2:(int-> int -> int),
                                                   app(add@l4:(int -> int -> int),
                                                       [1@l5:int]
                                                      )@l3:(int -> int),
                                                   unit@l6:unit
                                                   ), l1, unit, v, node(add, add:(a_add:int -> b_add:(ba_add:int -> bb_add:int)), 0, empty, empty),
                                               let(g@l2:g:(ba_add_g:int->bb_add_g:int),
                                                   app(add@l4:(int -> int -> int),
                                                       [1@l5:int]
                                                      )@l3:(int -> int),
                                                   unit@l6:v:unit
                                                  )@l1:v:unit)).
ut("path     let g = add 1 in ()", (   t_e_to_n_e1(let(g@l2:(int-> int -> int),
                                                       app(add@l4:(int -> int -> int),
                                                           [1@l5:int]
                                                          )@l3:(int -> int),
                                                       unit@l6:unit
                                                      ), l1, unit, v, node(add, add:(a_add:int -> b_add:(ba_add:int -> bb_add:int)), 0, empty, empty), En@L:N),
                                       n_e_to_p_e1(En, L, N,
                                                   let(g@l2:g:(ba_add_g:int->bb_add_g:int),
                                                       app(add@l4:(int -> int -> int),
                                                           [1@l5:int]
                                                          )@l3:(int -> int),
                                                       unit@l6:v:unit --> ('V'=1)
                                                      )@l1:v:unit --> ('V'=1)))).
ut("summ     let g = add 1 in ()", (   t_e_to_n_e1(let(g@l2:(int-> int -> int),
                                                       app(add@l4:(int -> int -> int),
                                                           [1@l5:int]
                                                          )@l3:(int -> int),
                                                       unit@l6:unit
                                                      ), l1, unit, v, node(add, add:(a_add:int -> b_add:(ba_add:int -> bb_add:int)), 0, empty, empty), En@L:N),
                                       n_e_to_p_e1(En, L, N, Ep@L:N-->K),
                                       p_e_to_c1(Ep, L, N, empty, true, K, node(add,(true,(+)@l:add:(a_add:int->b_add:(ba_add:int->bb_add:int))),0,empty,empty), []))).

/*
let h =                                                        ==> { h_i->i(Y,R) :- R=1+Y }
  ((+) 1 ):h:(y:i->r:i)                 --> R=1+Y              ==> {}
in ():v:unit                            --> V=1                ==> {}
*/
% ut("naming   let h = ((+) 1) in ()", false).
% ut("path     let h = ((+) 1) in ()", false).
% ut("summ     let h = ((+) 1) in ()", false).

% /*
% let k =                                                        ==> { k_i->i(Y,R) :- f_i->i->i(1,Y,R) }
%   ( f  1 ):k:(y:i->r:i)                 --> f_i->i->i(1,Y,R)   ==> { ctx_f_i->i->i(X,Y) :- X=1, ctx_k_i->i(Y) }
% in ():v:unit                            --> V=1                ==> {}
% */
% ut("naming   let k = f 1 in ()", false).
% ut("path     let k = f 1 in ()", false).
% ut("summ     let k = f 1 in ()", false).

/*
(let
  id1:id1:(x2:int -> ret_id1:int)
=
  (fun
    x2:x2:int
  ->
    x2:ret_id1:int
  ):id1:(x2:int -> ret_id1:int)
in
  (
    id1:id1_v:(a_id1_v:int -> v:int)
    3:a_id1_v:int                    --> A_ID1_V=3
  ):v:int                            --> 'id1_int->int'(3, V) ==> { 'ctx_id1_int->int'(A_ID1_V) :- A_ID1_V=3 }
):v:int                              --> 'id1_int->int'(3, V) ==> {}
*/
ut("naming   let id1 = fun (x2 : int) -> x2 in id1 3", t_e_to_n_e1(let(id1@l:(int->int),
                                                                       abs([x2@l:int],
                                                                           x2@l:int
                                                                          )@l:(int->int),
                                                                       app(id1@l:(int->int),
                                                                           [3@l:int]
                                                                          )@l:int
                                                                      ), l, int, v, empty,
                                                                   let(id1@l:id1:(x2:int->ret_id1:int),
                                                                       abs([x2@l:int],
                                                                           x2@l:int
                                                                          )@l:(int->int),
                                                                       app(id1@l:id1_v:(a_id1_v:int->v:int),
                                                                           [3@l:a_id1_v:int]
                                                                          )@l:v:int
                                                                      )@l:v:int)).
ut("path     let id1 = fun (x2 : int) -> x2 in id1 3", (   t_e_to_n_e1(let(id1@l:(int->int),
                                                                       abs([x2@l:int],
                                                                           x2@l:int
                                                                          )@l:(int->int),
                                                                       app(id1@l:(int->int),
                                                                           [3@l:int]
                                                                          )@l:int
                                                                      ), l, int, v, empty, En@L:N),
                                                           n_e_to_p_e1(En, L, N,
                                                                       let(id1@l:id1:(x2:int->ret_id1:int),
                                                                           abs([x2@l:int],
                                                                               x2@l:int
                                                                              )@l:(int->int),
                                                                           app(id1@l:id1_v:(a_id1_v:int->v:int),
                                                                               [3@l:a_id1_v:int --> ('A_ID1_V'=3)]
                                                                              )@l:v:int --> 'id1_int->int'(3, 'V')
                                                                          )@l:v:int --> 'id1_int->int'(3, 'V')))).
ut("procs    let id1 = fun (x2 : int) -> x2 in id1 3", (   t_e_to_n_e1(let(id1@l:(int->int),
                                                                       abs([x2@l:int],
                                                                           x2@l:int
                                                                          )@l:(int->int),
                                                                       app(id1@l:(int->int),
                                                                           [3@l:int]
                                                                          )@l:int
                                                                      ), l, int, v, empty, En@L:N),
                                                           n_e_to_p_e1(En, L, N, Ep@L:N-->K),
                                                           p_e_to_p_d1(Ep, L, N, true, K, empty, node(id1,(true,abs([x2@l:int],x2@l:int)@l:(int->int)),0,empty,empty)))).
ut("summ     let id1 = fun (x2 : int) -> x2 in id1 3", (   t_e_to_n_e1(let(id1@l:(int->int),
                                                                           abs([x2@l:int],
                                                                               x2@l:int
                                                                              )@l:(int->int),
                                                                           app(id1@l:(int->int),
                                                                               [3@l:int]
                                                                              )@l:int
                                                                          ), l, int, v, empty, En@L:N),
                                                           n_e_to_p_e1(En, L, N, Ep@L:N-->K),
                                                           p_e_to_p_d1(Ep, L, N, true, K, empty, D),
                                                           p_e_to_c1(Ep, L, N, empty, true, 'id1_int->int'(3,'V'), D,
                                                                     [('ctx_id1_int->int'('A_ID1_V') :- 'A_ID1_V'=3),
                                                                      ('id1_int->int'('X2', 'RET_ID1') :- 'RET_ID1'='X2', 'ctx_id1_int->int'('X2'))]))).

/*
(let
  max1:max1:(x2:int -> f1_max1:(y3:int -> ret_max1:int))
=
  (fun
    x2:x2:int
    y3:y3:int
  ->
    (if
      (
        (>):gt_c_ret_max1:(a_gt_c_ret_max1:int -> b_gt_c_ret_max1:(ba_gt_c_ret_max1:int -> c_ret_max1:bool))
        x2:a_gt_c_ret_max1:int
        y3:ba_gt_c_ret_max1:int
      ):c_ret_max1:bool
    then
      x2:ret_max1:int
    else
      y3:ret_max1:int
    ):ret_max1:int
  ):max1:(x2:int -> f1_max1:(y3:int -> ret_max1:int))
in
  (
    max1:max1_v:(a_max1_v:int -> f1_max1:(ba_max1_v:int -> v:int))
    3:a_max1_v:int                                          --> A_MAX1_V=3                           ==> {}
    1:ba_max1_v:int                                         --> BA_MAX1_V=1                          ==> {}
  ):v:int                                                   --> 'max1_int->int->int'(3, 1, V)        ==> { 'ctx_max1_int->int->int'(A_MAX1_V, BA_MAX1_V) :- BA_MAX1_V=1, A_MAX1_V=3 }
):v:int                                                     --> 'max1_int->int->int'(3, 1, V)        ==> {}
*/
ut("naming   let max1 = fun (x2 : int) y3 -> ... in max1 3 1", t_e_to_n_e1(let(max1@l:(int->int->int),
                                                                               abs([x2@l:int,y3@l:int],
                                                                                   ite(
                                                                                       app(> @l:(int->int->bool),
                                                                                           [x2@l:int,
                                                                                            y3@l:int]
                                                                                          )@l:bool,
                                                                                       x2@l:int,
                                                                                       y3@l:int
                                                                                      )@l:int
                                                                                  )@l:(int->int->int),
                                                                               app(max1@l:(int->int->int),
                                                                                   [3@l:int,
                                                                                    1@l:int]
                                                                                  )@l:int
                                                                              ), l, int, v, empty,
                                                                           let(max1@l:max1:(x2:int->f1_max1:(y3:int->ret_max1:int)),
                                                                               abs([x2@l:int,y3@l:int],
                                                                                   ite(
                                                                                       app(> @l:(int->int->bool),
                                                                                           [x2@l:int,
                                                                                            y3@l:int]
                                                                                          )@l:bool,
                                                                                       x2@l:int,
                                                                                       y3@l:int
                                                                                      )@l:int
                                                                                  )@l:(int->int->int),
                                                                               app(max1@l:max1_v:(a_max1_v:int->f1_max1:(ba_max1_v:int->v:int)),
                                                                                   [3@l:a_max1_v:int,
                                                                                    1@l:ba_max1_v:int]
                                                                                  )@l:v:int
                                                                              )@l:v:int)).
ut("path     let max1 = fun (x2 : int) y3 -> ... in max1 3 1", (   t_e_to_n_e1(let(max1@l:(int->int->int),
                                                                                   abs([x2@l:int,y3@l:int],
                                                                                       ite(
                                                                                           app(> @l:(int->int->bool),
                                                                                               [x2@l:int,
                                                                                                y3@l:int]
                                                                                              )@l:bool,
                                                                                           x2@l:int,
                                                                                           y3@l:int
                                                                                          )@l:int
                                                                                      )@l:(int->int->int),
                                                                                   app(max1@l:(int->int->int),
                                                                                       [3@l:int,
                                                                                        1@l:int]
                                                                                      )@l:int
                                                                                  ), l, int, v, empty, En@L:N),
                                                                   n_e_to_p_e1(En, L, N,
                                                                               let(max1@l:max1:(x2:int->f1_max1:(y3:int->ret_max1:int)),
                                                                                   abs([x2@l:int,y3@l:int],
                                                                                       ite(
                                                                                           app(> @l:(int->int->bool),
                                                                                               [x2@l:int,
                                                                                                y3@l:int]
                                                                                              )@l:bool,
                                                                                           x2@l:int,
                                                                                           y3@l:int
                                                                                          )@l:int
                                                                                      )@l:(int->int->int),
                                                                                   app(max1@l:max1_v:(a_max1_v:int->f1_max1:(ba_max1_v:int->v:int)),
                                                                                       [3@l:a_max1_v:int-->('A_MAX1_V'=3),
                                                                                        1@l:ba_max1_v:int-->('BA_MAX1_V'=1)]
                                                                                      )@l:v:int-->'max1_int->int->int'(3,1,'V')
                                                                                  )@l:v:int-->'max1_int->int->int'(3,1,'V')))).
ut("procs    let max1 = fun (x2 : int) y3 -> ... in max1 3 1", (   t_e_to_n_e1(let(max1@l:(int->int->int),
                                                                               abs([x2@l:int,y3@l:int],
                                                                                   ite(
                                                                                       app(> @l:(int->int->bool),
                                                                                           [x2@l:int,
                                                                                            y3@l:int]
                                                                                          )@l:bool,
                                                                                       x2@l:int,
                                                                                       y3@l:int
                                                                                      )@l:int
                                                                                  )@l:(int->int->int),
                                                                               app(max1@l:(int->int->int),
                                                                                   [3@l:int,
                                                                                    1@l:int]
                                                                                  )@l:int
                                                                              ), l, int, v, empty, En@L:N),
                                                                   n_e_to_p_e1(En, L, N, Ep@L:N-->K),
                                                                   p_e_to_p_d1(Ep, L, N, true, K, empty, node(max1, (true,
                                                                                                                        abs([x2@l:int,y3@l:int],
                                                                                                                            ite(
                                                                                                                                app(> @l:(int->int->bool),
                                                                                                                                    [x2@l:int,
                                                                                                                                     y3@l:int]
                                                                                                                                   )@l:bool,
                                                                                                                                x2@l:int,
                                                                                                                                y3@l:int
                                                                                                                               )@l:int
                                                                                                                           )@l:(int->int->int)), 0, empty, empty)))).
ut("summ     let max1 = fun (x2 : int) y3 -> ... in max1 3 1", (   t_e_to_n_e1(let(max1@l:(int->int->int),
                                                                               abs([x2@l:int,y3@l:int],
                                                                                   ite(
                                                                                       app(> @l:(int->int->bool),
                                                                                           [x2@l:int,
                                                                                            y3@l:int]
                                                                                          )@l:bool,
                                                                                       x2@l:int,
                                                                                       y3@l:int
                                                                                      )@l:int
                                                                                  )@l:(int->int->int),
                                                                               app(max1@l:(int->int->int),
                                                                                   [3@l:int,
                                                                                    1@l:int]
                                                                                  )@l:int
                                                                              ), l, int, v, empty, En@L:N),
                                                                   n_e_to_p_e1(En, L, N, Ep@L:N-->K),
                                                                   p_e_to_p_d1(Ep, L, N, true, K, empty, D),
                                                                   p_e_to_c1(Ep, L, N, empty, true, K, D,
                                                                             [('ctx_max1_int->int->int'('A_MAX1_V', 'BA_MAX1_V') :- 'BA_MAX1_V'=1, 'A_MAX1_V'=3),
                                                                              ('max1_int->int->int'('X2', 'Y3', 'RET_MAX1') :- (('C_RET_MAX1'=1 -> 'RET_MAX1'='X2' ; 'RET_MAX1'='Y3'), ('X2'>'Y3' -> 'C_RET_MAX1'=1 ; 'C_RET_MAX1'=0), 'ctx_max1_int->int->int'('X2', 'Y3')))]))).

% /*
% let l1 =                                                       ==> { l1_i->i->i(X,Y,R) :- R=X+Y }
%   (fun x y ->
%     (x + y):r:i                         --> R=X+Y              ==> {}
%   ):l1:(x:i->y:i->r:i)                  --> R=X+Y              ==> {}
% in ():v:unit                            --> V=1                ==> {}
% */
% ut("naming   let l1 = fun x y -> x+y in ()",
% ut("path     let l1 = fun x y -> x+y in ()", false).
% ut("summ     let l1 = fun x y -> x+y in ()", false).

% /*
% let l2 =                                                       ==> { l2_i->i->i(X,Y,R) :- R=X+Y, Z=1 }
%   (let z =
%     1:z                                 --> Z=1                ==> {}
%   in
%     (fun x y ->
%       (x + y):r:i                       --> R=X+Y              ==> {}
%     ):l2:(x:i->y:i->r:i)                --> R=X+Y              ==> {}
%   ):l2:(x:i->y:i->r:i)                  --> R=X+Y, Z=1         ==> {}
% in ():v:unit                            --> V=1                ==> {}
% */
% ut("naming   let l2 = let z = 1 in fun x y -> x+y in ()", false).
% ut("path     let l2 = let z = 1 in fun x y -> x+y in ()", false).
% ut("summ     let l2 = let z = 1 in fun x y -> x+y in ()", false).

% /*
% let m =                                                        ==> { m_i->i->i(X,B,R) :- R=X+B }
%   (fun x ->
%     ((+) x):pa:(b:i -> r:i)             --> R=X+B              ==> {}
%   ):m:(x:i->b:i->r:i)                   --> R=X+B              ==> {}
% in ():v:unit                            --> V=1                ==> {}
% */
% ut("naming   let m = fun x -> (+) x in ()", false).
% ut("path     let m = fun x -> (+) x in ()", false).
% ut("summ     let m = fun x -> (+) x in ()", false).

% /*
% let o =                                                        ==> { o_i->i(Y,R) :- R=X+Y, X=1 }
%   (
%     (fun x y ->
%       (x + y):r:i                       --> R=X+Y              ==> {}
%     ):f_o:(x:i->y:i->r:i)               --> R=X+Y              ==> {}
%     1:x:i                               --> X=1                ==> {}
%   ):o:(y:i->r:i)                        --> R=X+Y, X=1         ==> {}
% in ():v:unit                            --> V=1                ==> {}
% */
% ut("naming   let o = (fun x y -> x+y) 1 in ()", false).
% ut("path     let o = (fun x y -> x+y) 1 in ()", false).
% ut("summ     let o = (fun x y -> x+y) 1 in ()", false).

% /*
% let p =                                                        ==> { p_i->i->(B,R) :- R=X+B, X=1 }
%   (
%     (fun x ->
%       ((+) x):pa:(b:i -> r:i)           --> R=X+B              ==> {}
%     ):m:(x:i->b:i->r:i)                 --> R=X+B              ==> {}
%     1:x:i                               --> X=1                ==> {}
%   ):p:(b:i->r:i)                        --> R=X+B, X=1         ==> {}
% in ():v:unit                            --> V=1                ==> {}
% */
% ut("naming   let p = (fun x -> (+) x) 1 in ()", false).
% ut("path     let p = (fun x -> (+) x) 1 in ()", false).
% ut("summ     let p = (fun x -> (+) x) 1 in ()", false).


/*
(let
  plus:plus:(x:int -> y:int -> r:int)                         ++> plus := < ctx_plus_i->i->i(X,Y), (+):plus:(x:int -> y:int -> r:int) >
=
  (+):plus:(x:int -> y:int -> r:int)
in
  (
    plus:plus_v:(x_v:int -> y_v:int -> v:int)
    1:x_v:int                                 --> X_V=1                  ==> {}
    2:y_v:int                                 --> Y_V=2                  ==> {}
  ):v:int                                     --> plus(1,2,V)            ==> (goto defn of plus)
):v:int                                       --> plus(1,2,V)                 |
                                                                              | {}, ctx_plus_i->i->i(X,Y) |- (+):plus:(x:int -> y:int -> r:int) --> R=X+Y ==> {}
                                                                              |
                                                                             { plus_i->i->i(X,Y,R)       :- R=X+Y, ctx_plus_i->i->i(X,Y).
                                                                               ctx_plus_i->i->i(X_V,Y_V) :- X_V=1, Y_V=1.                 }
*/
ut("naming          let plus = (+) in plus 1 2", t_e_to_n_e1(let(plus@l:(int->int->int),
                                                                 (+)@l:(int->int->int),
                                                                 app(plus@l:(int->int->int),
                                                                     [1@l:int,
                                                                      2@l:int]
                                                                    )@l:int
                                                                ), l, int, v, empty,
                                                             let(plus@l:plus:(a_plus:int->b_plus:(ba_plus:int->bb_plus:int)),
                                                                 (+)@l:(int->(int->int)),
                                                                 app(plus@l:plus_v:(a_plus_v:int->b_plus:(ba_plus_v:int->v:int)),
                                                                     [1@l:a_plus_v:int,
                                                                      2@l:ba_plus_v:int]
                                                                    )@l:v:int
                                                                )@l:v:int)).
ut("path            let plus = (+) in plus 1 2", (   t_e_to_n_e1(let(plus@l:(int->int->int),
                                                                (+)@l:(int->int->int),
                                                                app(plus@l:(int->int->int),
                                                                    [1@l:int,
                                                                     2@l:int]
                                                                   )@l:int
                                                               ), l, int, v, empty, En@L:N),
                                                     n_e_to_p_e1(En, L, N,
                                                                 let(plus@l:plus:(a_plus:int->b_plus:(ba_plus:int->bb_plus:int)),
                                                                     (+)@l:(int->int->int),
                                                                     app(plus@l:plus_v:(a_plus_v:int->b_plus:(ba_plus_v:int->v:int)),
                                                                         [1@l:a_plus_v:int --> ('A_PLUS_V'=1),
                                                                          2@l:ba_plus_v:int --> ('BA_PLUS_V'=2)]
                                                                        )@l:v:int --> 'plus_int->int->int'(1,2,'V')
                                                                    )@l:v:int --> 'plus_int->int->int'(1,2,'V')))).
ut("summ            let plus = (+) in plus 1 2", (   t_e_to_n_e1(let(plus@l:(int->int->int),
                                                                (+)@l:(int->int->int),
                                                                app(plus@l:(int->int->int),
                                                                    [1@l:int,
                                                                     2@l:int]
                                                                   )@l:int
                                                               ), l, int, v, empty, En@L:N),
                                                     n_e_to_p_e1(En, L, N, Ep@L:N-->K),
                                                     p_e_to_p_d1(Ep, L, N, true, K, empty, D),
                                                     p_e_to_c1(Ep, L, N, empty, true, K, D,
                                                               [('ctx_plus_int->int->int'('A_PLUS_V', 'BA_PLUS_V') :- 'BA_PLUS_V'=2, 'A_PLUS_V'=1),
                                                                ('plus_int->int->int'('A_PLUS', 'BA_PLUS', 'BB_PLUS') :- 'BB_PLUS'='A_PLUS'+'BA_PLUS', 'ctx_plus_int->int->int'('A_PLUS', 'BA_PLUS'))]))).
ut("Negative summ 1 let plus = (+) in plus 1 2", (   t_e_to_n_e1(let(plus@l:(int->int->int),
                                                                (+)@l:(int->int->int),
                                                                app(plus@l:(int->int->int),
                                                                    [1@l:int,
                                                                     2@l:int]
                                                                   )@l:int
                                                               ), l, int, v, empty, En@L:N),
                                                     n_e_to_p_e1(En, L, N, Ep@L:N-->K),
                                                     p_e_to_p_d1(Ep, L, N, true, K, empty, D),
                                                     \+ p_e_to_c1(Ep, L, N, empty, true, K, D, [(_)]))).
ut("Negative summ 2 let plus = (+) in plus 1 2", (   t_e_to_n_e1(let(plus@l:(int->int->int),
                                                                (+)@l:(int->int->int),
                                                                app(plus@l:(int->int->int),
                                                                    [1@l:int,
                                                                     2@l:int]
                                                                   )@l:int
                                                               ), l, int, v, empty, En@L:N),
                                                     n_e_to_p_e1(En, L, N, Ep@L:N-->K),
                                                     p_e_to_p_d1(Ep, L, N, true, K, empty, D),
                                                     \+ p_e_to_c1(Ep, L, N, empty, true, K, D, []))).

ut("naming          let neq = (<>) in neq 1 2", t_e_to_n_e1(let(neq@l:(int->int->bool),
                                                                (<>)@l:(int->int->bool),
                                                                app(neq@l:(int->int->bool),
                                                                    [1@l:int,
                                                                     2@l:int]
                                                                   )@l:bool
                                                               ), l, bool, v, empty,
                                                            let(neq@l:neq:(a_neq:int->b_neq:(ba_neq:int->bb_neq:bool)),
                                                                (<>)@l:(int->int->bool),
                                                                app(neq@l:neq_v:(a_neq_v:int->b_neq:(ba_neq_v:int->v:bool)),
                                                                    [1@l:a_neq_v:int,
                                                                     2@l:ba_neq_v:int]
                                                                   )@l:v:bool
                                                               )@l:v:bool)).
ut("path            let neq = (<>) in neq 1 2", (   t_e_to_n_e1(let(neq@l:(int->int->bool),
                                                                    (<>)@l:(int->int->bool),
                                                                    app(neq@l:(int->int->bool),
                                                                        [1@l:int,
                                                                         2@l:int]
                                                                       )@l:bool
                                                                   ), l, bool, v, empty, En@L:N),
                                                    n_e_to_p_e1(En, L, N,
                                                                let(neq@l:neq:(a_neq:int->b_neq:(ba_neq:int->bb_neq:bool)),
                                                                    (<>)@l:(int->int->bool),
                                                                    app(neq@l:neq_v:(a_neq_v:int->b_neq:(ba_neq_v:int->v:bool)),
                                                                        [1@l:a_neq_v:int --> ('A_NEQ_V'=1),
                                                                         2@l:ba_neq_v:int --> ('BA_NEQ_V'=2)]
                                                                       )@l:v:bool --> 'neq_int->int->bool'(1, 2, 'V')
                                                                   )@l:v:bool --> 'neq_int->int->bool'(1, 2, 'V')))).
ut("Negative path   let neq = (<>) in neq 1 2", (   t_e_to_n_e1(let(neq@l:(int->int->bool),
                                                                    (<>)@l:(int->int->bool),
                                                                    app(neq@l:(int->int->bool),
                                                                        [1@l:int,
                                                                         2@l:int]
                                                                       )@l:bool
                                                                   ), l, bool, v, empty, En@L:N),
                                                    \+ n_e_to_p_e1(En, L, N,
                                                                   let(neq@l:neq:(a_neq:int->b_neq:(ba_neq:int->bb_neq:bool)),
                                                                       (<>)@l:neq:(a_neq:int->b_neq:(ba_neq:int->bb_neq:bool)) --> _,
                                                                       app(neq@l:neq_v:(a_neq_v:int->b_neq:(ba_neq_v:int->v:bool)),
                                                                           [1@l:a_neq_v:int --> _,
                                                                            2@l:ba_neq_v:int --> _]
                                                                          )@l:v:bool --> _
                                                                      )@l:v:bool --> _))).
ut("summ            let neq = (<>) in neq 1 2", (   t_e_to_n_e1(let(neq@l:(int->int->bool),
                                                                    (<>)@l:(int->int->bool),
                                                                    app(neq@l:(int->int->bool),
                                                                        [1@l:int,
                                                                         2@l:int]
                                                                       )@l:bool
                                                                   ), l, bool, v, empty, En@L:N),
                                                    n_e_to_p_e1(En, L, N, Ep@L:N-->K),
                                                    p_e_to_p_d1(Ep, L, N, true, K, empty, D),
                                                    p_e_to_c1(Ep, L, N, empty, true, K, D,
                                                              [('ctx_neq_int->int->bool'('A_NEQ_V', 'BA_NEQ_V') :- ('BA_NEQ_V'=2, 'A_NEQ_V'=1)),
                                                               ('neq_int->int->bool'('A_NEQ', 'BA_NEQ', 'BB_NEQ') :- (('A_NEQ'=\='BA_NEQ' -> 'BB_NEQ'=1 ; 'BB_NEQ'=0), 'ctx_neq_int->int->bool'('A_NEQ','BA_NEQ')))]))).



% **********************************************************************
% | c                                                      Constant
% | e e ... e                                              Application
% | let x = e in e                                         Let
% | assert(e)                                              Assert
% | assume(e)                                              Assume

ut("naming let x = false in x", t_e_to_n_e1(let(x@l:bool,
                                                false@l:bool,
                                                x@l:unit
                                               ), l, unit, v, empty,
                                            let(x@l:x:bool,
                                                false@l:x:bool,
                                                x@l:v:unit
                                               )@l:v:unit)).
ut("path   let x = false in x", (   t_e_to_n_e1(let(x@l:bool,
                                                false@l:bool,
                                                x@l:unit
                                               ), l, unit, v, empty, En@L:N),
                                    n_e_to_p_e1(En, L, N,
                                                let(x@l:x:bool,
                                                    false@l:x:bool --> ('X'=0),
                                                    x@l:v:unit --> ('V'='X')
                                                   )@l:v:unit --> ('V'='X', 'X'=0)))).

ut("naming let x = false in assert x", t_e_to_n_e1(let(x@l:bool,
                                                       false@l:bool,
                                                       assert(
                                                              x@l:bool
                                                             )@l:unit
                                                      ), l, unit, v, empty,
                                                   let(x@l:x:bool,
                                                       false@l:x:bool,
                                                       assert(
                                                              x@l:ase_v:bool
                                                             )@l:v:unit
                                                      )@l:v:unit)).
ut("path   let x = false in assert x", (   t_e_to_n_e1(let(x@l:bool,
                                                           false@l:bool,
                                                           assert(
                                                                  x@l:bool
                                                                 )@l:unit
                                                          ), l, unit, v, empty, En@L:N),
                                           n_e_to_p_e1(En, L, N,
                                                       let(x@l:x:bool,
                                                           false@l:x:bool --> ('X'=0),
                                                           assert(
                                                                  x@l:ase_v:bool --> ('ASE_V'='X')
                                                                 )@l:v:unit --> ('ASE_V'=1, 'ASE_V'='X')
                                                          )@l:v:unit --> ('ASE_V'=1, 'ASE_V'='X', 'X'=0)))).
ut("summ   let x = false in assert x", (   t_e_to_n_e1(let(x@l:bool,
                                                           false@l:bool,
                                                           assert(
                                                                  x@l:bool
                                                                 )@l:unit
                                                          ), l, unit, v, empty, En@L:N),
                                           n_e_to_p_e1(En, L, N, Ep@L:N-->K),
                                           p_e_to_c1(Ep, L, N, empty, true, K, empty,
                                                     [('ASE_V'=1 :- 'ASE_V'='X', 'X'=0)]))).

ut("naming let x = comp 1 2 in assert(x)", t_e_to_n_e1(let(x@l:bool,
                                                           app(comp@l:(int->int->bool),
                                                               [1@l:int,
                                                                2@l:int]
                                                              )@l:bool,
                                                           assert(x@l:bool)@l:unit
                                                          ), l, unit, v, node(comp, comp:(a_comp:int -> b_comp:(ba_comp:int -> bb_comp:bool)), 0, empty, empty),
                                                       let(x@l:x:bool,
                                                           app(comp@l:comp_x:(a_comp_x:int->b_comp:(ba_comp_x:int->x:bool)),
                                                               [1@l:a_comp_x:int,
                                                                2@l:ba_comp_x:int]
                                                              )@l:x:bool,
                                                           assert(x@l:ase_v:bool)@l:v:unit
                                                          )@l:v:unit)).
ut("path   let x = comp 1 2 in assert(x)", (   t_e_to_n_e1(let(x@l:bool,
                                                           app(comp@l:(int->int->bool),
                                                               [1@l:int,
                                                                2@l:int]
                                                              )@l:bool,
                                                           assert(x@l:bool)@l:unit
                                                          ), l, unit, v, node(comp, comp:(a_comp:int -> b_comp:(ba_comp:int -> bb_comp:bool)), 0, empty, empty), En@L:N),
                                               n_e_to_p_e1(En, L, N,
                                                           let(x@l:x:bool,
                                                               app(comp@l:comp_x:(a_comp_x:int->b_comp:(ba_comp_x:int->x:bool)),
                                                                   [1@l:a_comp_x:int --> ('A_COMP_X'=1),
                                                                    2@l:ba_comp_x:int --> ('BA_COMP_X'=2)]
                                                                  )@l:x:bool --> 'comp_int->int->bool'(1, 2, 'X'),
                                                               assert(
                                                                      x@l:ase_v:bool --> ('ASE_V'='X')
                                                                     )@l:v:unit --> ('ASE_V'=1, 'ASE_V'='X')
                                                              )@l:v:unit --> ('ASE_V'=1, 'ASE_V'='X', 'comp_int->int->bool'(1, 2, 'X'))))).
ut("procs  let x = comp 1 2 in assert(x)", (   t_e_to_n_e1(let(x@l:bool,
                                                               app(comp@l:(int->int->bool),
                                                                   [1@l:int,
                                                                    2@l:int]
                                                                  )@l:bool,
                                                               assert(x@l:bool)@l:unit
                                                              ), l, unit, v, node(comp, comp:(a_comp:int -> b_comp:(ba_comp:int -> bb_comp:bool)), 0, empty, empty), En@L:N),
                                               n_e_to_p_e1(En, L, N, Ep@L:N-->K),
                                               p_e_to_p_d1(Ep, L, N, true, K, node(comp,(true,> @l:(int->int->bool)),0,empty,empty), node(comp,(true,> @l:(int->int->bool)),0,empty,empty)))).
ut("summ   let x = comp 1 2 in assert(x)", (   t_e_to_n_e1(let(x@l:bool,
                                                               app(comp@l:(int->int->bool),
                                                                   [1@l:int,
                                                                    2@l:int]
                                                                  )@l:bool,
                                                               assert(x@l:bool)@l:unit
                                                              ), l, unit, v, node(comp, comp:(a_comp:int->b_comp:(ba_comp:int->bb_comp:bool)), 0, empty, empty), En@L:N),
                                               n_e_to_p_e1(En, L, N, Ep@L:N-->K),
                                               p_e_to_p_d1(Ep, L, N, true, K, node(comp,(true,> @l:(int->int->bool)),0,empty,empty), D),
                                               p_e_to_c1(Ep, L, N, empty, true, K, D,
                                                         [('ASE_V'=1 :- 'ASE_V'='X', 'comp_int->int->bool'(1, 2, 'X')),
                                                          ('ctx_comp_int->int->bool'('A_COMP_X', 'BA_COMP_X') :- 'BA_COMP_X'=2, 'A_COMP_X'=1),
                                                          ('comp_int->int->bool'('A_COMP', 'BA_COMP', 'BB_COMP') :- ('A_COMP'>'BA_COMP'->'BB_COMP'=1;'BB_COMP'=0), 'ctx_comp_int->int->bool'('A_COMP', 'BA_COMP'))]))).

ut("naming let y = 1+3 in let x = comp y 2 in assert(x)", t_e_to_n_e1(let(y@l:int,
                                                                          app((+)@l:(int->int->int),
                                                                              [1@l:int,
                                                                               3@l:int]
                                                                             )@l:int,
                                                                          let(x@l:bool,
                                                                              app(comp@l:(int->int->bool),
                                                                                  [y@l:int,
                                                                                   2@l:int]
                                                                                 )@l:bool,
                                                                              assert(x@l:bool)@l:unit
                                                                             )@l:unit
                                                                         ), l, unit, v, node(comp, comp:(a_comp:int -> b_comp:(ba_comp:int -> bb_comp:bool)), 0, empty, empty),
                                                                      let(y@l:y:int,
                                                                          app((+)@l:plus_y:(a_plus_y:int->b_plus_y:(ba_plus_y:int->y:int)),
                                                                              [1@l:a_plus_y:int,
                                                                               3@l:ba_plus_y:int]
                                                                             )@l:y:int,
                                                                          let(x@l:x:bool,
                                                                              app(comp@l:comp_x:(a_comp_x:int->b_comp:(ba_comp_x:int->x:bool)),
                                                                                  [y@l:a_comp_x:int,
                                                                                   2@l:ba_comp_x:int]
                                                                                 )@l:x:bool,
                                                                              assert(x@l:ase_v:bool)@l:v:unit
                                                                             )@l:v:unit
                                                                         )@l:v:unit)).
ut("path   let y = 1+3 in let x = comp y 2 in assert(x)", (   t_e_to_n_e1(let(y@l:int,
                                                                              app((+)@l:(int->int->int),
                                                                                  [1@l:int,
                                                                                   3@l:int]
                                                                                 )@l:int,
                                                                              let(x@l:bool,
                                                                                  app(comp@l:(int->int->bool),
                                                                                      [y@l:int,
                                                                                       2@l:int]
                                                                                     )@l:bool,
                                                                                  assert(x@l:bool)@l:unit
                                                                                 )@l:unit
                                                                             ), l, unit, v, node(comp, comp:(a_comp:int -> b_comp:(ba_comp:int -> bb_comp:bool)), 0, empty, empty), En@L:N),
                                                              n_e_to_p_e1(En, L, N,
                                                                          let(y@l:y:int,
                                                                              app((+)@l:plus_y:(a_plus_y:int->b_plus_y:(ba_plus_y:int->y:int)),
                                                                                  [1@l:a_plus_y:int --> ('A_PLUS_Y'=1),
                                                                                   3@l:ba_plus_y:int --> ('BA_PLUS_Y'=3)]
                                                                                 )@l:y:int --> ('Y'=1+3),
                                                                              let(x@l:x:bool,
                                                                                  app(comp@l:comp_x:(a_comp_x:int->b_comp:(ba_comp_x:int->x:bool)),
                                                                                      [y@l:a_comp_x:int --> ('A_COMP_X'='Y'),
                                                                                       2@l:ba_comp_x:int --> ('BA_COMP_X'=2)]
                                                                                     )@l:x:bool --> 'comp_int->int->bool'('Y', 2, 'X'),
                                                                                  assert(
                                                                                         x@l:ase_v:bool --> ('ASE_V'='X')
                                                                                        )@l:v:unit --> ('ASE_V'=1, 'ASE_V'='X')
                                                                                 )@l:v:unit --> ('ASE_V'=1, 'ASE_V'='X', 'comp_int->int->bool'('Y', 2, 'X'))
                                                                             )@l:v:unit --> ('ASE_V'=1, 'ASE_V'='X', 'comp_int->int->bool'('Y', 2, 'X'), 'Y'=1+3)))).
ut("procs let y = 1+3 in let x = comp y 2 in assert(x)", (   t_e_to_n_e1(let(y@l:int,
                                                                              app((+)@l:(int->int->int),
                                                                                  [1@l:int,
                                                                                   3@l:int]
                                                                                 )@l:int,
                                                                              let(x@l:bool,
                                                                                  app(comp@l:(int->int->bool),
                                                                                      [y@l:int,
                                                                                       2@l:int]
                                                                                     )@l:bool,
                                                                                  assert(x@l:bool)@l:unit
                                                                                 )@l:unit
                                                                             ), l, unit, v, node(comp, comp:(a_comp:int -> b_comp:(ba_comp:int -> bb_comp:bool)), 0, empty, empty), En@L:N),
                                                              n_e_to_p_e1(En, L, N, Ep@L:N-->K),
                                                              p_e_to_p_d1(Ep, L, N, true, K,
                                                                          node(comp,(true,> @l:(int->int->bool)),0,empty,empty),
                                                                          node(comp,(true,> @l:(int->int->bool)),0,empty,empty)))).
ut("summ   let y = 1+3 in let x = comp y 2 in assert(x)", (   Env = node(comp, comp:(a_comp:int -> b_comp:(ba_comp:int -> bb_comp:bool)), 0, empty, empty),
                                                              t_e_to_n_e1(let(y@l:int,
                                                                              app((+)@l:(int->int->int),
                                                                                  [1@l:int,
                                                                                   3@l:int]
                                                                                 )@l:int,
                                                                              let(x@l:bool,
                                                                                  app(comp@l:(int->int->bool),
                                                                                      [y@l:int,
                                                                                       2@l:int]
                                                                                     )@l:bool,
                                                                                  assert(x@l:bool)@l:unit
                                                                                 )@l:unit
                                                                             ), l, unit, v, Env, En@L:N),
                                                              n_e_to_p_e1(En, L, N, Ep@L:N-->K),
                                                              p_e_to_p_d1(Ep, L, N, true, K, node(comp,(true,> @l:(int->int->bool)),0,empty,empty), D),
                                                              p_e_to_c1(Ep, L, N, Env, true, K, D,
                                                                        [('ASE_V'=1:-'ASE_V'='X','comp_int->int->bool'('Y',2,'X'),'Y'=1+3),
                                                                         ('ctx_comp_int->int->bool'('A_COMP_X', 'BA_COMP_X') :- ('BA_COMP_X'=2, 'A_COMP_X'='Y', 'Y'=1+3)),
                                                                         ('comp_int->int->bool'('A_COMP', 'BA_COMP', 'BB_COMP') :- ('A_COMP'>'BA_COMP'->'BB_COMP'=1;'BB_COMP'=0), 'ctx_comp_int->int->bool'('A_COMP', 'BA_COMP'))]))).

ut("naming let x = false && true in x", t_e_to_n_e1(let(x@l:bool,
                                                        app('&&'@l:(bool -> bool -> bool),
                                                            [false@l:bool,
                                                             true@l:bool]
                                                           )@l:bool,
                                                        x@l:bool
                                                       ), l, unit, v, empty,
                                                    let(x@l:x:bool,
                                                        app(&& @l:and_x:(a_and_x:bool->b_and_x:(ba_and_x:bool->x:bool)),
                                                            [false@l:a_and_x:bool,
                                                             true@l:ba_and_x:bool]
                                                           )@l:x:bool,
                                                        x@l:v:bool
                                                       )@l:v:bool)).
ut("path   let x = false && true in x", (   t_e_to_n_e1(let(x@l:bool,
                                                        app('&&'@l:(bool -> bool -> bool),
                                                            [false@l:bool,
                                                             true@l:bool]
                                                           )@l:bool,
                                                        x@l:bool
                                                       ), l, unit, v, empty, En@L:N),
                                            n_e_to_p_e1(En, L, N,
                                                        let(x@l:x:bool,
                                                            app(&& @l:and_x:(a_and_x:bool->b_and_x:(ba_and_x:bool->x:bool)),
                                                                [false@l:a_and_x:bool --> ('A_AND_X'=0),
                                                                 true@l:ba_and_x:bool --> ('BA_AND_X'=1)]
                                                               )@l:x:bool --> (('A_AND_X'=1, 'BA_AND_X'=1 -> 'X'=1 ; 'X'=0), 'BA_AND_X'=1, 'A_AND_X'=0),
                                                            x@l:v:bool --> ('V'='X')
                                                           )@l:v:bool --> ('V'='X', ('A_AND_X'=1, 'BA_AND_X'=1 -> 'X'=1 ; 'X'=0), 'BA_AND_X'=1, 'A_AND_X'=0)))).
ut("summ   let x = false && true in x", (   t_e_to_n_e1(let(x@l:bool,
                                                        app('&&'@l:(bool -> bool -> bool),
                                                            [false@l:bool,
                                                             true@l:bool]
                                                           )@l:bool,
                                                        x@l:bool
                                                       ), l, unit, v, empty, En@L:N),
                                            n_e_to_p_e1(En, L, N, Ep@L:N-->K),
                                            p_e_to_p_d1(Ep, L, N, true, K, empty, A),
                                            p_e_to_c1(Ep, L, N, empty, true, K, A, []))).

/*
assert(
       true@l2:ase_v:bool --> 'ASE_V'=1 ==> {}
      )@l1:v:unit --> 'ASE_V'=1, 'ASE_V'=1 ==> {}
*/
ut("path   assert true", n_e_to_p_e1(assert(true@l2:ase_v:bool), l1, v:unit,
                                     assert(
                                            true@l2:ase_v:bool --> ('ASE_V'=1)
                                           )@l1:v:unit --> ('ASE_V'=1, 'ASE_V'=1))).
ut("procs  assert true", (   n_e_to_p_e1(assert(true@l2:ase_v:bool), l1, v:unit, Ep@L:N-->K),
                             p_e_to_p_d1(Ep, L, N, true, K, empty, empty))).
ut("summ   assert true", (   n_e_to_p_e1(assert(true@l2:ase_v:bool), l1, v:unit, Ep@L:N-->K),
                             p_e_to_p_d1(Ep, L, N, true, K, empty, A),
                             p_e_to_c1(Ep, L, N, empty, true, K, A, [('ASE_V'=1 :- 'ASE_V'=1)]))).

ut("path   assert false", n_e_to_p_e1(assert(false@l2:ase_v:bool), l1, v:unit,
                                      assert(
                                             false@l2:ase_v:bool --> ('ASE_V'=0)
                                            )@l1:v:unit --> ('ASE_V'=1, 'ASE_V'=0))).
ut("summ   assert false", (   n_e_to_p_e1(assert(false@l2:ase_v:bool), l1, v:unit, Ep@L:N-->K),
                              p_e_to_p_d1(Ep, L, N, true, K, empty, A),
                              p_e_to_c1(Ep, L, N, empty, true, K, A, [('ASE_V'=1 :- 'ASE_V'=0)]))).

ut("naming assert(comp 1 2)", t_e_to_n_e1(assert(app(comp@l:(int->int->bool),
                                                     [1@l:int,
                                                      2@l:int]
                                                    )@l:bool
                                                ), l, unit, v, node(comp, comp:(a_comp:int -> b_comp:(ba_comp:int -> bb_comp:bool)), 0, empty, empty),
                                          assert(app(comp@l:comp_ase_v:(a_comp_ase_v:int->b_comp:(ba_comp_ase_v:int->ase_v:bool)),
                                                     [1@l:a_comp_ase_v:int,
                                                      2@l:ba_comp_ase_v:int]
                                                    )@l:ase_v:bool
                                                )@l:v:unit)).
ut("path   assert(comp 1 2)", (   t_e_to_n_e1(assert(app(comp@l:(int->int->bool),
                                                         [1@l:int,
                                                          2@l:int]
                                                        )@l:bool
                                                    ), l, unit, v, node(comp, comp:(a_comp:int -> b_comp:(ba_comp:int -> bb_comp:bool)), 0, empty, empty), En@L:N),
                                  n_e_to_p_e1(En, L, N,
                                              assert(app(comp@l:comp_ase_v:(a_comp_ase_v:int->b_comp:(ba_comp_ase_v:int->ase_v:bool)),
                                                         [1@l:a_comp_ase_v:int --> ('A_COMP_ASE_V'=1),
                                                          2@l:ba_comp_ase_v:int --> ('BA_COMP_ASE_V'=2)]
                                                        )@l:ase_v:bool --> 'comp_int->int->bool'(1, 2, 'ASE_V')
                                                    )@l:v:unit --> ('ASE_V'=1, 'comp_int->int->bool'(1, 2, 'ASE_V'))))).
ut("procs  assert(comp 1 2)", (   t_e_to_n_e1(assert(app(comp@l:(int->int->bool),
                                                         [1@l:int,
                                                          2@l:int]
                                                        )@l:bool
                                                    ), l, unit, v, node(comp, comp:(a_comp:int -> b_comp:(ba_comp:int -> bb_comp:bool)), 0, empty, empty), En@L:N),
                                  n_e_to_p_e1(En, L, N, Ep@L:N-->K),
                                  D = node(comp,(true,> @l:(int->int->bool)),0,empty,empty),
                                  p_e_to_p_d1(Ep, L, N, true, K, D, D))).
ut("summ   assert(comp 1 2)", (   t_e_to_n_e1(assert(app(comp@l:(int->int->bool),
                                                         [1@l:int,
                                                          2@l:int]
                                                        )@l:bool
                                                    ), l, unit, v, node(comp, comp:(a_comp:int -> b_comp:(ba_comp:int -> bb_comp:bool)), 0, empty, empty), En@L:N),
                                  n_e_to_p_e1(En, L, N, Ep@L:N-->K),
                                  p_e_to_p_d1(Ep, L, N, true, K, node(comp,(true,> @l:(int->int->bool)),0,empty,empty), D),
                                  p_e_to_c1(Ep, L, N, empty, true, K, D,
                                            [('ASE_V'=1 :- 'comp_int->int->bool'(1, 2, 'ASE_V')),
                                             ('ctx_comp_int->int->bool'('A_COMP_ASE_V', 'BA_COMP_ASE_V') :- 'BA_COMP_ASE_V'=2, 'A_COMP_ASE_V'=1),
                                             ('comp_int->int->bool'('A_COMP', 'BA_COMP', 'BB_COMP') :- ('A_COMP'>'BA_COMP'->'BB_COMP'=1;'BB_COMP'=0), 'ctx_comp_int->int->bool'('A_COMP', 'BA_COMP'))]))).

ut("path   assert(1>2)", n_e_to_p_e1(assert(app((>)@l2:gt_ase_v:(a_gt_ase_v:int -> b_gt_ase_v:(ba_gt_ase_v:int -> v:bool)),
                                                [1@l3:a_gt_ase_v:int,
                                                 2@l4:ba_gt_ase_v:int]
                                               )@l1:ase_v:bool
                                           ), l, v:bool,
                                     assert(app((>)@l2:gt_ase_v:(a_gt_ase_v:int -> b_gt_ase_v:(ba_gt_ase_v:int -> v:bool)),
                                                [1@l3:a_gt_ase_v:int --> ('A_GT_ASE_V'=1),
                                                 2@l4:ba_gt_ase_v:int --> ('BA_GT_ASE_V'=2)]
                                               )@l1:ase_v:bool --> (1>2 -> 'ASE_V'=1 ; 'ASE_V'=0)
                                           )@l:v:bool --> ('ASE_V'=1, (1>2 -> 'ASE_V'=1 ; 'ASE_V'=0)))).
ut("summ   assert(1>2)", (   n_e_to_p_e1(assert(app((>)@l2:gt_ase_v:(a_gt_ase_v:int -> b_gt_ase_v:(ba_gt_ase_v:int -> v:bool)),
                                                    [1@l3:a_gt_ase_v:int,
                                                     2@l4:ba_gt_ase_v:int]
                                                   )@l1:ase_v:bool
                                               ), l, v:bool, Ep@L:N-->K),
                             p_e_to_p_d1(Ep, L, N, true, K, empty, A),
                             p_e_to_c1(Ep, L, N, empty, true, K, A, [('ASE_V'=1 :- (1>2 -> 'ASE_V'=1 ; 'ASE_V'=0))]))).

ut("path   assert(if true then false else true)", n_e_to_p_e1(assume(ite(true@l2:c_ase_res:bool, false@l2:ase_res:bool, true@l3:ase_res:bool)@l1:ase_res:bool), l0, res:unit,
                                                              assume(ite(true@l2:c_ase_res:bool --> ('C_ASE_RES'=1),
                                                                         false@l2:ase_res:bool --> ('ASE_RES'=0),
                                                                         true@l3:ase_res:bool --> ('ASE_RES'=1)
                                                                        )@l1:ase_res:bool --> (('C_ASE_RES'=1 -> 'ASE_RES'=0 ; 'ASE_RES'=1), 'C_ASE_RES'=1)
                                                                    )@l0:res:unit --> ('ASE_RES'=1, (('C_ASE_RES'=1 -> 'ASE_RES'=0 ; 'ASE_RES'=1), 'C_ASE_RES'=1)))).

ut("path   let x = false in assert(x=true)", n_e_to_p_e1(let(x@l:x:bool,
                                                             false@l:x:bool,
                                                             assert(app(= @l:eq_ase_v:(a_eq_ase_v:bool->b_eq_ase_v:(ba_eq_ase_v:bool->ase_v:bool)),
                                                                        [x@l:a_eq_ase_v:bool,
                                                                         true@l:ba_eq_ase_v:bool]
                                                                       )@l:ase_v:bool
                                                                   )@l:v:unit
                                                            ), l, v:unit,
                                                         let(x@l:x:bool,
                                                             false@l:x:bool --> ('X'=0),
                                                             assert(app(= @l:eq_ase_v:(a_eq_ase_v:bool->b_eq_ase_v:(ba_eq_ase_v:bool->ase_v:bool)),
                                                                        [x@l:a_eq_ase_v:bool --> ('A_EQ_ASE_V'='X'),
                                                                         true@l:ba_eq_ase_v:bool --> ('BA_EQ_ASE_V'=1)]
                                                                       )@l:ase_v:bool --> ('X'=1 -> 'ASE_V'=1 ; 'ASE_V'=0)
                                                                   )@l:v:unit --> ('ASE_V'=1, ('X'=1 -> 'ASE_V'=1 ; 'ASE_V'=0))
                                                            )@l:v:unit --> ('ASE_V'=1, ('X'=1 -> 'ASE_V'=1 ; 'ASE_V'=0), 'X'=0))).

ut("path   let x = 0 in assert(x=1)", n_e_to_p_e1(let(x@l:x:bool,
                                                             0@l:x:bool,
                                                             assert(app(= @l:eq_ase_v:(a_eq_ase_v:bool->b_eq_ase_v:(ba_eq_ase_v:bool->ase_v:bool)),
                                                                        [x@l:a_eq_ase_v:bool,
                                                                         1@l:ba_eq_ase_v:bool]
                                                                       )@l:ase_v:bool
                                                                   )@l:v:unit
                                                            ), l, v:unit,
                                                         let(x@l:x:bool,
                                                             0@l:x:bool --> ('X'=0),
                                                             assert(app(= @l:eq_ase_v:(a_eq_ase_v:bool->b_eq_ase_v:(ba_eq_ase_v:bool->ase_v:bool)),
                                                                        [x@l:a_eq_ase_v:bool --> ('A_EQ_ASE_V'='X'),
                                                                         1@l:ba_eq_ase_v:bool --> ('BA_EQ_ASE_V'=1)]
                                                                       )@l:ase_v:bool --> ('X'=1 -> 'ASE_V'=1 ; 'ASE_V'=0)
                                                                   )@l:v:unit --> ('ASE_V'=1, ('X'=1 -> 'ASE_V'=1 ; 'ASE_V'=0))
                                                            )@l:v:unit --> ('ASE_V'=1, ('X'=1 -> 'ASE_V'=1 ; 'ASE_V'=0), 'X'=0))).

ut("naming let x = false in assert(x || true)", t_e_to_n_e1(let(x@l:bool,
                                                                  false@l:bool,
                                                                  assert(app('||'@l:(bool->bool->bool),
                                                                             [x@l:bool,
                                                                              true@l:bool]
                                                                            )@l:bool
                                                                        )@l:unit
                                                                 ), l, unit, v, empty,
                                                              let(x@l:x:bool,
                                                                  false@l:x:bool,
                                                                  assert(app('||'@l:or_ase_v:(a_or_ase_v:bool->b_or_ase_v:(ba_or_ase_v:bool->ase_v:bool)),
                                                                             [x@l:a_or_ase_v:bool,
                                                                              true@l:ba_or_ase_v:bool]
                                                                            )@l:ase_v:bool
                                                                        )@l:v:unit
                                                                 )@l:v:unit)).
ut("path   let x = false in assert(x || true)", (   t_e_to_n_e1(let(x@l:bool,
                                                                  false@l:bool,
                                                                  assert(app('||'@l:(bool->bool->bool),
                                                                             [x@l:bool,
                                                                              true@l:bool]
                                                                            )@l:bool
                                                                        )@l:unit
                                                                 ), l, unit, v, empty, En@L:N),
                                                    n_e_to_p_e1(En, L, N,
                                                                let(x@l:x:bool,
                                                                    false@l:x:bool --> ('X'=0),
                                                                    assert(app('||'@l:or_ase_v:(a_or_ase_v:bool->b_or_ase_v:(ba_or_ase_v:bool->ase_v:bool)),
                                                                               [x@l:a_or_ase_v:bool --> ('A_OR_ASE_V'='X'),
                                                                                true@l:ba_or_ase_v:bool --> ('BA_OR_ASE_V'=1)]
                                                                              )@l:ase_v:bool --> ((('A_OR_ASE_V'=1 ; 'BA_OR_ASE_V'=1) -> 'ASE_V'=1 ; 'ASE_V'=0), 'BA_OR_ASE_V'=1, 'A_OR_ASE_V'='X')
                                                                          )@l:v:unit --> ('ASE_V'=1, (('A_OR_ASE_V'=1 ; 'BA_OR_ASE_V'=1) -> 'ASE_V'=1 ; 'ASE_V'=0), 'BA_OR_ASE_V'=1, 'A_OR_ASE_V'='X')
                                                                   )@l:v:unit --> ('ASE_V'=1, (('A_OR_ASE_V'=1 ; 'BA_OR_ASE_V'=1) -> 'ASE_V'=1 ; 'ASE_V'=0), 'BA_OR_ASE_V'=1, 'A_OR_ASE_V'='X', 'X'=0)))).

ut("path   let x = false in assert(x && true)", n_e_to_p_e1(let(x@l:x:bool,
                                                                false@l:x:bool,
                                                                assert(app('&&'@l:and_ase_v:(a_and_ase_v:bool->b_and_ase_v:(ba_and_ase_v:bool->ase_v:bool)),
                                                                           [x@l:a_and_ase_v:bool,
                                                                            true@l:ba_and_ase_v:bool]
                                                                          )@l:ase_v:bool
                                                                      )@l:v:unit
                                                               ), l, v:unit,
                                                            let(x@l:x:bool,
                                                                false@l:x:bool --> ('X'=0),
                                                                assert(app('&&'@l:and_ase_v:(a_and_ase_v:bool->b_and_ase_v:(ba_and_ase_v:bool->ase_v:bool)),
                                                                           [x@l:a_and_ase_v:bool --> ('A_AND_ASE_V'='X'),
                                                                            true@l:ba_and_ase_v:bool --> ('BA_AND_ASE_V'=1)]
                                                                          )@l:ase_v:bool --> (('A_AND_ASE_V'=1, 'BA_AND_ASE_V'=1 -> 'ASE_V'=1 ; 'ASE_V'=0), 'BA_AND_ASE_V'=1, 'A_AND_ASE_V'='X')
                                                                      )@l:v:unit --> ('ASE_V'=1, ('A_AND_ASE_V'=1, 'BA_AND_ASE_V'=1 -> 'ASE_V'=1 ; 'ASE_V'=0), 'BA_AND_ASE_V'=1, 'A_AND_ASE_V'='X')
                                                               )@l:v:unit --> ('ASE_V'=1, ('A_AND_ASE_V'=1, 'BA_AND_ASE_V'=1 -> 'ASE_V'=1 ; 'ASE_V'=0), 'BA_AND_ASE_V'=1, 'A_AND_ASE_V'='X', 'X'=0))).
ut("summ   let x = false in assert(x && true)", (   n_e_to_p_e1(let(x@l:x:bool,
                                                                    false@l:x:bool,
                                                                    assert(app('&&'@l:and_ase_v:(a_and_ase_v:bool->b_and_ase_v:(ba_and_ase_v:bool->ase_v:bool)),
                                                                               [x@l:a_and_ase_v:bool,
                                                                                true@l:ba_and_ase_v:bool]
                                                                              )@l:ase_v:bool
                                                                          )@l:v:unit
                                                                   ), l, v:unit, Ep@L:N-->K),
                                                    p_e_to_p_d1(Ep, L, N, true, K, empty, A),
                                                    p_e_to_c1(Ep, L, N, empty, true, K, A,
                                                              [('ASE_V'=1 :- (('A_AND_ASE_V'=1, 'BA_AND_ASE_V'=1 -> 'ASE_V'=1 ; 'ASE_V'=0), 'BA_AND_ASE_V'=1, 'A_AND_ASE_V'='X', 'X'=0))]))).

ut("path   let x = false in assume x", n_e_to_p_e1(let(x@l:x:bool,
                                                        false@l:x:bool,
                                                        assume(
                                                               x@l:asu_v:bool
                                                              )@l:v:unit
                                                       ), l, v:unit,
                                                   let(x@l:x:bool,
                                                       false@l:x:bool --> ('X'=0),
                                                       assume(
                                                              x@l:asu_v:bool --> ('ASU_V'='X')
                                                             )@l:v:unit --> ('ASU_V'=1, 'ASU_V'='X')
                                                      )@l:v:unit --> ('ASU_V'=1, 'ASU_V'='X', 'X'=0))).
ut("summ   let x = false in assume x", (   n_e_to_p_e1(let(x@l:x:bool,
                                                        false@l:x:bool,
                                                        assume(
                                                               x@l:asu_v:bool
                                                              )@l:v:unit
                                                       ), l, v:unit, Ep@L:N-->K),
                                           p_e_to_p_d1(Ep, L, N, true, K, empty, A),
                                           p_e_to_c1(Ep, L, N, empty, true, K, A, []))).

ut("path   let x = false in assume(x=true)", n_e_to_p_e1(let(x@l:x:bool,
                                                             false@l:x:bool,
                                                             assume(app(= @l:eq_asu_v:(a_eq_asu_v:bool->b_eq_asu_v:(ba_eq_asu_v:bool->asu_v:bool)),
                                                                        [x@l:a_eq_asu_v:bool,
                                                                         true@l:ba_eq_asu_v:bool]
                                                                       )@l:asu_v:bool
                                                                   )@l:v:unit
                                                            ), l, v:unit,
                                                         let(x@l:x:bool,
                                                             false@l:x:bool --> ('X'=0),
                                                             assume(app(= @l:eq_asu_v:(a_eq_asu_v:bool->b_eq_asu_v:(ba_eq_asu_v:bool->asu_v:bool)),
                                                                        [x@l:a_eq_asu_v:bool --> ('A_EQ_ASU_V'='X'),
                                                                         true@l:ba_eq_asu_v:bool --> ('BA_EQ_ASU_V'=1)]
                                                                       )@l:asu_v:bool --> ('X'=1 -> 'ASU_V'=1 ; 'ASU_V'=0)
                                                                   )@l:v:unit --> ('ASU_V'=1, ('X'=1 -> 'ASU_V'=1 ; 'ASU_V'=0))
                                                            )@l:v:unit --> ('ASU_V'=1, ('X'=1 -> 'ASU_V'=1 ; 'ASU_V'=0), 'X'=0))).

ut("path   let x = 0 in assume(x=1)", n_e_to_p_e1(let(x@l:x:bool,
                                                             0@l:x:bool,
                                                             assume(app(= @l:eq_asu_v:(a_eq_asu_v:bool->b_eq_asu_v:(ba_eq_asu_v:bool->asu_v:bool)),
                                                                        [x@l:a_eq_asu_v:bool,
                                                                         1@l:ba_eq_asu_v:bool]
                                                                       )@l:asu_v:bool
                                                                   )@l:v:unit
                                                            ), l, v:unit,
                                                         let(x@l:x:bool,
                                                             0@l:x:bool --> ('X'=0),
                                                             assume(app(= @l:eq_asu_v:(a_eq_asu_v:bool->b_eq_asu_v:(ba_eq_asu_v:bool->asu_v:bool)),
                                                                        [x@l:a_eq_asu_v:bool --> ('A_EQ_ASU_V'='X'),
                                                                         1@l:ba_eq_asu_v:bool --> ('BA_EQ_ASU_V'=1)]
                                                                       )@l:asu_v:bool --> ('X'=1 -> 'ASU_V'=1 ; 'ASU_V'=0)
                                                                   )@l:v:unit --> ('ASU_V'=1, ('X'=1 -> 'ASU_V'=1 ; 'ASU_V'=0))
                                                            )@l:v:unit --> ('ASU_V'=1, ('X'=1 -> 'ASU_V'=1 ; 'ASU_V'=0), 'X'=0))).

ut("naming let x = false in assume(x || true)", t_e_to_n_e1(let(x@l:bool,
                                                                  false@l:bool,
                                                                  assume(app('||'@l:(bool->bool->bool),
                                                                             [x@l:bool,
                                                                              true@l:bool]
                                                                            )@l:bool
                                                                        )@l:unit
                                                                 ), l, unit, v, empty,
                                                              let(x@l:x:bool,
                                                                  false@l:x:bool,
                                                                  assume(app('||'@l:or_asu_v:(a_or_asu_v:bool->b_or_asu_v:(ba_or_asu_v:bool->asu_v:bool)),
                                                                             [x@l:a_or_asu_v:bool,
                                                                              true@l:ba_or_asu_v:bool]
                                                                            )@l:asu_v:bool
                                                                        )@l:v:unit
                                                                 )@l:v:unit)).
ut("path   let x = false in assume(x || true)", (   t_e_to_n_e1(let(x@l:bool,
                                                                    false@l:bool,
                                                                    assume(app('||'@l:(bool->bool->bool),
                                                                               [x@l:bool,
                                                                                true@l:bool]
                                                                              )@l:bool
                                                                          )@l:unit
                                                                   ), l, unit, v, empty, En@L:N),
                                                    n_e_to_p_e1(En, L, N,
                                                                let(x@l:x:bool,
                                                                    false@l:x:bool --> ('X'=0),
                                                                    assume(app('||'@l:or_asu_v:(a_or_asu_v:bool->b_or_asu_v:(ba_or_asu_v:bool->asu_v:bool)),
                                                                               [x@l:a_or_asu_v:bool --> ('A_OR_ASU_V'='X'),
                                                                                true@l:ba_or_asu_v:bool --> ('BA_OR_ASU_V'=1)]
                                                                              )@l:asu_v:bool --> ((('A_OR_ASU_V'=1 ; 'BA_OR_ASU_V'=1) -> 'ASU_V'=1 ; 'ASU_V'=0), 'BA_OR_ASU_V'=1, 'A_OR_ASU_V'='X')
                                                                          )@l:v:unit --> ('ASU_V'=1, (('A_OR_ASU_V'=1 ; 'BA_OR_ASU_V'=1) -> 'ASU_V'=1 ; 'ASU_V'=0), 'BA_OR_ASU_V'=1, 'A_OR_ASU_V'='X')
                                                                   )@l:v:unit --> ('ASU_V'=1, (('A_OR_ASU_V'=1 ; 'BA_OR_ASU_V'=1) -> 'ASU_V'=1 ; 'ASU_V'=0), 'BA_OR_ASU_V'=1, 'A_OR_ASU_V'='X', 'X'=0)))).

ut("path   let x = false in assume(x && true)", n_e_to_p_e1(let(x@l:x:bool,
                                                                  false@l:x:bool,
                                                                  assume(app('&&'@l:and_asu_v:(a_and_asu_v:bool->b_and_asu_v:(ba_and_asu_v:bool->asu_v:bool)),
                                                                             [x@l:a_and_asu_v:bool,
                                                                              true@l:ba_and_asu_v:bool]
                                                                            )@l:asu_v:bool
                                                                        )@l:v:unit
                                                                 ), l, v:unit,
                                                            let(x@l:x:bool,
                                                                false@l:x:bool --> ('X'=0),
                                                                assume(app('&&'@l:and_asu_v:(a_and_asu_v:bool->b_and_asu_v:(ba_and_asu_v:bool->asu_v:bool)),
                                                                           [x@l:a_and_asu_v:bool --> ('A_AND_ASU_V'='X'),
                                                                            true@l:ba_and_asu_v:bool --> ('BA_AND_ASU_V'=1)]
                                                                          )@l:asu_v:bool --> (('A_AND_ASU_V'=1, 'BA_AND_ASU_V'=1 -> 'ASU_V'=1 ; 'ASU_V'=0), 'BA_AND_ASU_V'=1, 'A_AND_ASU_V'='X')
                                                                      )@l:v:unit --> ('ASU_V'=1, ('A_AND_ASU_V'=1, 'BA_AND_ASU_V'=1 -> 'ASU_V'=1 ; 'ASU_V'=0), 'BA_AND_ASU_V'=1, 'A_AND_ASU_V'='X')
                                                               )@l:v:unit --> ('ASU_V'=1, ('A_AND_ASU_V'=1, 'BA_AND_ASU_V'=1 -> 'ASU_V'=1 ; 'ASU_V'=0), 'BA_AND_ASU_V'=1, 'A_AND_ASU_V'='X', 'X'=0))).

ut("summ   let x = false in assume(x && true)", (   n_e_to_p_e1(let(x@l:x:bool,
                                                                  false@l:x:bool,
                                                                  assume(app('&&'@l:and_asu_v:(a_and_asu_v:bool->b_and_asu_v:(ba_and_asu_v:bool->asu_v:bool)),
                                                                             [x@l:a_and_asu_v:bool,
                                                                              true@l:ba_and_asu_v:bool]
                                                                            )@l:asu_v:bool
                                                                        )@l:v:unit
                                                                 ), l, v:unit, Ep@L:N-->K),
                                                    p_e_to_p_d1(Ep, L, N, true, K, empty, A),
                                                    p_e_to_c1(Ep, L, N, empty, true, K, A, []))).

ut("naming let x = false && true in assume x", t_e_to_n_e1(let(x@l:bool,
                                                        app('&&'@l:(bool -> bool -> bool),
                                                            [false@l:bool,
                                                             true@l:bool]
                                                           )@l:bool,
                                                        assume(
                                                               x@l:bool
                                                              )@l:unit
                                                       ), l, unit, v, empty,
                                                    let(x@l:x:bool,
                                                        app(&& @l:and_x:(a_and_x:bool->b_and_x:(ba_and_x:bool->x:bool)),
                                                            [false@l:a_and_x:bool,
                                                             true@l:ba_and_x:bool]
                                                           )@l:x:bool,
                                                        assume(
                                                               x@l:asu_v:bool
                                                              )@l:v:unit
                                                       )@l:v:unit)).
ut("path   let x = false && true in assume x", (   t_e_to_n_e1(let(x@l:bool,
                                                        app('&&'@l:(bool -> bool -> bool),
                                                            [false@l:bool,
                                                             true@l:bool]
                                                           )@l:bool,
                                                        assume(
                                                               x@l:bool
                                                              )@l:unit
                                                       ), l, unit, v, empty, En@L:N),
                                                   n_e_to_p_e1(En, L, N,
                                                               let(x@l:x:bool,
                                                                   app(&& @l:and_x:(a_and_x:bool->b_and_x:(ba_and_x:bool->x:bool)),
                                                                       [false@l:a_and_x:bool --> ('A_AND_X'=0),
                                                                        true@l:ba_and_x:bool --> ('BA_AND_X'=1)]
                                                                      )@l:x:bool --> (('A_AND_X'=1, 'BA_AND_X'=1 -> 'X'=1 ; 'X'=0), 'BA_AND_X'=1, 'A_AND_X'=0),
                                                                   assume(
                                                                          x@l:asu_v:bool --> ('ASU_V'='X')
                                                                         )@l:v:unit --> ('ASU_V'=1, 'ASU_V'='X')
                                                                  )@l:v:unit --> ('ASU_V'=1, 'ASU_V'='X', (('A_AND_X'=1, 'BA_AND_X'=1 -> 'X'=1 ; 'X'=0), 'BA_AND_X'=1, 'A_AND_X'=0))))).
ut("summ   let x = false && true in assume x", (   t_e_to_n_e1(let(x@l:bool,
                                                        app('&&'@l:(bool -> bool -> bool),
                                                            [false@l:bool,
                                                             true@l:bool]
                                                           )@l:bool,
                                                        assume(
                                                               x@l:bool
                                                              )@l:unit
                                                       ), l, unit, v, empty, En@L:N),
                                                   n_e_to_p_e1(En, L, N, Ep@L:N-->K),
                                                   p_e_to_p_d1(Ep, L, N, true, K, empty, A),
                                                   p_e_to_c1(Ep, L, N, empty, true, K, A, []))).



% **********************************************************************
% | c                                                      Constant
% | e e ... e                                              Application
% | nondet ()                                              Non-det value
% | if e then e else e                                     If

ut("naming   if nondet () then 1 else 0", t_e_to_n_e1(ite(app(nondet@l:(unit->bool),
                                                              [unit@l:unit]
                                                             )@l:bool,
                                                          1@l:int, 0@l:int), l, int, res, empty,
                                                      ite(app(nondet@l:nondet_c_res:(a_nondet_c_res:unit->c_res:bool),
                                                              [unit@l:a_nondet_c_res:unit]
                                                             )@l:c_res:bool,
                                                          1@l:res:int,
                                                          0@l:res:int
                                                         )@l:res:int)).
ut("path     if nondet () then 1 else 0", n_e_to_p_e1(ite(app(nondet@l:nondet_c_res:(a_nondet_c_res:unit->c_res:bool),
                                                              [unit@l:a_nondet_c_res:unit]
                                                             )@l:c_res:bool,
                                                          1@l:res:int,
                                                          0@l:res:int
                                                         ), l, res:int,
                                                      ite(app(nondet@l:nondet_c_res:(a_nondet_c_res:unit->c_res:bool),
                                                              [unit@l:a_nondet_c_res:unit]
                                                             )@l:c_res:bool-->('C_RES'='_'),
                                                          1@l:res:int-->('RES'=1),
                                                          0@l:res:int-->('RES'=0)
                                                         )@l:res:int-->(('C_RES'=1->'RES'=1;'RES'=0), 'C_RES'='_'))).
ut("summ     if nondet () then 1 else 0", p_e_to_c1(ite(app(nondet@l:nondet_c_res:(a_nondet_c_res:unit->c_res:bool),
                                                              [unit@l:a_nondet_c_res:unit]
                                                             )@l:c_res:bool-->('C_RES'='_'),
                                                          1@l:res:int-->('RES'=1),
                                                          0@l:res:int-->('RES'=0)
                                                         ), l, res:int, empty, true, (('C_RES'=1->'RES'=1;'RES'=0), 'C_RES'='_'), empty, [])).
ut("PP path  if nondet () then 1 else 0", pp(ite(app(nondet@l:nondet_c_res:(a_nondet_c_res:unit->c_res:bool),
                                                              [unit@l:a_nondet_c_res:unit]
                                                             )@l:c_res:bool-->('C_RES'='_'),
                                                          1@l:res:int-->('RES'=1),
                                                          0@l:res:int-->('RES'=0)
                                                         )@l:res:int-->(('C_RES'=1->'RES'=1;'RES'=0), 'C_RES'='_'),
                                             "(if\n  (\n    nondet:nondet_c_res:(a_nondet_c_res:unit -> c_res:bool)\n    unit:a_nondet_c_res:unit\n  ):c_res:bool --> C_RES=_\nthen\n  1:res:int --> RES=1\nelse\n  0:res:int --> RES=0\n):res:int --> (C_RES=1 -> RES=1 ; RES=0), C_RES=_")).

ut("naming  assume-assert", t_e_to_n_e1(let('f1'@l:(int -> unit),
                                            abs(['x2'@l:int],
                                                let('_3'@l:unit,
                                                    assume(app('>'@l:(int -> int -> bool),
                                                               ['x2'@l:int,
                                                                1@l:int
                                                               ]
                                                              )@l:bool
                                                          )@l:unit,
                                                    assert(app('>'@l:(int -> int -> bool),
                                                               ['x2'@l:int,
                                                                0@l:int
                                                               ]
                                                              )@l:bool
                                                          )@l:unit
                                                   )@l:unit
                                               )@l:(int -> unit),
                                            app('f1'@l:(int -> unit),
                                                [app(nondet@l:(unit -> int),
                                                     [unit@l:unit
                                                     ]
                                                    )@l:int
                                                ]
                                               )@l:unit
                                           ), l, unit, v, empty,
                                        let(f1@l:f1:(x2:int->ret_f1:unit),
                                            abs(['x2'@l:int],
                                                let('_3'@l:unit,
                                                    assume(app('>'@l:(int -> int -> bool),
                                                               ['x2'@l:int,
                                                                1@l:int
                                                               ]
                                                              )@l:bool
                                                          )@l:unit,
                                                    assert(app('>'@l:(int -> int -> bool),
                                                               ['x2'@l:int,
                                                                0@l:int
                                                               ]
                                                              )@l:bool
                                                          )@l:unit
                                                   )@l:unit
                                               )@l:(int -> unit),
                                            app(f1@l:f1_v:(a_f1_v:int->v:unit),
                                                [app(nondet@l:nondet_a_f1_v:(a_nondet_a_f1_v:unit->a_f1_v:int),
                                                     [unit@l:a_nondet_a_f1_v:unit]
                                                    )@l:a_f1_v:int]
                                               )@l:v:unit
                                           )@l:v:unit)).
ut("path  assume-assert", (   t_e_to_n_e1(let('f1'@l:(int -> unit),
                                              abs(['x2'@l:int],
                                                  let('_3'@l:unit,
                                                      assume(app('>'@l:(int -> int -> bool),
                                                                 ['x2'@l:int,
                                                                  1@l:int
                                                                 ]
                                                                )@l:bool
                                                            )@l:unit,
                                                      assert(app('>'@l:(int -> int -> bool),
                                                                 ['x2'@l:int,
                                                                  0@l:int
                                                                 ]
                                                                )@l:bool
                                                            )@l:unit
                                                     )@l:unit
                                                 )@l:(int -> unit),
                                              app('f1'@l:(int -> unit),
                                                  [app(nondet@l:(unit -> int),
                                                       [unit@l:unit
                                                       ]
                                                      )@l:int
                                                  ]
                                                 )@l:unit
                                             ), l, unit, v, empty, En@L:N),
                              n_e_to_p_e1(En, L, N,
                                          let(f1@l:f1:(x2:int->ret_f1:unit),
                                              abs(['x2'@l:int],
                                                  let('_3'@l:unit,
                                                      assume(app('>'@l:(int -> int -> bool),
                                                                 ['x2'@l:int,
                                                                  1@l:int
                                                                 ]
                                                                )@l:bool
                                                            )@l:unit,
                                                      assert(app('>'@l:(int -> int -> bool),
                                                                 ['x2'@l:int,
                                                                  0@l:int
                                                                 ]
                                                                )@l:bool
                                                            )@l:unit
                                                     )@l:unit
                                                 )@l:(int -> unit),
                                              app(f1@l:f1_v:(a_f1_v:int->v:unit),
                                                  [app(nondet@l:nondet_a_f1_v:(a_nondet_a_f1_v:unit->a_f1_v:int),
                                                       [unit@l:a_nondet_a_f1_v:unit]
                                                      )@l:a_f1_v:int-->('A_F1_V'='_')]
                                                 )@l:v:unit-->('f1_int->unit'('A_F1_V', 'V'),'A_F1_V'='_')
                                             )@l:v:unit-->('f1_int->unit'('A_F1_V', 'V'),'A_F1_V'='_')))).
ut("procs assume-assert", (   t_e_to_n_e1(let('f1'@l:(int -> unit),
                                              abs(['x2'@l:int],
                                                  let('_3'@l:unit,
                                                      assume(app('>'@l:(int -> int -> bool),
                                                                 ['x2'@l:int,
                                                                  1@l:int
                                                                 ]
                                                                )@l:bool
                                                            )@l:unit,
                                                      assert(app('>'@l:(int -> int -> bool),
                                                                 ['x2'@l:int,
                                                                  0@l:int
                                                                 ]
                                                                )@l:bool
                                                            )@l:unit
                                                     )@l:unit
                                                 )@l:(int -> unit),
                                              app('f1'@l:(int -> unit),
                                                  [app(nondet@l:(unit -> int),
                                                       [unit@l:unit
                                                       ]
                                                      )@l:int
                                                  ]
                                                 )@l:unit
                                             ), l, unit, v, empty, En@L:N),
                              n_e_to_p_e1(En, L, N, Ep@L:N-->K),
                              p_e_to_p_d1(Ep, L, N, true, K, empty,
                                          node(f1, (true,
                                                       abs(['x2'@l:int],
                                                           let('_3'@l:unit,
                                                               assume(app('>'@l:(int -> int -> bool),
                                                                          ['x2'@l:int,
                                                                           1@l:int
                                                                          ]
                                                                         )@l:bool
                                                                     )@l:unit,
                                                               assert(app('>'@l:(int -> int -> bool),
                                                                          ['x2'@l:int,
                                                                           0@l:int
                                                                          ]
                                                                         )@l:bool
                                                                     )@l:unit
                                                              )@l:unit
                                                          )@l:(int -> unit)
                                                   ),0,empty,empty)))).
ut("summ  assume-assert", (   t_e_to_n_e1(let('f1'@l:(int -> unit),
                                              abs(['x2'@l:int],
                                                  let('_3'@l:unit,
                                                      assume(app('>'@l:(int -> int -> bool),
                                                                 ['x2'@l:int,
                                                                  1@l:int
                                                                 ]
                                                                )@l:bool
                                                            )@l:unit,
                                                      assert(app('>'@l:(int -> int -> bool),
                                                                 ['x2'@l:int,
                                                                  0@l:int
                                                                 ]
                                                                )@l:bool
                                                            )@l:unit
                                                     )@l:unit
                                                 )@l:(int -> unit),
                                              app('f1'@l:(int -> unit),
                                                  [app(nondet@l:(unit -> int),
                                                       [unit@l:unit
                                                       ]
                                                      )@l:int
                                                  ]
                                                 )@l:unit
                                             ), l, unit, v, empty, En@L:N),
                              n_e_to_p_e1(En, L, N, Ep@L:N-->K),
                              p_e_to_p_d1(Ep, L, N, true, K, empty, A),
                              p_e_to_c1(Ep, L, N, empty, true, K, A,
                                        [('ctx_f1_int->unit'('A_F1_V') :- 'A_F1_V'='_'),
                                         ('ASE_RET_F1'=1 :- (('X2'>0 -> 'ASE_RET_F1'=1 ; 'ASE_RET_F1'=0), 'ASU__3'=1, ('X2'>1 -> 'ASU__3'=1 ; 'ASU__3'=0), 'ctx_f1_int->unit'('X2'))),
                                         ('f1_int->unit'('X2', 'RET_F1') :- ('ASE_RET_F1'=1, ('X2'>0 -> 'ASE_RET_F1'=1 ; 'ASE_RET_F1'=0), 'ASU__3'=1, ('X2'>1 -> 'ASU__3'=1 ; 'ASU__3'=0), 'ctx_f1_int->unit'('X2')))]))).



% **********************************************************************
% Polymorphism

ut("naming polymorphic (>)", t_e_to_n_e1((>), l, (A->A->bool), v, empty, (>)@l:v:(a_v:A -> b_v:(ba_v:A -> bb_v:bool)))).
ut("path   polymorphic (>)", n_e_to_p_e1((>), l, v:(a_v:A -> b_v:(ba_v:A -> bb_v:bool)),
                                         (>)@l:v:(a_v:A -> b_v:(ba_v:A -> bb_v:bool)) --> ('A_V'>'BA_V' -> 'BB_V'=1 ; 'BB_V'=0))).
ut("summ   polymorphic (>)", p_e_to_c1((>), l, v:(a_v:A -> b_v:(ba_v:A -> bb_v:bool)), empty, true, ('A_V'>'BA_V'), empty, [])).

ut("naming polymorphic let gt = (>) in gt 2 1", t_e_to_n_e1(let(gt@l:(A->A->bool),
                                                                (>)@l:(A->A->bool),
                                                                app(gt@l:(int->int->bool),
                                                                    [2@l:int,
                                                                     1@l:int]
                                                                   )@l:bool
                                                               ), l, bool, v, empty,
                                                            let(gt@l:gt:(a_gt:A->b_gt:(ba_gt:A->bb_gt:bool)),
                                                                (>)@l:(A->A->bool),
                                                                app(gt@l:gt_v:(a_gt_v:int->b_gt:(ba_gt_v:int->v:bool)),
                                                                    [2@l:a_gt_v:int,
                                                                     1@l:ba_gt_v:int]
                                                                   )@l:v:bool
                                                               )@l:v:bool)).
ut("path   polymorphic let gt = (>) in gt 2 1", (   t_e_to_n_e1(let(gt@l:(A->A->bool),
                                                                    (>)@l:(A->A->bool),
                                                                    app(gt@l:(int->int->bool),
                                                                        [2@l:int,
                                                                         1@l:int]
                                                                       )@l:bool
                                                                   ), l, bool, v, empty, En@L:N),
                                                    n_e_to_p_e1(En, L, N,
                                                                let(gt@l:gt:(a_gt:A->b_gt:(ba_gt:A->bb_gt:bool)),
                                                                    (>)@l:(A->A->bool),
                                                                    app(gt@l:gt_v:(a_gt_v:int->b_gt:(ba_gt_v:int->v:bool)),
                                                                        [2@l:a_gt_v:int --> ('A_GT_V'=2),
                                                                         1@l:ba_gt_v:int --> ('BA_GT_V'=1)]
                                                                       )@l:v:bool --> 'gt_int->int->bool'(2, 1, 'V')
                                                                   )@l:v:bool --> 'gt_int->int->bool'(2, 1, 'V')))).
ut("procs  polymorphic let gt = (>) in gt 2 1", (   t_e_to_n_e1(let(gt@l:(A->A->bool),
                                                                    (>)@l:(A->A->bool),
                                                                    app(gt@l:(int->int->bool),
                                                                        [2@l:int,
                                                                         1@l:int]
                                                                       )@l:bool
                                                                   ), l, bool, v, empty, En@L:N),
                                                    n_e_to_p_e1(En, L, N, Ep@L:N-->K),
                                                    p_e_to_p_d1(Ep, L, N, true, K, empty, node(gt,(true,> @l:(A->A->bool)),0,empty,empty)))).
ut("summ   polymorphic let gt = (>) in gt 2 1", (   t_e_to_n_e1(let(gt@l:(A->A->bool),
                                                                    (>)@l:(A->A->bool),
                                                                    app(gt@l:(int->int->bool),
                                                                        [2@l:int,
                                                                         1@l:int]
                                                                       )@l:bool
                                                                   ), l, bool, v, empty, En@L:N),
                                                    n_e_to_p_e1(En, L, N, Ep@L:N-->K),
                                                    p_e_to_p_d1(Ep, L, N, true, K, empty, D),
                                                    p_e_to_c1(Ep, L, N, empty, true, K, D, [('ctx_gt_int->int->bool'('A_GT_V','BA_GT_V'):-'BA_GT_V'=1,'A_GT_V'=2),
                                                                                     ('gt_int->int->bool'('A_GT','BA_GT','BB_GT'):-('A_GT'>'BA_GT'->'BB_GT'=1;'BB_GT'=0),'ctx_gt_int->int->bool'('A_GT','BA_GT'))]))).

ut("summ   polymorphic let gt = (>) in gt (gt 2 1) true", (   t_e_to_n_e1(let(gt@l:(A->A->bool),
                                                                              (>)@l:(A->A->bool),
                                                                              app(gt@l:(bool->bool->bool),
                                                                                  [app(gt@l:(int->int->bool),
                                                                                       [2@l:int,
                                                                                        1@l:int]
                                                                                      )@l:bool,
                                                                                   true@l:bool]
                                                                                 )@l:bool
                                                                             ), l, bool, v, empty, En@L:N),
                                                              n_e_to_p_e1(En, L, N, Ep@L:N-->K),
                                                              p_e_to_p_d1(Ep, L, N, true, K, empty, D),
                                                              p_e_to_c1(Ep, L, N, empty, true, K, D,
                                                                        [('ctx_gt_bool->bool->bool'('A_GT_V','BA_GT_V'):-'BA_GT_V'=1,'gt_int->int->bool'(2,1,'A_GT_V')),
                                                                         ('ctx_gt_int->int->bool'('A_GT_A_GT_V','BA_GT_A_GT_V'):-'BA_GT_A_GT_V'=1,'A_GT_A_GT_V'=2),
                                                                         ('gt_bool->bool->bool'('A_GT','BA_GT','BB_GT'):-('A_GT'>'BA_GT'->'BB_GT'=1;'BB_GT'=0),'ctx_gt_bool->bool->bool'('A_GT','BA_GT')),
                                                                         ('gt_int->int->bool'('A_GT','BA_GT','BB_GT'):-('A_GT'>'BA_GT'->'BB_GT'=1;'BB_GT'=0),'ctx_gt_int->int->bool'('A_GT','BA_GT'))]))).

ut("procs  polymorphic let x = comp 1 2 in assert(x)", (   t_e_to_n_e1(let(x@l:bool,
                                                               app(comp@l:(int->int->bool),
                                                                   [1@l:int,
                                                                    2@l:int]
                                                                  )@l:bool,
                                                               assert(x@l:bool)@l:unit
                                                              ), l, unit, v, node(comp, comp:(a_comp:int -> b_comp:(ba_comp:int -> bb_comp:bool)), 0, empty, empty), En@L:N),
                                               n_e_to_p_e1(En, L, N, Ep@L:N-->K),
                                               p_e_to_p_d1(Ep, L, N, true, K, node(comp,(true,> @l:(A->A->bool)),0,empty,empty), node(comp,(true,> @l:(A->A->bool)),0,empty,empty)))).
ut("summ   polymorphic let x = comp 1 2 in assert(x)", (   t_e_to_n_e1(let(x@l:bool,
                                                               app(comp@l:(int->int->bool),
                                                                   [1@l:int,
                                                                    2@l:int]
                                                                  )@l:bool,
                                                               assert(x@l:bool)@l:unit
                                                              ), l, unit, v, node(comp, comp:(a_comp:A -> b_comp:(ba_comp:A -> bb_comp:bool)), 0, empty, empty), En@L:N),
                                               n_e_to_p_e1(En, L, N, Ep@L:N-->K),
                                               p_e_to_p_d1(Ep, L, N, true, K, node(comp,(true,> @l:(A->A->bool)),0,empty,empty), D),
                                               p_e_to_c1(Ep, L, N, empty, true, K, D,
                                                         [('ASE_V'=1 :- 'ASE_V'='X', 'comp_int->int->bool'(1, 2, 'X')),
                                                          ('ctx_comp_int->int->bool'('A_COMP_X', 'BA_COMP_X') :- 'BA_COMP_X'=2, 'A_COMP_X'=1),
                                                          ('comp_int->int->bool'('A_COMP', 'BA_COMP', 'BB_COMP') :- ('A_COMP'>'BA_COMP'->'BB_COMP'=1;'BB_COMP'=0), 'ctx_comp_int->int->bool'('A_COMP', 'BA_COMP'))]))).

ut("procs polymoprhic let y = 1+3 in let x = comp y 2 in assert(x)", (   t_e_to_n_e1(let(y@l:int,
                                                                              app((+)@l:(int->int->int),
                                                                                  [1@l:int,
                                                                                   3@l:int]
                                                                                 )@l:int,
                                                                              let(x@l:bool,
                                                                                  app(comp@l:(int->int->bool),
                                                                                      [y@l:int,
                                                                                       2@l:int]
                                                                                     )@l:bool,
                                                                                  assert(x@l:bool)@l:unit
                                                                                 )@l:unit
                                                                             ), l, unit, v, node(comp, comp:(a_comp:int -> b_comp:(ba_comp:int -> bb_comp:bool)), 0, empty, empty), En@L:N),
                                                              n_e_to_p_e1(En, L, N, Ep@L:N-->K),
                                                              p_e_to_p_d1(Ep, L, N, true, K,
                                                                          node(comp,(true,> @l:comp:(a_comp:A->b_comp:(ba_comp:A->bb_comp:bool))),0,empty,empty),
                                                                          node(comp,(true,> @l:comp:(a_comp:A->b_comp:(ba_comp:A->bb_comp:bool))),0,empty,empty)))).
ut("summ  polymorphic let y = 1+3 in let x = comp y 2 in assert(x)", (   t_e_to_n_e1(let(y@l:int,
                                                                              app((+)@l:(int->int->int),
                                                                                  [1@l:int,
                                                                                   3@l:int]
                                                                                 )@l:int,
                                                                              let(x@l:bool,
                                                                                  app(comp@l:(int->int->bool),
                                                                                      [y@l:int,
                                                                                       2@l:int]
                                                                                     )@l:bool,
                                                                                  assert(x@l:bool)@l:unit
                                                                                 )@l:unit
                                                                             ), l, unit, v, node(comp, comp:(a_comp:A -> b_comp:(ba_comp:A -> bb_comp:bool)), 0, empty, empty), En@L:N),
                                                              n_e_to_p_e1(En, L, N, Ep@L:N-->K),
                                                              p_e_to_p_d1(Ep, L, N, true, K, node(comp,(true,> @l:(A->A->bool)),0,empty,empty), D),
                                                              p_e_to_c1(Ep, L, N, empty, true, K, D,
                                                                        [('ASE_V'=1:-'ASE_V'='X','comp_int->int->bool'('Y',2,'X'),'Y'=1+3),
                                                                         ('ctx_comp_int->int->bool'('A_COMP_X', 'BA_COMP_X') :- ('BA_COMP_X'=2, 'A_COMP_X'='Y', 'Y'=1+3)),
                                                                         ('comp_int->int->bool'('A_COMP', 'BA_COMP', 'BB_COMP') :- ('A_COMP'>'BA_COMP'->'BB_COMP'=1;'BB_COMP'=0), 'ctx_comp_int->int->bool'('A_COMP', 'BA_COMP'))]))).

ut("procs  polymorphic assert(comp 1 2)", (   t_e_to_n_e1(assert(app(comp@l:(int->int->bool),
                                                                     [1@l:int,
                                                                      2@l:int]
                                                                    )@l:bool
                                                                ), l, unit, v, node(comp, comp:(a_comp:A -> b_comp:(ba_comp:A -> bb_comp:bool)), 0, empty, empty), En@L:N),
                                              n_e_to_p_e1(En, L, N, Ep@L:N-->K),
                                              D = node(comp,(true,> @l:(A->A->bool)),0,empty,empty),
                                              p_e_to_p_d1(Ep, L, N, true, K, D, D))).
ut("summ   polymorphic assert(comp 1 2)", (   t_e_to_n_e1(assert(app(comp@l:(int->int->bool),
                                                                     [1@l:int,
                                                                      2@l:int]
                                                                    )@l:bool
                                                                ), l, unit, v, node(comp, comp:(a_comp:A->b_comp:(ba_comp:A->bb_comp:bool)), 0, empty, empty), En@L:N),
                                              n_e_to_p_e1(En, L, N, Ep@L:N-->K),
                                              p_e_to_p_d1(Ep, L, N, true, K, node(comp,(true,> @l:(A->A->bool)),0,empty,empty), D),
                                              p_e_to_c1(Ep, L, N, empty, true, K, D,
                                                        [('ASE_V'=1 :- 'comp_int->int->bool'(1, 2, 'ASE_V')),
                                                         ('ctx_comp_int->int->bool'('A_COMP_ASE_V', 'BA_COMP_ASE_V') :- 'BA_COMP_ASE_V'=2, 'A_COMP_ASE_V'=1),
                                                         ('comp_int->int->bool'('A_COMP', 'BA_COMP', 'BB_COMP') :- ('A_COMP'>'BA_COMP'->'BB_COMP'=1;'BB_COMP'=0), 'ctx_comp_int->int->bool'('A_COMP', 'BA_COMP'))]))).


ut("summ   polymorphic let max1 = fun x2 y3 -> ... in max1 3 1", (   t_e_to_n_e1(let(max1@l:(A->A->A),
                                                                                     abs([x2@l:A,y3@l:A],
                                                                                         ite(
                                                                                             app(> @l:(A->A->bool),
                                                                                                 [x2@l:A,
                                                                                                  y3@l:A]
                                                                                                )@l:bool,
                                                                                             x2@l:A,
                                                                                             y3@l:A
                                                                                            )@l:A
                                                                                        )@l:(A->A->A),
                                                                                     app(max1@l:(int->int->int),
                                                                                         [3@l:int,
                                                                                          1@l:int]
                                                                                        )@l:int
                                                                                    ), l, int, v, empty, En@L:N),
                                                                     n_e_to_p_e1(En, L, N, Ep@L:N-->K),
                                                                     p_e_to_p_d1(Ep, L, N, true, K, empty, D),
                                                                     p_e_to_c1(Ep, L, N, empty, true, K, D,
                                                                               [('ctx_max1_int->int->int'('A_MAX1_V', 'BA_MAX1_V') :- 'BA_MAX1_V'=1, 'A_MAX1_V'=3),
                                                                                ('max1_int->int->int'('X2', 'Y3', 'RET_MAX1') :- (('C_RET_MAX1'=1 -> 'RET_MAX1'='X2' ; 'RET_MAX1'='Y3'), ('X2'>'Y3' -> 'C_RET_MAX1'=1 ; 'C_RET_MAX1'=0), 'ctx_max1_int->int->int'('X2', 'Y3')))]))).

ut("naming      FO param passing let app = fun g x -> g x in app (+) 1", t_e_to_n_e1(let(app@l:((A->B)->A->B),
                                                                                      abs([g@l:(A->B), x@l:A],
                                                                                          app(g@l:(A->B),
                                                                                              [x@l:A]
                                                                                             )@l:B
                                                                                         )@l:((A->B)->A->B),
                                                                                      app(app@l:((int->int->int)->int->int->int),
                                                                                          [(+)@l:(int->int->int),
                                                                                           1@l:int]
                                                                                         )@l:(int->int)
                                                                                     ), l, (int->int), v, empty,
                                                                                  let(app@l:app:(g:(a_g:A->b_g:B)->f1_app:(x:A->ret_app:B)),
                                                                                      abs([g@l:(A->B), x@l:A],
                                                                                          app(g@l:(A->B),
                                                                                              [x@l:A]
                                                                                             )@l:B
                                                                                         )@l:((A->B)->A->B),
                                                                                      app(app@l:app_v:(g:(a_g:int->b_g:(ba_g:int->bb_g:int))->f1_app:(ba_app_v:int->v:(bba_app_v:int->bbb_app_v:int))),
                                                                                          [(+)@l:g:(a_g:int->b_g:(ba_g:int->bb_g:int)),
                                                                                           1@l:ba_app_v:int]
                                                                                         )@l:v:(bba_app_v:int->bbb_app_v:int)
                                                                                     )@l:v:(bba_app_v:int->bbb_app_v:int))).
ut("path        FO param passing let app = fun g x -> g x in app (+) 1", (   t_e_to_n_e1(let(app@l:((A->B)->A->B),
                                                                                          abs([g@l:(A->B), x@l:A],
                                                                                              app(g@l:(A->B),
                                                                                                  [x@l:A]
                                                                                                 )@l:B
                                                                                             )@l:((A->B)->A->B),
                                                                                          app(app@l:((int->int->int)->int->int->int),
                                                                                              [(+)@l:(int->int->int),
                                                                                               1@l:int]
                                                                                             )@l:(int->int)
                                                                                         ), l, (int->int), v, empty, En@L:N),
                                                                          n_e_to_p_e1(En, L, N,
                                                                                      let(app@l:app:(g:(a_g:A->b_g:B)->f1_app:(x:A->ret_app:B)),
                                                                                          abs([g@l:(A->B), x@l:A],
                                                                                              app(g@l:(A->B),
                                                                                                  [x@l:A]
                                                                                                 )@l:B
                                                                                             )@l:((A->B)->A->B),
                                                                                          app(app@l:app_v:(g:(a_g:int->b_g:(ba_g:int->bb_g:int))->f1_app:(ba_app_v:int->v:(bba_app_v:int->bbb_app_v:int))),
                                                                                              [(+)@l:g:(a_g:int->b_g:(ba_g:int->bb_g:int)) --> ('BB_G'='A_G'+'BA_G'),
                                                                                               1@l:ba_app_v:int --> ('BA_APP_V'=1)]
                                                                                             )@l:v:(bba_app_v:int->bbb_app_v:int) --> 'app_(int->int->int)->int->int->int'(1, 'BBA_APP_V', 'BBB_APP_V')
                                                                                         )@l:v:(bba_app_v:int->bbb_app_v:int) --> 'app_(int->int->int)->int->int->int'(1, 'BBA_APP_V', 'BBB_APP_V')))).
ut("end-to-end  FO param passing let app = fun g x -> g x in app (+) 1", (   t_e_to_c1(let(app@l:((A->B)->A->B),
                                                                                        abs([g@l:(A->B), x@l:A],
                                                                                            app(g@l:(A->B),
                                                                                                [x@l:A]
                                                                                               )@l:B
                                                                                           )@l:((A->B)->A->B),
                                                                                        app(app@l:((int->int->int)->int->int->int),
                                                                                            [(+)@l:(int->int->int),
                                                                                             1@l:int]
                                                                                           )@l:(int->int)
                                                                                       ), l, (int->int), v, empty, true, empty, _N, _Kd,
                                                                                    [('ctx_app_(int->int->int)->int->int->int'('BA_APP_V','BBA_APP_V'):-'BA_APP_V'=1,'ctx_v_int->int'('BBA_APP_V')),
                                                                                     ('ctx_g_int->int->int'('A_G_RET_APP','BA_G_RET_APP'):-'A_G_RET_APP'='X','ctx_app_(int->int->int)->int->int->int'('X','BA_G_RET_APP'),'ctx_v_int->int'('BBA_APP_V')),
                                                                                     ('app_(int->int->int)->int->int->int'('X','BA_G_RET_APP','BB_G_RET_APP'):-'g_int->int->int'('X','BA_G_RET_APP','BB_G_RET_APP'),'ctx_app_(int->int->int)->int->int->int'('X','BA_G_RET_APP'),'ctx_v_int->int'('BBA_APP_V')),
                                                                                     ('g_int->int->int'('A_G','BA_G','BB_G') :- 'BB_G'='A_G'+'BA_G', 'ctx_g_int->int->int'('A_G','BA_G'),'ctx_v_int->int'('BBA_APP_V'))]
                                                                                   ) )).

ut("naming      SO param passing let app = fun g x -> g x in app app (+)", (   t_e_to_n_e1(let(app@l:((A->B)->A->B),
                                                                                            abs([g@l:(A->B), x@l:A],
                                                                                                app(g@l:(A->B),
                                                                                                    [x@l:A]
                                                                                                   )@l:B
                                                                                               )@l:((A->B)->A->B),
                                                                                            app(app@l:(((int->int->int)->int->int->int)->(int->int->int)->int->int->int),
                                                                                                [app@l:((int->int->int)->int->int->int),
                                                                                                 (+)@l:(int->int->int)]
                                                                                               )@l:(int->int->int)
                                                                                           ), l, (int->int->int), v, empty,
                                                                                        let(app@l:app:(g:(a_g:A->b_g:B)->f1_app:(x:A->ret_app:B)),
                                                                                            abs([g@l:(A->B),x@l:A],
                                                                                                app(g@l:(A->B),
                                                                                                    [x@l:A]
                                                                                                   )@l:B
                                                                                               )@l:((A->B)->A->B),
                                                                                            app(app@l:app_v:(g:(g:(aa_g:int->b_g:(aba_g:int->abb_g:int))->f1_app:(ba_g:int->ret_app:(bba_g:int->bbb_g:int)))->f1_app:(x:(a_x:int->b_x:(ba_x:int->bb_x:int))->v:(bba_app_v:int->bbb_app_v:(bbba_app_v:int->bbbb_app_v:int)))),
                                                                                                [app@l:g:(g:(aa_g:int->b_g:(aba_g:int->abb_g:int))->f1_app:(ba_g:int->ret_app:(bba_g:int->bbb_g:int))),
                                                                                                 (+)@l:x:(a_x:int->b_x:(ba_x:int->bb_x:int))]
                                                                                               )@l:v:(bba_app_v:int->bbb_app_v:(bbba_app_v:int->bbbb_app_v:int))
                                                                                           )@l:v:(bba_app_v:int->bbb_app_v:(bbba_app_v:int->bbbb_app_v:int)) ))).
ut("path        SO param passing let app = fun g x -> g x in app app (+)", (   t_e_to_n_e1(let(app@l:((A->B)->A->B),
                                                                                            abs([g@l:(A->B), x@l:A],
                                                                                                app(g@l:(A->B),
                                                                                                    [x@l:A]
                                                                                                   )@l:B
                                                                                               )@l:((A->B)->A->B),
                                                                                            app(app@l:(((int->int->int)->int->int->int)->(int->int->int)->int->int->int),
                                                                                                [app@l:((int->int->int)->int->int->int),
                                                                                                 (+)@l:(int->int->int)]
                                                                                               )@l:(int->int->int)
                                                                                           ), l, (int->int->int), v, empty, En@L:N),
                                                                            n_e_to_p_e1(En, L, N,
                                                                                        let(app@l:app:(g:(a_g:A->b_g:B)->f1_app:(x:A->ret_app:B)),
                                                                                            abs([g@l:(A->B),x@l:A],
                                                                                                app(g@l:(A->B),
                                                                                                    [x@l:A]
                                                                                                   )@l:B
                                                                                               )@l:((A->B)->A->B),
                                                                                            app(app@l:app_v:(g:(g:(aa_g:int -> b_g:(aba_g:int -> abb_g:int)) -> f1_app:(ba_g:int -> ret_app:(bba_g:int ->bbb_g:int)))->f1_app:(x:(a_x:int->b_x:(ba_x:int->bb_x:int))->v:(bba_app_v:int->bbb_app_v:(bbba_app_v:int->bbbb_app_v:int)))),
                                                                                                [app@l:g:(g:(aa_g:int->b_g:(aba_g:int->abb_g:int))->f1_app:(ba_g:int->ret_app:(bba_g:int->bbb_g:int))) --> 'app_(int->int->int)->int->int->int'('BA_G', 'BBA_G', 'BBB_G'),
                                                                                                 (+)@l:x:(a_x:int->b_x:(ba_x:int->bb_x:int)) --> ('BB_X'='A_X'+'BA_X')]
                                                                                               )@l:v:(bba_app_v:int->bbb_app_v:(bbba_app_v:int->bbbb_app_v:int)) --> 'app_((int->int->int)->int->int->int)->(int->int->int)->int->int->int'('BBA_APP_V', 'BBBA_APP_V', 'BBBB_APP_V')
                                                                                           )@l:v:(bba_app_v:int->bbb_app_v:(bbba_app_v:int->bbbb_app_v:int)) --> 'app_((int->int->int)->int->int->int)->(int->int->int)->int->int->int'('BBA_APP_V', 'BBBA_APP_V', 'BBBB_APP_V') ))).
ut("procs       SO param passing let app = fun g x -> g x in app app (+)", (   t_e_to_n_e1(let(app@l:((A->B)->A->B),
                                                                                            abs([g@l:(A->B), x@l:A],
                                                                                                app(g@l:(A->B),
                                                                                                    [x@l:A]
                                                                                                   )@l:B
                                                                                               )@l:((A->B)->A->B),
                                                                                            app(app@l:(((int->int->int)->int->int->int)->(int->int->int)->int->int->int),
                                                                                                [app@l:((int->int->int)->int->int->int),
                                                                                                 (+)@l:(int->int->int)]
                                                                                               )@l:(int->int->int)
                                                                                           ), l, (int->int->int), v, empty, En@L:N),
                                                                            n_e_to_p_e1(En, L, N, Ep@L:N-->K),
                                                                            p_e_to_p_d1(Ep, L, N, true, K, empty, node(app, (true, abs([g@l:(A->B), x@l:A],
                                                                                                app(g@l:(A->B),
                                                                                                    [x@l:A]
                                                                                                   )@l:B
                                                                                               )@l:((A->B)->A->B)), 0, empty, empty)) )).
ut("summ        SO param passing let app = fun g x -> g x in app app (+)", (   t_e_to_n_e1(let(app@l:((A->B)->A->B),
                                                                                            abs([g@l:(A->B), x@l:A],
                                                                                                app(g@l:(A->B),
                                                                                                    [x@l:A]
                                                                                                   )@l:B
                                                                                               )@l:((A->B)->A->B),
                                                                                            app(app@l:(((int->int->int)->int->int->int)->(int->int->int)->int->int->int),
                                                                                                [app@l:((int->int->int)->int->int->int),
                                                                                                 (+)@l:(int->int->int)]
                                                                                               )@l:(int->int->int)
                                                                                           ), l, (int->int->int), v, empty, En@L:N),
                                                                            n_e_to_p_e1(En, L, N, Ep@L:N-->K),
                                                                            p_e_to_p_d1(Ep, L, N, true, K, empty, D),
                                                                            p_e_to_c1(Ep, L, N, empty, true, K, D,
                                                                                      [('ctx_app_((int->int->int)->int->int->int)->(int->int->int)->int->int->int'('BBA_APP_V','BBBA_APP_V'):-true),
                                                                                       ('ctx_app_(int->int->int)->int->int->int'('BA_G','BBA_G'):-'ctx_g_(int->int->int)->int->int->int'('BA_G','BBA_G')),
                                                                                       ('ctx_g_(int->int->int)->int->int->int'('BA_G_RET_APP','BBA_G_RET_APP'):-'ctx_app_((int->int->int)->int->int->int)->(int->int->int)->int->int->int'('BA_G_RET_APP','BBA_G_RET_APP')),
                                                                                       ('ctx_x_int->int->int'('A_A_G','BA_A_G'):-'ctx_a_g_int->int->int'('A_A_G','BA_A_G')),
                                                                                       ('a_g_int->int->int'('A_A_G','BA_A_G','BB_A_G'):-'x_int->int->int'('A_A_G','BA_A_G','BB_A_G'),'ctx_a_g_int->int->int'('A_A_G','BA_A_G'),'ctx_app_((int->int->int)->int->int->int)->(int->int->int)->int->int->int'('BA_G_RET_APP','BBA_G_RET_APP')),
                                                                                       ('app_((int->int->int)->int->int->int)->(int->int->int)->int->int->int'('BA_G_RET_APP','BBA_G_RET_APP','BBB_G_RET_APP'):-'g_(int->int->int)->int->int->int'('BA_G_RET_APP','BBA_G_RET_APP','BBB_G_RET_APP'),'ctx_app_((int->int->int)->int->int->int)->(int->int->int)->int->int->int'('BA_G_RET_APP','BBA_G_RET_APP')),
                                                                                       ('g_(int->int->int)->int->int->int'('BA_G','BBA_G','BBB_G'):-'app_(int->int->int)->int->int->int'('BA_G','BBA_G','BBB_G'),'ctx_g_(int->int->int)->int->int->int'('BA_G','BBA_G')),
                                                                                       ('x_int->int->int'('A_X','BA_X','BB_X'):-'BB_X'='A_X'+'BA_X','ctx_x_int->int->int'('A_X','BA_X'))]) )).
ut("end-to-end  SO param passing let app = fun g x -> g x in app app (+)",
   t_e_to_c1(let(app@l:((A->B)->A->B),
                 abs([g@l:(A->B), x@l:A],
                     app(g@l:(A->B),
                         [x@l:A]
                        )@l:B
                    )@l:((A->B)->A->B),
                 app(app@l:(((int->int->int)->int->int->int)->(int->int->int)->int->int->int),
                     [app@l:((int->int->int)->int->int->int),
                      (+)@l:(int->int->int)]
                    )@l:(int->int->int)
                ), l, (int->int->int), v, empty, true, empty, _N, _K,
             [('ctx_app_((int->int->int)->int->int->int)->(int->int->int)->int->int->int'('BBA_APP_V','BBBA_APP_V'):-'ctx_v_int->int->int'('BBA_APP_V','BBBA_APP_V')),
              ('ctx_app_(int->int->int)->int->int->int'('BA_G','BBA_G'):-'ctx_g_(int->int->int)->int->int->int'('BA_G','BBA_G')),
              ('ctx_g_(int->int->int)->int->int->int'('BA_G_RET_APP','BBA_G_RET_APP'):-'ctx_app_((int->int->int)->int->int->int)->(int->int->int)->int->int->int'('BA_G_RET_APP','BBA_G_RET_APP'),'ctx_v_int->int->int'('BBA_APP_V','BBBA_APP_V')),
              ('ctx_x_int->int->int'('A_A_G','BA_A_G'):-'ctx_a_g_int->int->int'('A_A_G','BA_A_G')),
              ('a_g_int->int->int'('A_A_G','BA_A_G','BB_A_G'):-'x_int->int->int'('A_A_G','BA_A_G','BB_A_G'),'ctx_a_g_int->int->int'('A_A_G','BA_A_G'),'ctx_app_((int->int->int)->int->int->int)->(int->int->int)->int->int->int'('BA_G_RET_APP','BBA_G_RET_APP'),'ctx_v_int->int->int'('BBA_APP_V','BBBA_APP_V')),
              ('app_((int->int->int)->int->int->int)->(int->int->int)->int->int->int'('BA_G_RET_APP','BBA_G_RET_APP','BBB_G_RET_APP'):-'g_(int->int->int)->int->int->int'('BA_G_RET_APP','BBA_G_RET_APP','BBB_G_RET_APP'),'ctx_app_((int->int->int)->int->int->int)->(int->int->int)->int->int->int'('BA_G_RET_APP','BBA_G_RET_APP'),'ctx_v_int->int->int'('BBA_APP_V','BBBA_APP_V')),
              ('g_(int->int->int)->int->int->int'('BA_G','BBA_G','BBB_G'):-'app_(int->int->int)->int->int->int'('BA_G','BBA_G','BBB_G'),'ctx_g_(int->int->int)->int->int->int'('BA_G','BBA_G'),'ctx_v_int->int->int'('BBA_APP_V','BBBA_APP_V')),
              ('x_int->int->int'('A_X','BA_X','BB_X'):-'BB_X'='A_X'+'BA_X','ctx_x_int->int->int'('A_X','BA_X'),'ctx_v_int->int->int'('BBA_APP_V','BBBA_APP_V'))]
            )).

ut("end-to-end  let app = fun g x -> g x in assert(app app (+) 1 2 = 1 + 2)",
   t_e_to_c1(let(app@l:((A->B)->A->B),
                 abs([g@l:(A->B), x@l:A],
                     app(g@l:(A->B),
                         [x@l:A]
                        )@l:B
                    )@l:((A->B)->A->B),
                 assert(
                        app((=)@l:(int->int->bool),
                            [app(app@l:(((int->int->int)->int->int->int)->(int->int->int)->int->int->int),
                                 [app@l:((int->int->int)->int->int->int),
                                  (+)@l:(int->int->int),
                                  1@l:int,
                                  2@l:int]
                                )@l:int,
                             app((+)@l:(int->int->int),
                                 [1@l:int,
                                  2@l:int]
                                )@l:int]
                           )@l:bool
                       )@l:unit
                ), l, unit, v, empty, true, empty, _N, _K,
             [('ASE_V'=1:-('A_EQ_ASE_V'='BA_EQ_ASE_V'->'ASE_V'=1;'ASE_V'=0),'BA_EQ_ASE_V'=1+2,'app_((int->int->int)->int->int->int)->(int->int->int)->int->int->int'(1,2,'A_EQ_ASE_V')),
              ('ctx_app_((int->int->int)->int->int->int)->(int->int->int)->int->int->int'('BBA_APP_A_EQ_ASE_V','BBBA_APP_A_EQ_ASE_V'):-'BBBA_APP_A_EQ_ASE_V'=2,'BBA_APP_A_EQ_ASE_V'=1),
              ('ctx_app_(int->int->int)->int->int->int'('BA_G','BBA_G'):-'ctx_g_(int->int->int)->int->int->int'('BA_G','BBA_G')),
              ('ctx_g_(int->int->int)->int->int->int'('BA_G_RET_APP','BBA_G_RET_APP'):-'ctx_app_((int->int->int)->int->int->int)->(int->int->int)->int->int->int'('BA_G_RET_APP','BBA_G_RET_APP')),
              ('ctx_x_int->int->int'('A_A_G','BA_A_G'):-'ctx_a_g_int->int->int'('A_A_G','BA_A_G')),
              ('a_g_int->int->int'('A_A_G','BA_A_G','BB_A_G'):-'x_int->int->int'('A_A_G','BA_A_G','BB_A_G'),'ctx_a_g_int->int->int'('A_A_G','BA_A_G'),'ctx_app_((int->int->int)->int->int->int)->(int->int->int)->int->int->int'('BA_G_RET_APP','BBA_G_RET_APP')),
              ('app_((int->int->int)->int->int->int)->(int->int->int)->int->int->int'('BA_G_RET_APP','BBA_G_RET_APP','BBB_G_RET_APP'):-'g_(int->int->int)->int->int->int'('BA_G_RET_APP','BBA_G_RET_APP','BBB_G_RET_APP'),'ctx_app_((int->int->int)->int->int->int)->(int->int->int)->int->int->int'('BA_G_RET_APP','BBA_G_RET_APP')),
              ('g_(int->int->int)->int->int->int'('BA_G','BBA_G','BBB_G'):-'app_(int->int->int)->int->int->int'('BA_G','BBA_G','BBB_G'),'ctx_g_(int->int->int)->int->int->int'('BA_G','BBA_G')),
              ('x_int->int->int'('A_X','BA_X','BB_X'):-'BB_X'='A_X'+'BA_X','ctx_x_int->int->int'('A_X','BA_X'))]
            )).

ut("etoe fun x y -> let app = fun g x -> g x in assert(app app (+) x y=x+y)",
   t_e_to_c1(abs([x@l:int,
                  y@l:int],
                 let(app@l:((A->B)->A->B),
                     abs([g@l:(A->B), x@l:A],
                         app(g@l:(A->B),
                             [x@l:A]
                            )@l:B
                        )@l:((A->B)->A->B),
                     assert(
                            app((=)@l:(int->int->bool),
                                [app(app@l:(((int->int->int)->int->int->int)->(int->int->int)->int->int->int),
                                     [app@l:((int->int->int)->int->int->int),
                                      (+)@l:(int->int->int),
                                      x@l:int,
                                      y@l:int]
                                    )@l:int,
                                 app((+)@l:(int->int->int),
                                     [x@l:int,
                                      y@l:int]
                                    )@l:int]
                               )@l:bool
                           )@l:unit
                    )@l:unit
                ), l, (int->int->unit), v, empty, true, empty, _N, _K,
             [('ASE_RET_V'=1:-('A_EQ_ASE_RET_V'='BA_EQ_ASE_RET_V'->'ASE_RET_V'=1;'ASE_RET_V'=0),'BA_EQ_ASE_RET_V'='X'+'Y','app_((int->int->int)->int->int->int)->(int->int->int)->int->int->int'('X','Y','A_EQ_ASE_RET_V'),'ctx_v_int->int->unit'('X','Y')),
              ('ctx_app_((int->int->int)->int->int->int)->(int->int->int)->int->int->int'('BBA_APP_A_EQ_ASE_RET_V','BBBA_APP_A_EQ_ASE_RET_V'):-'BBBA_APP_A_EQ_ASE_RET_V'='Y','BBA_APP_A_EQ_ASE_RET_V'='X','ctx_v_int->int->unit'('X','Y')),
              ('ctx_app_(int->int->int)->int->int->int'('BA_G','BBA_G'):-'ctx_g_(int->int->int)->int->int->int'('BA_G','BBA_G')),
              ('ctx_g_(int->int->int)->int->int->int'('BA_G_RET_APP','BBA_G_RET_APP'):-'ctx_app_((int->int->int)->int->int->int)->(int->int->int)->int->int->int'('BA_G_RET_APP','BBA_G_RET_APP'),'ctx_v_int->int->unit'('X','Y')),
              ('ctx_x_int->int->int'('A_A_G','BA_A_G'):-'ctx_a_g_int->int->int'('A_A_G','BA_A_G')),
              ('a_g_int->int->int'('A_A_G','BA_A_G','BB_A_G'):-'x_int->int->int'('A_A_G','BA_A_G','BB_A_G'),'ctx_a_g_int->int->int'('A_A_G','BA_A_G'),'ctx_app_((int->int->int)->int->int->int)->(int->int->int)->int->int->int'('BA_G_RET_APP','BBA_G_RET_APP'),'ctx_v_int->int->unit'('X','Y')),
              ('app_((int->int->int)->int->int->int)->(int->int->int)->int->int->int'('BA_G_RET_APP','BBA_G_RET_APP','BBB_G_RET_APP'):-'g_(int->int->int)->int->int->int'('BA_G_RET_APP','BBA_G_RET_APP','BBB_G_RET_APP'),'ctx_app_((int->int->int)->int->int->int)->(int->int->int)->int->int->int'('BA_G_RET_APP','BBA_G_RET_APP'),'ctx_v_int->int->unit'('X','Y')),
              ('g_(int->int->int)->int->int->int'('BA_G','BBA_G','BBB_G'):-'app_(int->int->int)->int->int->int'('BA_G','BBA_G','BBB_G'),'ctx_g_(int->int->int)->int->int->int'('BA_G','BBA_G'),'ctx_v_int->int->unit'('X','Y')),
              ('x_int->int->int'('A_X','BA_X','BB_X'):-'BB_X'='A_X'+'BA_X','ctx_x_int->int->int'('A_X','BA_X'),'ctx_v_int->int->unit'('X','Y'))]
            )).

% ut("etoe   let app g x = g x in assert(app (fun a b -> a + b) 1 2 = 1 + 2)",
%    t_e_to_c1(let(app@l:((A->B)->A->B),
%                  abs([g@l:(A->B), x@l:A],
%                      app(g@l:(A->B),
%                          [x@l:A]
%                         )@l:B
%                     )@l:((A->B)->A->B),
%                  assert(
%                         app((=)@l:(int->int->bool),
%                             [app(app@l:((int->int->int)->int->int->int),
%                                  [abs([a@l:int, b@l:int],
%                                       app((+)@l:(int->int->int),
%                                           [a@l:int,
%                                            b@l:int]
%                                          )@l:int
%                                      )@l:(int->int->int),
%                                   1@l:int,
%                                   2@l:int]
%                                 )@l:int,
%                              app((+)@l:(int->int->int),
%                                  [1@l:int,
%                                   2@l:int]
%                                 )@l:int]
%                            )@l:bool
%                        )@l:unit
%                 ), l, unit, v, empty, true, empty, _N, _K, [])).

% /*

% * Names for main application:

% let apptoinc f : int = f (fun x -> x + 1) in
% let appto1 g : int = g 1 in
% apptoinc:apptoinc_v:(f:(g:(aa_f:int -> ab_f:int) -> b_f:int) -> v:int)
% appto1:              f:(g:(aa_f:int -> ab_f:int) -> b_f:int)

% * Steps for naming main application:

% (1) Name head. Name actuals with name of formals.
% apptoinc:apptoinc_v:(f:(a_f:(aaa_apptoinc_v:int -> aab_apptoinc_v:int) -> ab_apptoinc_v:int) -> b_apptoinc_v:int)
% appto1:              f:(g  :(aa_f          :int -> ab_f          :int) -> b_f          :int)

% (2) Replace formals with actuals.
% apptoinc:apptoinc_v:(f:(g  :(aa_f          :int -> ab_f          :int) -> b_f          :int) -> b_apptoinc_v:int)
% appto1:              f:(g  :(aa_f          :int -> ab_f          :int) -> b_f          :int)

% (3) Rename return.
% apptoinc:apptoinc_v:(f:(g  :(aa_f          :int -> ab_f          :int) -> b_f          :int) ->            v:int)
% appto1:              f:(g  :(aa_f          :int -> ab_f          :int) -> b_f          :int)

% *Constraints from main application:

% 'ctx_apptoinc_(((int->int)->int)->int)' :- true.
% 'f_((int->int)->int)'(B_F) :- 'appto1_((int->int)->int)'(B_F).
% 'ctx_appto1_((int->int)->int)' :- 'ctx_f_((int->int)->int)'.

% * Names for application in apptoinc:

% let apptoinc f : int = f (fun x -> x + 1)



% Constraints from application in apptoinc:

% 'g_int->int'(X, RET_G) :- RET_G = X + 1, 'ctx_g_int->int'(X), 'ctx_apptoinc_(((int->int)->int)->int)'.
% 'ctx

% */
% /* let apptoinc f : int = f (fun x -> x + 1) in let appto1 g : int = g 1 in apptoinc appto1 */
% ut("etoe TO param passing",
%    t_e_to_c1(let(apptoinc@l:(((int->int)->int)->int),
%                  abs([f@l:((int->int)->int)],
%                      app(f@l:((int->int)->int),
%                          [abs([x@l:int],
%                               app((+)@l:(int->int->int),
%                                   [x@l:int,
%                                    1@l:int]
%                                  )@l:int
%                              )@l:(int->int)]
%                         )@l:int
%                     )@l:(((int->int)->int)->int),
%                  let(appto1@l:((int->int)->int),
%                      abs([g@l:(int->int)],
%                          app(g@l:(int->int),
%                              [1@l:int]
%                             )@l:int
%                         )@l:((int->int)->int),
%                      app(apptoinc@l:(((int->int)->int)->int),
%                          [appto1@l:((int->int)->int)]
%                         )@l:int
%                      )@l:int
%                 ), l, int, v, empty, true, empty, _N, _K, [])).

ut("naming NO param passing call location  incr 2",
   (   t_e_to_n_e1(app(incr@l:(i->i), [2@l:i]), l, i, v,
                   node(incr,incr:(x:i->ret_incr:i),0,empty,empty),
                   app(incr@l:_:(A:i->B:i), [2@l:A:i])@l:B:i),
       % NO formals are different
       A\=B)).

/*
(
 app:app_v:(g:(a_g:i -> b_g:i) -> f1_app:(ba_app_v:i -> v:i))
 incr:      g:(a_g:i -> b_g:i)
 2:                                       ba_app_v:i
):v:i
*/
ut("naming FO param passing call location  app incr 2",
   (   t_e_to_n_e1(app(app@l:((i->i)->i->i), [incr@l:(i->i), 2@l:i]), l, i, v,
                   node(incr,incr:(x:i -> ret_incr:i),-1,node(app,app:(g:(a_g:Ta -> b_g:Tb) -> f1_app:(x:Ta -> ret_app:Tb)),0,empty,empty),empty),
                   app(app@l:_:(g:(A:i->B:i)->_:(C:i->D:i)), [incr@l:g:(A:i->B:i), 2@l:C:i])@l:D:i),
       % NO formals are different
       A\=B, A\=C, A\=D, B\=C, B\=D, C\=D)).

/*
(
 app:app_v:(g:(g:(aa_g:i -> ab_g:i) -> f1_app:(ba_g:i -> bb_g:i)) -> f1_app:(x:(a_x:i -> b_x:i) -> ret_app:(bba_app_v:i -> v:i)))
 app:       g:(g:(aa_g:i -> ab_g:i) -> f1_app:(ba_g:i -> bb_g:i))
 incr:                                                                       x:(a_x:i -> b_x:i)
 2:                                                                                                         bba_app_v:i
):v:i
*/
ut("naming SO param passing call location  app app incr 2",
   (   t_e_to_n_e1(app(app@l:(((i->i)->i->i)->(i->i)->i->i), [app@l:((i->i)->i->i), incr@l:(i->i), 2@l:i]), l, i, v,
                   node(incr,incr:(x:i -> ret_incr:i),-1,node(app,app:(g:(a_g:Ta -> b_g:Tb) -> f1_app:(x:Ta -> ret_app:Tb)),0,empty,empty),empty),
                   app(app@l:_:(g:(g:(A:i->B:i)->F1:(C:i->D:i))->F1:(x:(E:i->F:i)->_:(G:i->H:i))), [app@l:g:(g:(A:i->B:i)->F1:(C:i->D:i)), incr@l:x:(E:i->F:i), 2@l:G:i])@l:H:i),
       % NO formals are different
       A\=B, A\=C, A\=D, A\=E, A\=F, A\=G, A\=H,
       B\=C, B\=D, B\=E, B\=F, B\=G, B\=H,
       C\=D, C\=E, C\=F, C\=G, C\=H,
       D\=E, D\=F, D\=G, D\=H,
       E\=F, E\=G, E\=H,
       F\=G, F\=H,
       G\=H)).



% **********************************************************************
% Test

pp(In, Exp) :-
        format_to_codes("~p", [In], Codes),
        format_to_codes(Exp, [], Codes).

run_tests :-
        findall(
                _,
                (
                  ut(Desc, Test),
                  length(Desc, L),
                  format('~s', [Desc]),
                  write(' '),
                  (   for(_, L, 71)
                  do  write('.')
                  ),
                  write(' '),
                  (   Test ->
                      print('PASSED\n')
                  ;   print('FAILED\n')
                  )
                ),
                _).
:- run_tests.
