:- ['osummarizer.pl'].
:- use_module(library(ordsets), [list_to_ord_set/2]).
:- use_module(library(codesio), [format_to_codes/3]).
:- set_prolog_flag(discontiguous_warnings, off).



% **********************************************************************
% Pretty printing of typed expressions

ut("PP typed const 10", pp(10@loc('max.ml', 0, 0, 0, 0, 0, 0):int, "10:int")).
ut("PP typed const \"hola\"", pp("hola"@loc('max.ml', 0, 0, 0, 0, 0, 0):string, "\"hola\":string")).
ut("PP typed const +", pp((+)@loc('max.ml', 0, 0, 0, 0, 0, 0):(int -> int -> int), "(+):(int -> int -> int)")).
ut("PP typed const >", pp((>)@loc('max.ml', 0, 0, 0, 0, 0, 0):(int -> int -> bool), "(>):(int -> int -> bool)")).
ut("PP typed id x", pp(x@loc('max.ml', 0, 0, 0, 0, 0, 0):int, "x:int")).
ut("PP typed id inc", pp(inc@loc('max.ml', 0, 0, 0, 0, 0, 0):(int -> int), "inc:(int -> int)")).
ut("PP typed id max1", pp(max1@loc('max.ml', 0, 0, 0, 0, 0, 0):(int -> int -> int), "max1:(int -> int -> int)")).

ut("PP typed app >", pp(app(
                            '>'@loc('max.ml', 0, 0, 0, 0, 0, 0):(int -> int -> bool),
                            [x@loc('max.ml', 0, 0, 0, 0, 0, 0):int,
                             y@loc('max.ml', 0, 0, 0, 0, 0, 0):int]
                           )@loc('max.ml', 0, 0, 0, 0, 0, 0):bool, "(\n  (>):(int -> int -> bool)\n  x:int\n  y:int\n):bool")).
ut("PP typed abs id", pp(abs([x@loc('id.ml', 0, 0, 0, 0, 0, 0):int],
                             x@loc('id.ml', 0, 0, 0, 0, 0, 0):int
                            )@loc('id.ml', 0, 0, 0, 0, 0, 0):(int -> int), "(fun\n  x:int\n->\n  x:int\n):(int -> int)")).
ut("PP typed abs snd", pp(abs([x@loc('snd.ml', 0, 0, 0, 0, 0, 0):int, y@loc('snd.ml', 0, 0, 0, 0, 0, 0):int],
                              y@loc('snd.ml', 0, 0, 0, 0, 0, 0):int
                             )@loc('snd.ml', 0, 0, 0, 0, 0, 0):(int -> int -> int), "(fun\n  x:int\n  y:int\n->\n  y:int\n):(int -> int -> int)")).
ut("PP typed ite", pp(ite(true@loc('max.ml', 0, 0, 0, 0, 0, 0):bool,
                          x@loc('max.ml', 0, 0, 0, 0, 0, 0):int,
                          y@loc('max.ml', 0, 0, 0, 0, 0, 0):int
                         )@loc('max.ml', 0, 0, 0, 0, 0, 0):int, "(if\n  true:bool\nthen\n  x:int\nelse\n  y:int\n):int")).
ut("PP typed let", pp(let(x@loc('max.ml', 0, 0, 0, 0, 0, 0):int,
                          1@loc('max.ml', 0, 0, 0, 0, 0, 0):int,
                          x@loc('max.ml', 0, 0, 0, 0, 0, 0):int
                         )@loc('max.ml', 0, 0, 0, 0, 0, 0):int, "(let\n  x:int\n=\n  1:int\nin\n  x:int\n):int")).
ut("PP typed assert true", pp(assert(
                                     true@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):bool
                                    )@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):unit, "(assert(\n  true:bool\n)):unit")).
ut("PP typed assert gt", pp(assert(
                                   app(
                                       '>'@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):(int -> int -> bool),
                                       [x@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):int,
                                        y@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):int]
                                      )@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):bool
                                  )@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):unit, "(assert\n  (\n    (>):(int -> int -> bool)\n    x:int\n    y:int\n  ):bool\n):unit")).
ut("PP typed assume true", pp(assume(
                                     true@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):bool
                                    )@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):unit, "(assume(\n  true:bool\n)):unit")).
ut("PP typed assume gt", pp(assume(
                                   app(
                                       '>'@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):(int -> int -> bool),
                                       [x@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):int,
                                        y@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):int]
                                      )@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):bool
                                  )@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):unit, "(assume\n  (\n    (>):(int -> int -> bool)\n    x:int\n    y:int\n  ):bool\n):unit")).
ut("PP typed max int", pp(let('max1'@loc('max.ml', 0, 0, 0, 0, 0, 0):(int -> int -> int),
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
      "(let\n  max1:(int -> int -> int)\n=\n  (fun\n    x2:int\n    y3:int\n  ->\n    (if\n      (\n        (>):(int -> int -> bool)\n        x2:int\n        y3:int\n      ):bool\n    then\n      x2:int\n    else\n      y3:int\n    ):int\n  ):(int -> int -> int)\nin\n  (\n    max1:(int -> int -> int)\n    3:int\n    1:int\n  ):int\n):int")).
% ut("PP typed max", false).



% **********************************************************************
% Pretty printing of named expressions


ut("PP named const 10", pp(10@loc('max.ml', 0, 0, 0, 0, 0, 0):r:int, "10:r:int")).
ut("PP named const \"hola\"", pp("hola"@loc('max.ml', 0, 0, 0, 0, 0, 0):str:string, "\"hola\":str:string")).
ut("PP named const +", pp((+)@loc('max.ml', 0, 0, 0, 0, 0, 0):'plus_r':('a_plus_r':int -> 'b_plus_r':('ba_plus_r':int -> 'bb_plus_r':int)),
       "(+):plus_r:(a_plus_r:int -> b_plus_r:(ba_plus_r:int -> bb_plus_r:int))")).
ut("PP named const >", pp((>)@loc('max.ml', 0, 0, 0, 0, 0, 0):'gt_c_ret_max1':('a_gt_c_ret_max1':int -> 'b_gt_c_ret_max1':('ba_gt_c_ret_max1':int -> 'bb_gt_c_ret_max1':bool)),
       "(>):gt_c_ret_max1:(a_gt_c_ret_max1:int -> b_gt_c_ret_max1:(ba_gt_c_ret_max1:int -> bb_gt_c_ret_max1:bool))")).
ut("PP named id x", pp(x@loc('max.ml', 0, 0, 0, 0, 0, 0):r:int, "x:r:int")).
ut("PP named id inc", pp(inc@loc('max.ml', 0, 0, 0, 0, 0, 0):inc:(a_inc_x:int -> b_inc_x:int), "inc:inc:(a_inc_x:int -> b_inc_x:int)")).
ut("PP named id max1", pp(max1@loc('max.ml', 0, 0, 0, 0, 0, 0):max1:(a_max1_v:int -> f1_max1:(ba_max1_v:int -> bb_max1_v:int)),
       "max1:max1:(a_max1_v:int -> f1_max1:(ba_max1_v:int -> bb_max1_v:int))")).
ut("PP named app +", pp(app(
                             '+'@loc('max.ml', 0, 0, 0, 0, 0, 0):'plus_ret':('a_plus_ret':int -> 'b_plus_ret':('ba_plus_ret':int -> ret:int)),
                             [1@loc('max.ml', 0, 0, 0, 0, 0, 0):'a_plus_ret':int,
                              2@loc('max.ml', 0, 0, 0, 0, 0, 0):'ba_plus_ret':int]
                            )@loc('max.ml', 0, 0, 0, 0, 0, 0):ret:int,
       "(\n  (+):plus_ret:(a_plus_ret:int -> b_plus_ret:(ba_plus_ret:int -> ret:int))\n  1:a_plus_ret:int\n  2:ba_plus_ret:int\n):ret:int" )).
ut("PP named app >", pp(app(
                             '>'@loc('max.ml', 0, 0, 0, 0, 0, 0):'gt_c_ret_max1':('a_gt_c_ret_max1':int -> 'b_gt_c_ret_max1':('ba_gt_c_ret_max1':int -> c_ret_max1:bool)),
                             [x2@loc('max.ml', 0, 0, 0, 0, 0, 0):'a_gt_c_ret_max1':int,
                              y3@loc('max.ml', 0, 0, 0, 0, 0, 0):'ba_gt_c_ret_max1':int]
                            )@loc('max.ml', 0, 0, 0, 0, 0, 0):c_ret_max1:bool,
       "(\n  (>):gt_c_ret_max1:(a_gt_c_ret_max1:int -> b_gt_c_ret_max1:(ba_gt_c_ret_max1:int -> c_ret_max1:bool))\n  x2:a_gt_c_ret_max1:int\n  y3:ba_gt_c_ret_max1:int\n):c_ret_max1:bool")).
ut("PP named abs id", pp(abs([x@loc('id.ml', 0, 0, 0, 0, 0, 0):x:int],
                              x@loc('id.ml', 0, 0, 0, 0, 0, 0):ret_id:int
                             )@loc('id.ml', 0, 0, 0, 0, 0, 0):id:(x:int -> ret_id:int),
       "(fun\n  x:x:int\n->\n  x:ret_id:int\n):id:(x:int -> ret_id:int)")).
ut("PP named abs snd", pp(abs([x@loc('snd.ml', 0, 0, 0, 0, 0, 0):x:int, y@loc('snd.ml', 0, 0, 0, 0, 0, 0):y:int],
                               y@loc('snd.ml', 0, 0, 0, 0, 0, 0):ret_snd:int
                              )@loc('snd.ml', 0, 0, 0, 0, 0, 0):snd:(x:int -> f1_snd:(y:int -> ret_snd:int)),
       "(fun\n  x:x:int\n  y:y:int\n->\n  y:ret_snd:int\n):snd:(x:int -> f1_snd:(y:int -> ret_snd:int))")).
ut("PP named ite", pp(ite(true@loc('max.ml', 0, 0, 0, 0, 0, 0):c_ret:bool,
                           x@loc('max.ml', 0, 0, 0, 0, 0, 0):ret:int,
                           y@loc('max.ml', 0, 0, 0, 0, 0, 0):ret:int
                          )@loc('max.ml', 0, 0, 0, 0, 0, 0):ret:int,
       "(if\n  true:c_ret:bool\nthen\n  x:ret:int\nelse\n  y:ret:int\n):ret:int")).
ut("PP named let", pp(let(x@loc('max.ml', 0, 0, 0, 0, 0, 0):x:int,
                           1@loc('max.ml', 0, 0, 0, 0, 0, 0):x:int,
                           x@loc('max.ml', 0, 0, 0, 0, 0, 0):v:int
                          )@loc('max.ml', 0, 0, 0, 0, 0, 0):v:int,
       "(let\n  x:x:int\n=\n  1:x:int\nin\n  x:v:int\n):v:int")).
ut("PP named assert true", pp(assert(
                                      true@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):ase_ret_f1:bool
                                     )@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):ret_f1:unit,
       "(assert(\n  true:ase_ret_f1:bool\n)):ret_f1:unit")).
ut("PP named assert gt", pp(assert(app('>'@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):gt_ase_ret_f1:(a_gt_ase_ret_f1:int -> b_gt_ase_ret_f1:(ba_gt_ase_ret_f1:int -> ase_ret_f1:bool)),
                                        ['x2'@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):a_gt_ase_ret_f1:int,
                                         0@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):ba_gt_ase_ret_f1:int]
                                       )@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):ase_ret_f1:bool
                                   )@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):ret_f1:unit,
       "(assert\n  (\n    (>):gt_ase_ret_f1:(a_gt_ase_ret_f1:int -> b_gt_ase_ret_f1:(ba_gt_ase_ret_f1:int -> ase_ret_f1:bool))\n    x2:a_gt_ase_ret_f1:int\n    0:ba_gt_ase_ret_f1:int\n  ):ase_ret_f1:bool\n):ret_f1:unit")).
ut("PP named assume true", pp(assume(
                                      true@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):asu__3:bool
                                     )@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):'_3':unit,
       "(assume(\n  true:asu__3:bool\n)):_3:unit")).
ut("PP named assume gt", pp(assume(
                                    app('>'@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):gt_asu__3:(a_gt_asu__3:int -> b_gt_asu__3:(ba_gt_asu__3:int -> asu__3:bool)),
                                        ['x2'@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):a_gt_asu__3:int,
                                         1@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):ba_gt_asu__3:int]
                                       )@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):asu__3:bool
                                   )@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):'_3':unit,
       "(assume\n  (\n    (>):gt_asu__3:(a_gt_asu__3:int -> b_gt_asu__3:(ba_gt_asu__3:int -> asu__3:bool))\n    x2:a_gt_asu__3:int\n    1:ba_gt_asu__3:int\n  ):asu__3:bool\n):_3:unit")).
ut("PP named max int", pp(let('max1'@loc('max.ml', 0, 0, 0, 0, 0, 0):max1:(x2:int -> f1_max1:(y3:int -> ret_max1:int)),
                               abs(['x2'@loc('max.ml', 0, 0, 0, 0, 0, 0):x2:int,
                                    'y3'@loc('max.ml', 0, 0, 0, 0, 0, 0):y3:int],
                                   ite(app('>'@loc('max.ml', 0, 0, 0, 0, 0, 0):'gt_c_ret_max1':('a_gt_c_ret_max1':int -> 'b_gt_c_ret_max1':('ba_gt_c_ret_max1':int -> c_ret_max1:bool)),
                                           ['x2'@loc('max.ml', 0, 0, 0, 0, 0, 0):'a_gt_c_ret_max1':int,
                                            'y3'@loc('max.ml', 0, 0, 0, 0, 0, 0):'ba_gt_c_ret_max1':int]
                                           )@loc('max.ml', 0, 0, 0, 0, 0, 0):c_ret_max1:bool,
                                       'x2'@loc('max.ml', 0, 0, 0, 0, 0, 0):ret_max1:int,
                                       'y3'@loc('max.ml', 0, 0, 0, 0, 0, 0):ret_max1:int
                                       )@loc('max.ml', 0, 0, 0, 0, 0, 0):ret_max1:int
                                  )@loc('max.ml', 0, 0, 0, 0, 0, 0):max1:(x2:int -> f1_max1:(y3:int -> ret_max1:int)),
                               app('max1'@loc('max.ml', 0, 0, 0, 0, 0, 0):max1:(a_max1_v:int -> f1_max1:(ba_max1_v:int -> v:int)),
                                   [3@loc('max.ml', 0, 0, 0, 0, 0, 0):a_max1_v:int,
                                    1@loc('max.ml', 0, 0, 0, 0, 0, 0):ba_max1_v:int]
                                  )@loc('max.ml', 0, 0, 0, 0, 0, 0):v:int
                              )@loc('max.ml', 0, 0, 0, 0, 0, 0):v:int,
       "(let\n  max1:max1:(x2:int -> f1_max1:(y3:int -> ret_max1:int))\n=\n  (fun\n    x2:x2:int\n    y3:y3:int\n  ->\n    (if\n      (\n        (>):gt_c_ret_max1:(a_gt_c_ret_max1:int -> b_gt_c_ret_max1:(ba_gt_c_ret_max1:int -> c_ret_max1:bool))\n        x2:a_gt_c_ret_max1:int\n        y3:ba_gt_c_ret_max1:int\n      ):c_ret_max1:bool\n    then\n      x2:ret_max1:int\n    else\n      y3:ret_max1:int\n    ):ret_max1:int\n  ):max1:(x2:int -> f1_max1:(y3:int -> ret_max1:int))\nin\n  (\n    max1:max1:(a_max1_v:int -> f1_max1:(ba_max1_v:int -> v:int))\n    3:a_max1_v:int\n    1:ba_max1_v:int\n  ):v:int\n):v:int")).
% ut("PP named max", false).



% **********************************************************************
% Pretty printing of constraints

ut("PP cstr 'id1_int->int'", pp(('id1_int->int'('X2', 'RET_ID1'):-('RET_ID1'='X2', 'ctx_id1_int->int'('X2'))),
                                "'id1_int->int'(X2, RET_ID1) :- RET_ID1=X2, 'ctx_id1_int->int'(X2).")).
ut("PP cstr 'ctx_id1_int->int'", pp(('ctx_id1_int->int'('A_ID1_V') :- 'A_ID1_V'=3),
                                    "'ctx_id1_int->int'(A_ID1_V) :- A_ID1_V=3.")).
ut("PP cstr 'max1_int->int->int'", pp(('max1_int->int->int'('X2', 'Y3', 'RET_MAX1') :- (('X2'>'Y3', 'RET_MAX1'='X2' ; \+'X2'>'Y3', 'RET_MAX1'='Y3'), 'ctx_max1_int->int->int'('X2', 'Y3'))),
                                      "'max1_int->int->int'(X2, Y3, RET_MAX1) :- (X2>Y3, RET_MAX1=X2 ; \\+X2>Y3, RET_MAX1=Y3), 'ctx_max1_int->int->int'(X2, Y3).")).
ut("PP cstr 'ctx_max1_int->int->int'", pp(('ctx_max1_int->int->int'('A_MAX1_V', 'BA_MAX1_V') :- ('A_MAX1_V'=3, 'BA_MAX1_V'=1)),
                                          "'ctx_max1_int->int->int'(A_MAX1_V, BA_MAX1_V) :- A_MAX1_V=3, BA_MAX1_V=1.")).
ut("PP cstr 'snd_int->int->int'", pp(('snd_int->int->int'('X','Y','RET_SND') :- ('RET_SND'='Y', 'ctx_snd_int->int->int'('X','Y'))),
                                     "'snd_int->int->int'(X, Y, RET_SND) :- RET_SND=Y, 'ctx_snd_int->int->int'(X, Y).")).
ut("PP cstr assert", pp(('X2'>0 :- ('X2'>1, 'ctx_f1_int->unit'('X2'))),
                        "X2>0 :- X2>1, 'ctx_f1_int->unit'(X2).")).



% **********************************************************************
% Well-formedness of typed expressions

ut("WF const true", wf_typed_exp(true@loc('max.ml', 0, 0, 0, 0, 0, 0):bool)).
ut("WF const 10", wf_typed_exp('10'@loc('max.ml', 0, 0, 0, 0, 0, 0):int)).
ut("WF const \"hola\"", wf_typed_exp("hola"@loc('max.ml', 0, 0, 0, 0, 0, 0):string)).
ut("WF const +", wf_typed_exp('+'@loc('max.ml', 0, 0, 0, 0, 0, 0):(int -> int -> int))).
ut("WF const >", wf_typed_exp('>'@loc('max.ml', 0, 0, 0, 0, 0, 0):(int -> int -> bool))).
ut("WF id x", wf_typed_exp(x@loc('max.ml', 0, 0, 0, 0, 0, 0):int)).
ut("WF id inc", wf_typed_exp(inc@loc('max.ml', 0, 0, 0, 0, 0, 0):(int -> int))).
ut("WF id max1", wf_typed_exp(max1@loc('max.ml', 0, 0, 0, 0, 0, 0):(int -> int -> int))).
ut("WF app >", wf_typed_exp(
                     app(
                         '>'@loc('max.ml', 0, 0, 0, 0, 0, 0):(int -> int -> bool),
                         [x@loc('max.ml', 0, 0, 0, 0, 0, 0):int,
                          y@loc('max.ml', 0, 0, 0, 0, 0, 0):int]
                        )@loc('max.ml', 0, 0, 0, 0, 0, 0):bool
                    )).
ut("WF abs id", wf_typed_exp(
                     abs([x@loc('id.ml', 0, 0, 0, 0, 0, 0):int],
                         x@loc('id.ml', 0, 0, 0, 0, 0, 0):int
                         )@loc('id.ml', 0, 0, 0, 0, 0, 0):(int -> int)
                    )).
ut("WF abs snd", wf_typed_exp( abs([x@loc('snd.ml', 0, 0, 0, 0, 0, 0):int, y@loc('snd.ml', 0, 0, 0, 0, 0, 0):int],
                                    x@loc('snd.ml', 0, 0, 0, 0, 0, 0):int
                                   )@loc('snd.ml', 0, 0, 0, 0, 0, 0):(int -> int -> int) )).
ut("WF ite", wf_typed_exp( ite(true@loc('max.ml', 0, 0, 0, 0, 0, 0):bool,
                                    x@loc('max.ml', 0, 0, 0, 0, 0, 0):int,
                                    y@loc('max.ml', 0, 0, 0, 0, 0, 0):int
                                   )@loc('max.ml', 0, 0, 0, 0, 0, 0):int )).
ut("WF let", wf_typed_exp( let(x@loc('max.ml', 0, 0, 0, 0, 0, 0):int,
                                    1@loc('max.ml', 0, 0, 0, 0, 0, 0):int,
                                    x@loc('max.ml', 0, 0, 0, 0, 0, 0):int
                                   )@loc('max.ml', 0, 0, 0, 0, 0, 0):int )).
ut("WF assert", wf_typed_exp( assert(true@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):bool
                                      )@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):unit )).
ut("WF assume", wf_typed_exp( assume(true@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):bool
                                      )@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):unit )).
ut("WF max int", wf_typed_exp( let('max1'@loc('max.ml', 0, 0, 0, 0, 0, 0):(int -> int -> int),
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
                                   )@loc('max.ml', 0, 0, 0, 0, 0, 0):int )).
% ut("WF max", false).
ut("Negative WF app no param", \+ wf_typed_exp( app(
                                       '>'@loc('max.ml', 0, 0, 0, 0, 0, 0):(int -> int -> bool),
                                       []
                                      )@loc('max.ml', 0, 0, 0, 0, 0, 0):bool )).
ut("Negative WF abs no param", \+ wf_typed_exp( abs([],
                                       x@loc('id.ml', 0, 0, 0, 0, 0, 0):int
                                      )@loc('id.ml', 0, 0, 0, 0, 0, 0):(int -> int) )).
ut("Negative WF assert 1", \+ wf_typed_exp( assert(true@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):bool
                                      )@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):bool )).
ut("Negative WF assert 2", \+ wf_typed_exp( assert(true@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):bool
                                      )@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):int )).
ut("Negative WF assert 3", \+ wf_typed_exp( assert(true@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):bool
                                      )@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):_ )).
ut("Negative WF assume 1", \+ wf_typed_exp( assume(true@loc('assume_assume.ml', 0, 0, 0, 0, 0, 0):bool
                                      )@loc('assume_assume.ml', 0, 0, 0, 0, 0, 0):bool )).
ut("Negative WF assume 2", \+ wf_typed_exp( assume(true@loc('assume_assume.ml', 0, 0, 0, 0, 0, 0):bool
                                      )@loc('assume_assume.ml', 0, 0, 0, 0, 0, 0):int )).
ut("Negative WF assume 3", \+ wf_typed_exp( assume(true@loc('assume_assume.ml', 0, 0, 0, 0, 0, 0):bool
                                      )@loc('assume_assume.ml', 0, 0, 0, 0, 0, 0):_ )).



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
ut("formals 1", formals(f:(a_f:(aa_f:i -> ab_f:i) -> b_f:(ba_f:(baa_f:i -> bab_f:i) -> bb_f:i)),
                          [a_f:(aa_f:i -> ab_f:i),        ba_f:(baa_f:i -> bab_f:i)])).
ut("formals 2", formals(f:(g:(x:int -> y:bool) -> h:(z:int -> u:bool)),
                          [g:(x:int -> y:bool),      z:int] )).
ut("rename_return 1", rename_return(1, w, f:(g:(x:int -> y:bool) -> w:(z:int -> u:bool)),
                                          f:(g:(x:int -> y:bool) -> w:(z:int -> u:bool)) )).
ut("rename_return 2", rename_return(2, w, f:(g:(x:int -> y:bool) -> h:(z:int -> u:bool)),
                                          f:(g:(x:int -> y:bool) -> h:(z:int -> w:bool)) )).
ut("rename_return 3", rename_return(2, w, f:(a_f:(aa_f:i -> ab_f:i) -> b_f:(ba_f:(baa_f:i -> bab_f:i) -> bb_f:i)),
                                          f:(a_f:(aa_f:i -> ab_f:i) -> b_f:(ba_f:(baa_f:i -> bab_f:i) -> w   :i)) )).
ut("rename_return 4", rename_return(1, w, f:(a_f:(aa_f:i -> ab_f:i) -> b_f:(ba_f:(baa_f:i -> bab_f:i) -> bb_f:i)),
                                          f:(a_f:(aa_f:i -> ab_f:i) -> w  :(ba_f:(baa_f:i -> bab_f:i) -> bb_f:i)) )).
ut("remove_formals 1", remove_formals(1, f:(a_f:(aa_f:i -> ab_f:i) -> b_f:(ba_f:(baa_f:i -> bab_f:i) -> bb_f:i)),
                                                                      b_f:(ba_f:(baa_f:i -> bab_f:i) -> bb_f:i)  )).
ut("remove_formals 2", remove_formals(2, f:(a_f:(aa_f:i -> ab_f:i) -> b_f:(ba_f:(baa_f:i -> bab_f:i) -> bb_f:i)),
                                                                                                        bb_f:i   )).
ut("remove_formals 3", remove_formals(1, f:(g:(x:int -> y:bool) -> h:(z:int -> u:bool)),
                                                                   h:(z:int -> u:bool)  )).
ut("remove_formals 4", remove_formals(2, f:(g:(x:int -> y:bool) -> h:(z:int -> u:bool)),
                                                                               u:bool   )).


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
                                         abs(['x2'@loc('max.ml', 0, 0, 0, 0, 0, 0):x2:int,
                                              'y3'@loc('max.ml', 0, 0, 0, 0, 0, 0):y3:int],
                                             ite(app('>'@loc('max.ml', 0, 0, 0, 0, 0, 0):gt_c_ret_max1:(a_gt_c_ret_max1:int -> b_gt_c_ret_max1:(ba_gt_c_ret_max1:int -> c_ret_max1:bool)),
                                                     ['x2'@loc('max.ml', 0, 0, 0, 0, 0, 0):a_gt_c_ret_max1:int,
                                                      'y3'@loc('max.ml', 0, 0, 0, 0, 0, 0):ba_gt_c_ret_max1:int]
                                                     )@loc('max.ml', 0, 0, 0, 0, 0, 0):c_ret_max1:bool,
                                                 'x2'@loc('max.ml', 0, 0, 0, 0, 0, 0):ret_max1:int,
                                                 'y3'@loc('max.ml', 0, 0, 0, 0, 0, 0):ret_max1:int
                                                 )@loc('max.ml', 0, 0, 0, 0, 0, 0):ret_max1:int
                                            )@loc('max.ml', 0, 0, 0, 0, 0, 0):max1:(x2:int -> f1_max1:(y3:int -> ret_max1:int)),
                                         app('max1'@loc('max.ml', 0, 0, 0, 0, 0, 0):max1_v:(a_max1_v:int -> f1_max1:(ba_max1_v:int -> v:int)),
                                             [3@loc('max.ml', 0, 0, 0, 0, 0, 0):a_max1_v:int,
                                              1@loc('max.ml', 0, 0, 0, 0, 0, 0):ba_max1_v:int]
                                            )@loc('max.ml', 0, 0, 0, 0, 0, 0):v:int
                                        )@loc('max.ml', 0, 0, 0, 0, 0, 0):v:int) )).
% ut("naming max", false).



% % **********************************************************************
% % Summarization of named expressions

% ut("remove_true 1", remove_true(('X2'>1,'ctx_f1_int->unit'('X2'),true), ('X2'>1,'ctx_f1_int->unit'('X2'))) ).
% ut("remove_true 2", remove_true(('X2'>1,true,'ctx_f1_int->unit'('X2')), ('X2'>1,'ctx_f1_int->unit'('X2'))) ).
% ut("remove_true 3", remove_true((true,'X2'>1,'ctx_f1_int->unit'('X2')), ('X2'>1,'ctx_f1_int->unit'('X2'))) ).
% ut("return 1", return(f:(a_f:(aa_f:i -> ab_f:i) -> b_f:(ba_f:(baa_f:i -> bab_f:i) -> bb_f:i)), bb_f:i) ).
% ut("return 2", return(f:(g:(x:int -> y:bool) -> h:(z:int -> u:bool)), u:bool) ).
% ut("formals_return 1", formals_return(f:(a_f:(aa_f:i -> ab_f:i) -> b_f:(ba_f:(baa_f:i -> bab_f:i) -> bb_f:i)),
%                                         [a_f:(aa_f:i -> ab_f:i),        ba_f:(baa_f:i -> bab_f:i),   bb_f:i]  )).
% ut("formals_return 2", formals_return(f:(g:(x:int -> y:bool) -> h:(z:int -> u:bool)),
%                                         [g:(x:int -> y:bool),      z:int,   u:bool]  )).
% ut("uppercase_atom 1", uppercase_atom(str, 'STR') ).
% ut("unname_type 1", unname_type(v1:(v11:(v111:i->v112:i)->v12:(v121:(v1211:i->v1212:i)->v122:i)),
%                                    (    (     i->     i)->          (      i->      i)->     i ) )).
% ut("summarizing const true", n_e_to_c1(true, loc('max.ml', 0, 0, 0, 0, 0, 0), r:bool, empty, true, true, []) ).
% ut("summarizing const 10", n_e_to_c1(10, loc('max.ml', 0, 0, 0, 0, 0, 0), r:int, empty, true, 'R'=10, []) ).
% ut("summarizing const \"hola\"", n_e_to_c1("hola", loc('max.ml', 0, 0, 0, 0, 0, 0), str:string, empty, true, 'STR'="hola", []) ).
% ut("summarizing const +", n_e_to_c1('+', loc('max.ml', 0, 0, 0, 0, 0, 0),
%                                     'plus_r':('a_plus_r':int -> 'b_plus_r':('ba_plus_r':int -> 'bb_plus_r':int)),
%                                     empty, true, ('BB_PLUS_R' = 'A_PLUS_R' + 'BA_PLUS_R'), []) ).
% ut("summarizing const >", n_e_to_c1('>', loc('max.ml', 0, 0, 0, 0, 0, 0),
%                                     'gt_c_ret_max1':('a_gt_c_ret_max1':int -> 'b_gt_c_ret_max1':('ba_gt_c_ret_max1':int -> 'bb_gt_c_ret_max1':bool)),
%                                     empty, true, ('A_GT_C_RET_MAX1' > 'BA_GT_C_RET_MAX1'), []) ).
% ut("summarizing id x", n_e_to_c1(x, loc('max.ml', 0, 0, 0, 0, 0, 0), r:int, empty, true, ('R'='X'), []) ).
% ut("summarizing id inc", n_e_to_c1(inc, loc('max.ml', 0, 0, 0, 0, 0, 0),
%                                    inc:(a_inc_x:int -> b_inc_x:int), empty, true, 'inc_int->int'('A_INC_X', 'B_INC_X'), []) ).
% ut("summarizing id pos", n_e_to_c1(pos, loc('max.ml', 0, 0, 0, 0, 0, 0), pos:(a_pos_x:int -> b_pos_x:bool), empty, true, 'pos_int->bool'('A_POS_X'), []) ).
% ut("summarizing id max1", n_e_to_c1(max1, loc('max.ml', 0, 0, 0, 0, 0, 0), max1:(a_max1_v:int -> f1_max1:(ba_max1_v:int -> bb_max1_v:int)),
%                                     empty, true, 'max1_int->int->int'('A_MAX1_V', 'BA_MAX1_V', 'BB_MAX1_V'), []) ).
% ut("summarizing app >", (   E@L:N = app(
%                                         '>'@loc('max.ml', 0, 0, 0, 0, 0, 0):'gt_c_ret_max1':('a_gt_c_ret_max1':int -> 'b_gt_c_ret_max1':('ba_gt_c_ret_max1':int -> c_ret_max1:bool)),
%                                         [x2@loc('max.ml', 0, 0, 0, 0, 0, 0):'a_gt_c_ret_max1':int,
%                                          y3@loc('max.ml', 0, 0, 0, 0, 0, 0):'ba_gt_c_ret_max1':int]
%                                        )@loc('max.ml', 0, 0, 0, 0, 0, 0):c_ret_max1:bool,
%                             n_e_to_c1(E, L, N, empty, true, ('X2' > 'Y3'), []) )).
% ut("summarizing app Obj.magic",
%                   (   E@L:N = app('Obj.magic'@loc('assume_assert.ml',0,0,0,0,0,0):magic_a_f1_v:(a_magic_a_f1_v:unit->a_f1_v:int),
%                                   [unit@loc('assume_assert.ml',0,0,0,0,0,0):a_magic_a_f1_v:unit]
%                                  )@loc('assume_assert.ml',0,0,0,0,0,0):a_f1_v:int,
%                       n_e_to_c1(E, L, N, empty, true, 'A_F1_V'='_', []) )).
% ut("summarizing app max1",
%                   (   E@L:N = app('max1'@loc('max.ml', 0, 0, 0, 0, 0, 0):max1:(a_max1_v:int -> f1_max1:(ba_max1_v:int -> v:int)),
%                                    [3@loc('max.ml', 0, 0, 0, 0, 0, 0):a_max1_v:int,
%                                     1@loc('max.ml', 0, 0, 0, 0, 0, 0):ba_max1_v:int]
%                                   )@loc('max.ml', 0, 0, 0, 0, 0, 0):v:int,
%                       n_e_to_c1(E, L, N, empty, true, 'max1_int->int->int'(3, 1, 'V'), S),
%                       S == [('ctx_max1_int->int->int'('A_MAX1_V', 'BA_MAX1_V') :- 'A_MAX1_V'=3, 'BA_MAX1_V'=1)] )).
% ut("summarizing abs id",
%                   (   E@L:N = abs([x2@loc('id.ml', 0, 0, 0, 0, 0, 0):x2:int],
%                                   x2@loc('id.ml', 0, 0, 0, 0, 0, 0):ret_id1:int
%                                  )@loc('id.ml', 0, 0, 0, 0, 0, 0):id1:(x2:int -> ret_id1:int),
%                       n_e_to_c1(E, L, N, empty, true, true, S),
%                       S == [('id1_int->int'('X2', 'RET_ID1') :- 'RET_ID1'='X2', 'ctx_id1_int->int'('X2')) ] )).
% ut("summarizing abs snd",
%                   (   E@L:N = abs([x@loc('snd.ml', 0, 0, 0, 0, 0, 0):x:int, y@loc('snd.ml', 0, 0, 0, 0, 0, 0):y:int],
%                                   y@loc('snd.ml', 0, 0, 0, 0, 0, 0):ret_snd:int
%                                  )@loc('snd.ml', 0, 0, 0, 0, 0, 0):snd:(x:int -> f1_snd:(y:int -> ret_snd:int)),
%                       n_e_to_c1(E, L, N, empty, true, true, S),
%                       S == [('snd_int->int->int'('X','Y','RET_SND') :- 'RET_SND'='Y', 'ctx_snd_int->int->int'('X','Y'))] )).
% ut("summarizing ite nullary",
%                   (   E@L:N = ite(true@loc('max.ml', 0, 0, 0, 0, 0, 0):c_ret:bool,
%                                x@loc('max.ml', 0, 0, 0, 0, 0, 0):ret:int,
%                                y@loc('max.ml', 0, 0, 0, 0, 0, 0):ret:int
%                               )@loc('max.ml', 0, 0, 0, 0, 0, 0):ret:int,
%                       n_e_to_c1(E, L, N, empty, true, (true -> 'RET'='X'; 'RET'='Y'), []) )).
% ut("summarizing ite function",
%                   false ).
% ut("summarizing let nullary",
%                   (   E@L:N = let(x@loc('max.ml', 0, 0, 0, 0, 0, 0):x:int,
%                                   1@loc('max.ml', 0, 0, 0, 0, 0, 0):x:int,
%                                   x@loc('max.ml', 0, 0, 0, 0, 0, 0):v:int
%                                  )@loc('max.ml', 0, 0, 0, 0, 0, 0):v:int,
%                       n_e_to_c1(E, L, N, empty, true, ('V'='X', 'X'=1), []) )).
% ut("summarizing let function",
%                   (   E@L:N = let('id1'@loc('id.ml', 0, 0, 0, 0, 0, 0):id1:(x2:int -> ret_id1:int),
%                                   abs(['x2'@loc('id.ml', 0, 0, 0, 0, 0, 0):x2:int],
%                                       'x2'@loc('id.ml', 0, 0, 0, 0, 0, 0):ret_id1:int
%                                       )@loc('id.ml', 0, 0, 0, 0, 0, 0):id1:(x2:int -> ret_id1:int),
%                                   app('id1'@loc('id.ml', 0, 0, 0, 0, 0, 0):id1:(a_id1_v:int -> v:int),
%                                       [3@loc('id.ml', 0, 0, 0, 0, 0, 0):a_id1_v:int]
%                                      )@loc('id.ml', 0, 0, 0, 0, 0, 0):v:int
%                                  )@loc('id.ml', 0, 0, 0, 0, 0, 0):v:int,
%                       n_e_to_c1(E, L, N, empty, true, 'id1_int->int'(3, 'V'), S),
%                       S == [('ctx_id1_int->int'('A_ID1_V') :- 'A_ID1_V'=3),
%                             ('id1_int->int'('X2','RET_ID1'):- 'RET_ID1'='X2','ctx_id1_int->int'('X2'))] )).
% ut("summarizing let assume-assert",
%                   (   E@L:N = let('_3'@loc('assume_assert.ml',0,0,0,0,0,0):'_3':unit,
%                                           assume(app(> @loc('assume_assert.ml',0,0,0,0,0,0):gt_asu__3:(a_gt_asu__3:int->b_gt_asu__3:(ba_gt_asu__3:int->asu__3:bool)),
%                                                      [x2@loc('assume_assert.ml',0,0,0,0,0,0):a_gt_asu__3:int,
%                                                       1@loc('assume_assert.ml',0,0,0,0,0,0):ba_gt_asu__3:int
%                                                      ]
%                                                     )@loc('assume_assert.ml',0,0,0,0,0,0):asu__3:bool
%                                                 )@loc('assume_assert.ml',0,0,0,0,0,0):'_3':unit,
%                                           assert(app(> @loc('assume_assert.ml',0,0,0,0,0,0):gt_ase_ret_f1:(a_gt_ase_ret_f1:int->b_gt_ase_ret_f1:(ba_gt_ase_ret_f1:int->ase_ret_f1:bool)),
%                                                      [x2@loc('assume_assert.ml',0,0,0,0,0,0):a_gt_ase_ret_f1:int,
%                                                       0@loc('assume_assert.ml',0,0,0,0,0,0):ba_gt_ase_ret_f1:int
%                                                      ]
%                                                     )@loc('assume_assert.ml',0,0,0,0,0,0):ase_ret_f1:bool
%                                                 )@loc('assume_assert.ml',0,0,0,0,0,0):ret_f1:unit
%                                          )@loc('assume_assert.ml',0,0,0,0,0,0):ret_f1:unit,
%                       n_e_to_c1(E, L, N, empty, 'ctx_f1_int->unit'('X2'), ('X2'>0,'X2'>1), [('X2'>0 :- ('X2'>1, 'ctx_f1_int->unit'('X2')))]) )).
% ut("summarizing assert true",
%                   (   E@L:N = assert(
%                                      true@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):ase_ret_f1:bool
%                                     )@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):ret_f1:unit,
%                        n_e_to_c1(E, L, N, empty, true, true, [ (true :- true) ]) )).
% ut("summarizing assert gt",
%                   (   E@L:N = assert(app('>'@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):gt_ase_ret_f1:(a_gt_ase_ret_f1:int -> b_gt_ase_ret_f1:(ba_gt_ase_ret_f1:int -> ase_ret_f1:bool)),
%                                       ['x2'@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):a_gt_ase_ret_f1:int,
%                                        0@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):ba_gt_ase_ret_f1:int
%                                       ]
%                                      )@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):ase_ret_f1:bool
%                                  )@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):ret_f1:unit,
%                       n_e_to_c1(E, L, N, empty, ('X2'>1, 'ctx_f1_int->unit'('X2')), 'X2'>0, [('X2'>0 :- ('X2'>1, 'ctx_f1_int->unit'('X2')))]) )).
% ut("summarizing assume true",
%                   (   E@L:N = assume(
%                                      true@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):asu_3:bool
%                                     )@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):'_3':unit,
%                        n_e_to_c1(E, L, N, empty, true, true, []) )).
% ut("summarizing assume gt",
%                   (   E@L:N = assume(app(> @loc('assume_assert.ml',0,0,0,0,0,0):gt_asu__3:(a_gt_asu__3:int->b_gt_asu__3:(ba_gt_asu__3:int->asu__3:bool)),
%                                          [x2@loc('assume_assert.ml',0,0,0,0,0,0):a_gt_asu__3:int,
%                                           1@loc('assume_assert.ml',0,0,0,0,0,0):ba_gt_asu__3:int
%                                          ]
%                                         )@loc('assume_assert.ml',0,0,0,0,0,0):asu__3:bool
%                                     )@loc('assume_assert.ml',0,0,0,0,0,0):'_3':unit,
%                       n_e_to_c1(E, L, N, empty, true, 'X2'>1, []) )).
% ut("summarizing max int",
%                   (   E@L:N = let('max1'@loc('max.ml', 0, 0, 0, 0, 0, 0):max1:(x2:int -> f1_max1:(y3:int -> ret_max1:int)),
%                                   abs(['x2'@loc('max.ml', 0, 0, 0, 0, 0, 0):x2:int,
%                                        'y3'@loc('max.ml', 0, 0, 0, 0, 0, 0):y3:int],
%                                       ite(app('>'@loc('max.ml', 0, 0, 0, 0, 0, 0):'gt_c_ret_max1':('a_gt_c_ret_max1':int -> 'b_gt_c_ret_max1':('ba_gt_c_ret_max1':int -> c_ret_max1:bool)),
%                                               ['x2'@loc('max.ml', 0, 0, 0, 0, 0, 0):'a_gt_c_ret_max1':int,
%                                                'y3'@loc('max.ml', 0, 0, 0, 0, 0, 0):'ba_gt_c_ret_max1':int]
%                                               )@loc('max.ml', 0, 0, 0, 0, 0, 0):c_ret_max1:bool,
%                                           'x2'@loc('max.ml', 0, 0, 0, 0, 0, 0):ret_max1:int,
%                                           'y3'@loc('max.ml', 0, 0, 0, 0, 0, 0):ret_max1:int
%                                           )@loc('max.ml', 0, 0, 0, 0, 0, 0):ret_max1:int
%                                      )@loc('max.ml', 0, 0, 0, 0, 0, 0):max1:(x2:int -> f1_max1:(y3:int -> ret_max1:int)),
%                                   app('max1'@loc('max.ml', 0, 0, 0, 0, 0, 0):max1:(a_max1_v:int -> f1_max1:(ba_max1_v:int -> v:int)),
%                                       [3@loc('max.ml', 0, 0, 0, 0, 0, 0):a_max1_v:int,
%                                        1@loc('max.ml', 0, 0, 0, 0, 0, 0):ba_max1_v:int]
%                                      )@loc('max.ml', 0, 0, 0, 0, 0, 0):v:int
%                                  )@loc('max.ml', 0, 0, 0, 0, 0, 0):v:int,
%                       n_e_to_c1(E, L, N, empty, true, 'max1_int->int->int'(3, 1, 'V'), S),
%                       S == [('ctx_max1_int->int->int'('A_MAX1_V','BA_MAX1_V'):-'A_MAX1_V'=3,'BA_MAX1_V'=1),
%                             ('max1_int->int->int'('X2','Y3','RET_MAX1'):-('X2'>'Y3','RET_MAX1'='X2';\+'X2'>'Y3','RET_MAX1'='Y3'),'ctx_max1_int->int->int'('X2','Y3'))] )).
% ut("summarizing max",
%                   false ).
% ut("summarizing assume-assert",
%                   (   E@L:N = let(f1@loc('assume_assert.ml',0,0,0,0,0,0):f1:(x2:int->ret_f1:unit),
%                                   abs([x2@loc('assume_assert.ml',0,0,0,0,0,0):x2:int],
%                                       let('_3'@loc('assume_assert.ml',0,0,0,0,0,0):'_3':unit,
%                                           assume(app(> @loc('assume_assert.ml',0,0,0,0,0,0):gt_asu__3:(a_gt_asu__3:int->b_gt_asu__3:(ba_gt_asu__3:int->asu__3:bool)),
%                                                      [x2@loc('assume_assert.ml',0,0,0,0,0,0):a_gt_asu__3:int,
%                                                       1@loc('assume_assert.ml',0,0,0,0,0,0):ba_gt_asu__3:int
%                                                      ]
%                                                     )@loc('assume_assert.ml',0,0,0,0,0,0):asu__3:bool
%                                                 )@loc('assume_assert.ml',0,0,0,0,0,0):'_3':unit,
%                                           assert(app(> @loc('assume_assert.ml',0,0,0,0,0,0):gt_ase_ret_f1:(a_gt_ase_ret_f1:int->b_gt_ase_ret_f1:(ba_gt_ase_ret_f1:int->ase_ret_f1:bool)),
%                                                      [x2@loc('assume_assert.ml',0,0,0,0,0,0):a_gt_ase_ret_f1:int,
%                                                       0@loc('assume_assert.ml',0,0,0,0,0,0):ba_gt_ase_ret_f1:int
%                                                      ]
%                                                     )@loc('assume_assert.ml',0,0,0,0,0,0):ase_ret_f1:bool
%                                                 )@loc('assume_assert.ml',0,0,0,0,0,0):ret_f1:unit
%                                          )@loc('assume_assert.ml',0,0,0,0,0,0):ret_f1:unit
%                                      )@loc('assume_assert.ml',0,0,0,0,0,0):f1:(x2:int->ret_f1:unit),
%                                   app(f1@loc('assume_assert.ml',0,0,0,0,0,0):f1:(a_f1_v:int->v:unit),
%                                       [app('Obj.magic'@loc('assume_assert.ml',0,0,0,0,0,0):magic_a_f1_v:(a_magic_a_f1_v:unit->a_f1_v:int),
%                                            [unit@loc('assume_assert.ml',0,0,0,0,0,0):a_magic_a_f1_v:unit
%                                            ]
%                                           )@loc('assume_assert.ml',0,0,0,0,0,0):a_f1_v:int
%                                       ]
%                                      )@loc('assume_assert.ml',0,0,0,0,0,0):v:unit
%                                  )@loc('assume_assert.ml',0,0,0,0,0,0):v:unit,
%                       n_e_to_c1(E, L, N, empty, true, 'f1_int->unit'('_'), S),
%                       S == [('ctx_f1_int->unit'('A_F1_V') :- 'A_F1_V'='_'),
%                             ('f1_int->unit'('X2') :- ('X2'>0, 'X2'>1, 'ctx_f1_int->unit'('X2'))),
%                             ('X2'>0 :- ('X2'>1, 'ctx_f1_int->unit'('X2')))] )).



% **********************************************************************
% | c                                                      Constant

ut("naming const ()", t_e_to_n_e1(unit, loc('c.ml', 0, 0, 0, 0, 0, 0), unit, v, empty, unit@loc('c.ml', 0, 0, 0, 0, 0, 0):v:unit)).
ut("Negative naming const ()", \+ t_e_to_n_e1(unit, loc('c.ml', 0, 0, 0, 0, 0, 0), unit, v, empty, unit@loc('c.ml', 0, 0, 0, 0, 0, 0):unit:v)).
ut("path   const ()", n_e_to_p_e1(unit, loc('c.ml', 0, 0, 0, 0, 0, 0), v:unit, unit@loc('c.ml', 0, 0, 0, 0, 0, 0):v:unit-->('V'=1))).
ut("Negative path 1 const ()", \+ n_e_to_p_e1(unit, loc('c.ml', 0, 0, 0, 0, 0, 0), v:unit, unit@loc('c.ml', 0, 0, 0, 0, 0, 0):v:unit-->1)).
ut("Negative path 2 const ()", \+ n_e_to_p_e1(unit, loc('c.ml', 0, 0, 0, 0, 0, 0), v:unit, unit@loc('c.ml', 0, 0, 0, 0, 0, 0):v:unit-->true)).
ut("summ   const ()", p_e_to_c1(unit, loc('c.ml', 0, 0, 0, 0, 0, 0), v:unit, true, 'V'=1, [])).
ut("Negative summ   const ()", \+ p_e_to_c1(unit, loc('c.ml', 0, 0, 0, 0, 0, 0), v:unit, true, 'V'=1, [_])).

ut("PP typed const ()", pp(unit@loc('c.ml', 0, 0, 0, 0, 0, 0):unit, "unit:unit")).
ut("PP named const ()", pp(unit@loc('c.ml', 0, 0, 0, 0, 0, 0):v:unit, "unit:v:unit")).
ut("PP path  const ()", pp(unit@loc('c.ml', 0, 0, 0, 0, 0, 0):v:unit-->'V'=1, "unit:v:unit --> V=1")).

ut("naming const true", t_e_to_n_e1(true, loc('c.ml', 0, 0, 0, 0, 0, 0), bool, v, empty, true@loc('c.ml', 0, 0, 0, 0, 0, 0):v:bool)).
ut("path   const true", n_e_to_p_e1(true, loc('c.ml', 0, 0, 0, 0, 0, 0), v:bool, true@loc('c.ml', 0, 0, 0, 0, 0, 0):v:bool-->('V'=1))).
ut("Negative path 1 const true", \+ n_e_to_p_e1(true, loc('c.ml', 0, 0, 0, 0, 0, 0), v:bool, true@loc('c.ml', 0, 0, 0, 0, 0, 0):v:bool-->('V'=true))).
ut("Negative path 2 const true", \+ n_e_to_p_e1(true, loc('c.ml', 0, 0, 0, 0, 0, 0), v:bool, true@loc('c.ml', 0, 0, 0, 0, 0, 0):v:bool-->true)).
ut("Negative path 3 const true", \+ n_e_to_p_e1(true, loc('c.ml', 0, 0, 0, 0, 0, 0), v:bool, true@loc('c.ml', 0, 0, 0, 0, 0, 0):v:bool-->1)).
ut("summ   const true", p_e_to_c1(true, loc('c.ml', 0, 0, 0, 0, 0, 0), v:bool, true, 'V'=1, [])).

ut("PP typed const true", pp(true@loc('c.ml', 0, 0, 0, 0, 0, 0):bool, "true:bool")).
ut("PP named const true", pp(true@loc('c.ml', 0, 0, 0, 0, 0, 0):v:bool, "true:v:bool")).
ut("PP path  const true", pp(true@loc('c.ml', 0, 0, 0, 0, 0, 0):v:bool-->'V'=1, "true:v:bool --> V=1")).

ut("naming const 10", t_e_to_n_e1(10, loc('c.ml', 0, 0, 0, 0, 0, 0), int, v, empty, 10@loc('c.ml', 0, 0, 0, 0, 0, 0):v:int)).
ut("path   const 10", n_e_to_p_e1(10, loc('c.ml', 0, 0, 0, 0, 0, 0), v:int, 10@loc('c.ml', 0, 0, 0, 0, 0, 0):v:int-->('V'=10))).
ut("summ   const 10", p_e_to_c1(10, loc('c.ml', 0, 0, 0, 0, 0, 0), v:int, true, 'V'=10, [])).

ut("naming const \"hola\"", t_e_to_n_e1("hola", loc('c.ml', 0, 0, 0, 0, 0, 0), string, str, empty, "hola"@loc('c.ml', 0, 0, 0, 0, 0, 0):str:string)).
ut("path   const \"hola\"", n_e_to_p_e1("hola", loc('c.ml', 0, 0, 0, 0, 0, 0), str:string, "hola"@loc('c.ml', 0, 0, 0, 0, 0, 0):str:string-->('STR'="hola"))).
ut("summ   const \"hola\"", p_e_to_c1("hola", loc('c.ml', 0, 0, 0, 0, 0, 0), v:int, true, 'STR'="hola", [])).

ut("naming const +", t_e_to_n_e1('+', loc('c.ml', 0, 0, 0, 0, 0, 0), (int -> int -> int), plus_v, empty, '+'@loc('c.ml', 0, 0, 0, 0, 0, 0):plus_v:(a_plus_v:int -> b_plus_v:(ba_plus_v:int -> bb_plus_v:int)))).
ut("path   const +", n_e_to_p_e1('+', loc('c.ml', 0, 0, 0, 0, 0, 0), plus_v:(a_plus_v:int -> b_plus_v:(ba_plus_v:int -> bb_plus_v:int)), '+'@loc('c.ml', 0, 0, 0, 0, 0, 0):plus_v:(a_plus_v:int -> b_plus_v:(ba_plus_v:int -> bb_plus_v:int))-->true)).
ut("summ   const +", p_e_to_c1('+', loc('c.ml', 0, 0, 0, 0, 0, 0), plus_v:(a_plus_v:int -> b_plus_v:(ba_plus_v:int -> bb_plus_v:int)), true, true, [('plus_v_int->int->int'('A_PLUS_V', 'BA_PLUS_V', 'BB_PLUS_V') :- 'BB_PLUS_V'='A_PLUS_V'+'BA_PLUS_V')])).
ut("Negative summ   const +", \+ p_e_to_c1('+', loc('c.ml', 0, 0, 0, 0, 0, 0), v:(a_plus_v:int -> b_plus_v:(ba_plus_v:int -> bb_plus_v:int)), true, true, [])).

ut("PP typed const +", pp('+'@loc('c.ml', 0, 0, 0, 0, 0, 0):(int -> int -> int), "(+):(int -> int -> int)")).
ut("PP named const +", pp('+'@loc('c.ml', 0, 0, 0, 0, 0, 0):plus_v:(a_plus_v:int -> b_plus_v:(ba_plus_v:int -> bb_plus_v:int)), "(+):plus_v:(a_plus_v:int -> b_plus_v:(ba_plus_v:int -> bb_plus_v:int))")).
ut("PP path  const +", pp('+'@loc('c.ml', 0, 0, 0, 0, 0, 0):plus_v:(a_plus_v:int -> b_plus_v:(ba_plus_v:int -> bb_plus_v:int))-->true, "(+):plus_v:(a_plus_v:int -> b_plus_v:(ba_plus_v:int -> bb_plus_v:int)) --> true")).

ut("naming const >", t_e_to_n_e1('>', loc('c.ml', 0, 0, 0, 0, 0, 0), (int -> int -> bool), gt_v, empty, '>'@loc('c.ml', 0, 0, 0, 0, 0, 0):gt_v:(a_gt_v:int -> b_gt_v:(ba_gt_v:int -> bb_gt_v:bool)))).
ut("path   const >", n_e_to_p_e1('>', loc('c.ml', 0, 0, 0, 0, 0, 0), gt_v:(a_gt_v:int -> b_gt_v:(ba_gt_v:int -> bb_gt_v:bool)), '>'@loc('c.ml', 0, 0, 0, 0, 0, 0):gt_v:(a_gt_v:int -> b_gt_v:(ba_gt_v:int -> bb_gt_v:bool))-->true)).
ut("summ   const >", p_e_to_c1('>', loc('c.ml', 0, 0, 0, 0, 0, 0), gt_v:(a_gt_v:int -> b_gt_v:(ba_gt_v:int -> bb_gt_v:bool)), true, true, [('gt_v_int->int->bool'('A_GT_V', 'BA_GT_V') :- 'A_GT_V'>'BA_GT_V')])).
ut("Negative summ   const >", \+ p_e_to_c1('>', loc('c.ml', 0, 0, 0, 0, 0, 0), gt_v:(a_gt_v:int -> b_v:(ba_gt_v:int -> bb_gt_v:bool)), true, true, [])).

ut("PP typed const >", pp('>'@loc('c.ml', 0, 0, 0, 0, 0, 0):(int -> int -> bool), "(>):(int -> int -> bool)")).
ut("PP named const >", pp('>'@loc('c.ml', 0, 0, 0, 0, 0, 0):gt_v:(a_gt_v:int -> b_gt_v:(ba_gt_v:int -> bb_gt_v:bool)), "(>):gt_v:(a_gt_v:int -> b_gt_v:(ba_gt_v:int -> bb_gt_v:bool))")).
ut("PP path const >", pp('>'@loc('c.ml', 0, 0, 0, 0, 0, 0):gt_v:(a_gt_v:int -> b_gt_v:(ba_gt_v:int -> bb_gt_v:bool))-->true, "(>):gt_v:(a_gt_v:int -> b_gt_v:(ba_gt_v:int -> bb_gt_v:bool)) --> true")).



% **********************************************************************
% | x                                                      Identifier

ut("naming x:unit", t_e_to_n_e1(x, loc('x.ml', 0, 0, 0, 0, 0, 0), unit, v, empty, x@loc('x.ml', 0, 0, 0, 0, 0, 0):v:unit)).
ut("Negative naming x:unit", \+ t_e_to_n_e1(x, loc('x.ml', 0, 0, 0, 0, 0, 0), unit, v, empty, x@loc('x.ml', 0, 0, 0, 0, 0, 0):x:unit)).
ut("path   x:unit", n_e_to_p_e1(x, loc('x.ml', 0, 0, 0, 0, 0, 0), v:unit, x@loc('x.ml', 0, 0, 0, 0, 0, 0):v:unit-->('V'='X'))).
ut("Negative path 1 x:unit", \+ n_e_to_p_e1(x, loc('x.ml', 0, 0, 0, 0, 0, 0), v:unit, x@loc('x.ml', 0, 0, 0, 0, 0, 0):v:unit-->true)).
ut("Negative path 2 x:unit", \+ n_e_to_p_e1(x, loc('x.ml', 0, 0, 0, 0, 0, 0), v:unit, (x@loc('x.ml', 0, 0, 0, 0, 0, 0):v:unit-->'V')='X')).
ut("Negative path 3 x:unit", \+ n_e_to_p_e1(x, loc('x.ml', 0, 0, 0, 0, 0, 0), v:unit, x@loc('x.ml', 0, 0, 0, 0, 0, 0):v:unit-->('X'='V'))).
ut("summ   x:unit", p_e_to_c1(x, loc('x.ml', 0, 0, 0, 0, 0, 0), v:unit, true, 'V'='X', [])).
ut("Negative summ   x:unit", \+ p_e_to_c1(x, loc('x.ml', 0, 0, 0, 0, 0, 0), v:unit, true, 'V'='X', [_])).

ut("PP typed x:unit", pp(x@loc('x.ml', 0, 0, 0, 0, 0, 0):unit, "x:unit")).
ut("PP named x:unit", pp(x@loc('x.ml', 0, 0, 0, 0, 0, 0):v:unit, "x:v:unit")).
ut("PP path  x:unit", pp(x@loc('x.ml', 0, 0, 0, 0, 0, 0):v:unit-->('V'='X'), "x:v:unit --> V=X")).

ut("naming          x:bool", t_e_to_n_e1(x, loc('x.ml', 0, 0, 0, 0, 0, 0), bool, v, empty, x@loc('x.ml', 0, 0, 0, 0, 0, 0):v:bool)).
ut("Negative naming x:bool", \+ t_e_to_n_e1(x, loc('x.ml', 0, 0, 0, 0, 0, 0), bool, v, empty, x@loc('x.ml', 0, 0, 0, 0, 0, 0):x:bool)).
ut("path            x:bool", n_e_to_p_e1(x, loc('x.ml', 0, 0, 0, 0, 0, 0), v:bool, x@loc('x.ml', 0, 0, 0, 0, 0, 0):v:bool-->('V'='X'))).
ut("Negative path 1 x:bool", \+ n_e_to_p_e1(x, loc('x.ml', 0, 0, 0, 0, 0, 0), v:bool, x@loc('x.ml', 0, 0, 0, 0, 0, 0):v:bool-->true)).
ut("Negative path 2 x:bool", \+ n_e_to_p_e1(x, loc('x.ml', 0, 0, 0, 0, 0, 0), v:bool, x@loc('x.ml', 0, 0, 0, 0, 0, 0):v:bool-->'V'='X')).
ut("Negative path 3 x:bool", \+ n_e_to_p_e1(x, loc('x.ml', 0, 0, 0, 0, 0, 0), v:bool, x@loc('x.ml', 0, 0, 0, 0, 0, 0):v:bool-->'X'='V')).
ut("summ            x:bool", p_e_to_c1(x, loc('x.ml', 0, 0, 0, 0, 0, 0), v:bool, true, 'V'='X', [])).
ut("Negative summ   x:bool", \+ p_e_to_c1(x, loc('x.ml', 0, 0, 0, 0, 0, 0), v:bool, true, 'V'='X', [_])).

ut("naming          x:int", t_e_to_n_e1(x, loc('x.ml', 0, 0, 0, 0, 0, 0), int, v, empty, x@loc('x.ml', 0, 0, 0, 0, 0, 0):v:int)).
ut("Negative naming x:int", \+ t_e_to_n_e1(x, loc('x.ml', 0, 0, 0, 0, 0, 0), int, v, empty, x@loc('x.ml', 0, 0, 0, 0, 0, 0):x:int)).
ut("path            x:int", n_e_to_p_e1(x, loc('x.ml', 0, 0, 0, 0, 0, 0), v:int, x@loc('x.ml', 0, 0, 0, 0, 0, 0):v:int-->('V'='X'))).
ut("Negative path 1 x:int", \+ n_e_to_p_e1(x, loc('x.ml', 0, 0, 0, 0, 0, 0), v:int, x@loc('x.ml', 0, 0, 0, 0, 0, 0):v:int-->true)).
ut("Negative path 2 x:int", \+ n_e_to_p_e1(x, loc('x.ml', 0, 0, 0, 0, 0, 0), v:int, x@loc('x.ml', 0, 0, 0, 0, 0, 0):v:int-->'V'='X')).
ut("Negative path 3 x:int", \+ n_e_to_p_e1(x, loc('x.ml', 0, 0, 0, 0, 0, 0), v:int, x@loc('x.ml', 0, 0, 0, 0, 0, 0):v:int-->'X'='V')).
ut("summ            x:int", p_e_to_c1(x, loc('x.ml', 0, 0, 0, 0, 0, 0), v:int, true, 'V'='X', [])).
ut("Negative summ   x:int", \+ p_e_to_c1(x, loc('x.ml', 0, 0, 0, 0, 0, 0), v:int, true, 'V'='X', [_])).

ut("naming 1        add:(int->int->int)", t_e_to_n_e1(add, loc('x.ml', 0, 0, 0, 0, 0, 0), (int->int->int), add_v, node(add, add:(a_add:int -> b_add:(ba_add:int -> bb_add:int)), 0, empty, empty), add@loc('x.ml', 0, 0, 0, 0, 0, 0):add_v:(a_add_v:int->b_add:(ba_add_v:int->bb_add_v:int)))).
ut("naming 2        add:(int->int->int)", t_e_to_n_e1(add, loc('x.ml', 0, 0, 0, 0, 0, 0), (int->int->int), f, node(add, add:(a_add:int -> b_add:(ba_add:int -> bb_add:int)), 0, empty, empty), add@loc('x.ml', 0, 0, 0, 0, 0, 0):f:(a_f:int->b_add:(ba_f:int->bb_f:int)))).
ut("Negative naming add:(int->int->int)", \+ t_e_to_n_e1(add, loc('x.ml', 0, 0, 0, 0, 0, 0), (int->int->int), v, empty, add@loc('x.ml', 0, 0, 0, 0, 0, 0):add_v:(a_add_v:int->b_add:(ba_add_v:int->bb_add_v:int)))).
ut("path            add:(int->int->int)", n_e_to_p_e1(add, loc('x.ml', 0, 0, 0, 0, 0, 0), v:(a_add_v:int->b_add:(ba_add_v:int->bb_add_v:int)), add@loc('x.ml', 0, 0, 0, 0, 0, 0):v:(a_add_v:int->b_add:(ba_add_v:int->bb_add_v:int))-->true)).
ut("Negative path   add:(int->int->int)", \+ n_e_to_p_e1(add, loc('x.ml', 0, 0, 0, 0, 0, 0), v:(a_add_v:int->b_add:(ba_add_v:int->bb_add_v:int)), add@loc('x.ml', 0, 0, 0, 0, 0, 0):v:(a_add_v:int->b_add:(ba_add_v:int->bb_add_v:int))-->('V'='ADD'))).
ut("summ            add:(int->int->int)", p_e_to_c1(add, loc('x.ml', 0, 0, 0, 0, 0, 0), v:(a_add_v:int->b_add:(ba_add_v:int->bb_add_v:int)), true, true, [('v_int->int->int'('A_ADD_V', 'BA_ADD_V', 'BB_ADD_V') :- 'add_int->int->int'('A_ADD_V', 'BA_ADD_V', 'BB_ADD_V'))])).
ut("Negative summ   add:(int->int->int)", \+ p_e_to_c1(add, loc('x.ml', 0, 0, 0, 0, 0, 0), v:(a_add_v:int->b_add:(ba_add_v:int->bb_add_v:int)), true, true, [])).

ut("PP typed        add:(int->int->int)", pp(add@loc('x.ml', 0, 0, 0, 0, 0, 0):(int->int->int), "add:(int -> int -> int)")).
ut("PP named        add:(int->int->int)", pp(add@loc('x.ml', 0, 0, 0, 0, 0, 0):v:(a_add_v:int->b_add:(ba_add_v:int->bb_add_v:int)), "add:v:(a_add_v:int -> b_add:(ba_add_v:int -> bb_add_v:int))")).
ut("PP path         add:(int->int->int)", pp(add@loc('x.ml', 0, 0, 0, 0, 0, 0):v:(a_add_v:int->b_add:(ba_add_v:int->bb_add_v:int))-->true, "add:v:(a_add_v:int -> b_add:(ba_add_v:int -> bb_add_v:int)) --> true")).

ut("naming 1        comp:(int->int->bool)", t_e_to_n_e1(comp, loc('x.ml', 0, 0, 0, 0, 0, 0), (int->int->bool), comp_v, node(comp, comp:(a_comp:int -> b_comp:(ba_comp:int -> bb_comp:bool)), 0, empty, empty), comp@loc('x.ml', 0, 0, 0, 0, 0, 0):comp_v:(a_comp_v:int->b_comp:(ba_comp_v:int->bb_comp_v:bool)))).
ut("naming 2        comp:(int->int->bool)", t_e_to_n_e1(comp, loc('x.ml', 0, 0, 0, 0, 0, 0), (int->int->bool), g, node(comp, comp:(a_comp:int -> b_comp:(ba_comp:int -> bb_comp:bool)), 0, empty, empty), comp@loc('x.ml', 0, 0, 0, 0, 0, 0):g:(a_g:int->b_comp:(ba_g:int->bb_g:bool)))).
ut("Negative naming comp:(int->int->bool)", \+ t_e_to_n_e1(comp, loc('x.ml', 0, 0, 0, 0, 0, 0), (int->int->bool), v, empty, comp@loc('x.ml', 0, 0, 0, 0, 0, 0):comp_v:(a_comp_v:int->b_comp:(ba_comp_v:int->bb_comp_v:bool)))).
ut("path            comp:(int->int->bool)", n_e_to_p_e1(comp, loc('x.ml', 0, 0, 0, 0, 0, 0), v:(a_comp_v:int->b_comp:(ba_comp_v:int->bb_comp_v:bool)), comp@loc('x.ml', 0, 0, 0, 0, 0, 0):v:(a_comp_v:int->b_comp:(ba_comp_v:int->bb_comp_v:bool))-->true)).
ut("Negative path   comp:(int->int->bool)", \+ n_e_to_p_e1(comp, loc('x.ml', 0, 0, 0, 0, 0, 0), v:(a_comp_v:int->b_comp:(ba_comp_v:int->bb_comp_v:int)), comp@loc('x.ml', 0, 0, 0, 0, 0, 0):v:(a_comp_v:int->b_comp:(ba_comp_v:int->bb_comp_v:bool))-->('V'='COMP'))).
ut("summ            comp:(int->int->bool)", p_e_to_c1(comp, loc('x.ml', 0, 0, 0, 0, 0, 0), v:(a_comp_v:int->b_comp:(ba_comp_v:int->bb_comp_v:bool)), true, true, [('v_int->int->bool'('A_COMP_V', 'BA_COMP_V') :- 'comp_int->int->bool'('A_COMP_V', 'BA_COMP_V'))])).
ut("Negative summ   comp:(int->int->bool)", \+ p_e_to_c1(comp, loc('x.ml', 0, 0, 0, 0, 0, 0), v:(a_comp_v:int->b_comp:(ba_comp_v:int->bb_comp_v:bool)), true, true, [])).



% **********************************************************************
% | c                                                      Constant
% | e e ... e                                              Application

ut("naming 1+2", t_e_to_n_e1(app((+)@l2:(int->int->int), [1@l3:int, 2@l4:int]), l1, int, v, empty, app((+)@l2:plus_v:(a_plus_v:int -> b_plus_v:(ba_plus_v:int -> v:int)), [1@l3:a_plus_v:int, 2@l4:ba_plus_v:int])@l1:v:int)).
ut("path   1+2", n_e_to_p_e1(app((+)@l2:plus_v:(a_plus_v:int -> b_plus_v:(ba_plus_v:int -> v:int)), [1@l3:a_plus_v:int, 2@l4:ba_plus_v:int]), l1, v:int,
                             app((+)@l2:plus_v:(a_plus_v:int -> b_plus_v:(ba_plus_v:int -> v:int)),
                                 [1@l3:a_plus_v:int,
                                  2@l4:ba_plus_v:int]
                                )@l1:v:int-->('V'=1+2))).
ut("summ   1+2", p_e_to_c1(app((+)@l2:plus_v:(a_plus_v:int -> b_plus_v:(ba_plus_v:int -> v:int)), [1@l3:a_plus_v:int, 2@l4:ba_plus_v:int]), l1, v:int, true, 'V'=1+2, [])).
ut("Negative summ   1+2", \+ p_e_to_c1(app((+)@l2:plus_v:(a_plus_v:int -> b_plus_v:(ba_plus_v:int -> v:int)), [1@l3:a_plus_v:int, 2@l4:ba_plus_v:int]), l1, v:int, true, 'V'=1+2, [_])).

ut("PP typed 1+2", pp(app('+'@l2:(int -> int -> bool), [1@l3:int, 2@l4:int])@l1:bool, "(\n  (+):(int -> int -> bool)\n  1:int\n  2:int\n):bool")).

ut("naming 1>2", t_e_to_n_e1(app((>)@l2:(int->int->bool), [1@l3:int, 2@l4:int]), l1, bool, v, empty, app((>)@l2:gt_v:(a_gt_v:int -> b_gt_v:(ba_gt_v:int -> v:bool)), [1@l3:a_gt_v:int, 2@l4:ba_gt_v:int])@l1:v:bool)).
ut("path   1>2", n_e_to_p_e1(app((>)@l2:gt_v:(a_gt_v:int -> b_gt_v:(ba_gt_v:int -> v:bool)), [1@l3:a_gt_v:int, 2@l4:ba_gt_v:int]), l1, v:bool,
                             app((>)@l2:gt_v:(a_gt_v:int -> b_gt_v:(ba_gt_v:int -> v:bool)),
                                 [1@l3:a_gt_v:int,
                                  2@l4:ba_gt_v:int]
                                )@l1:v:bool-->(1>2))).
ut("summ   1>2", p_e_to_c1(app((>)@l2:gt_v:(a_gt_v:int -> b_gt_v:(ba_gt_v:int -> v:bool)), [1@l3:a_gt_v:int, 2@l4:ba_gt_v:int]), l1, v:bool, true, 1>2, [])).
ut("Negative summ   1>2", \+ p_e_to_c1(app((>)@l2:gt_v:(a_gt_v:int -> b_gt_v:(ba_gt_v:int -> v:bool)), [1@l3:a_gt_v:int, 2@l4:ba_gt_v:int]), l1, v:bool, true, 1>2, [_])).

ut("naming (1+2)+3", t_e_to_n_e1(app((+)@l2:(int->int->int), [app((+)@l4:(int->int->int), [1@l5:int, 2@l6:int])@l3:int, 3@l7:int]), l1, int, v, empty,
                                 app((+)@l2:plus_v:(a_plus_v:int->b_plus_v:(ba_plus_v:int->v:int)),
                                     [app((+)@l4:plus_a_plus_v:(a_plus_a_plus_v:int->b_plus_a_plus_v:(ba_plus_a_plus_v:int->a_plus_v:int)),
                                          [1@l5:a_plus_a_plus_v:int,
                                           2@l6:ba_plus_a_plus_v:int]
                                         )@l3:a_plus_v:int,
                                      3@l7:ba_plus_v:int]
                                    )@l1:v:int)).
ut("path   (1+2)+3", n_e_to_p_e1(app((+)@l2:plus_v:(a_plus_v:int->b_plus_v:(ba_plus_v:int->v:int)),
                                     [app((+)@l4:plus_a_plus_v:(a_plus_a_plus_v:int->b_plus_a_plus_v:(ba_plus_a_plus_v:int->a_plus_v:int)),
                                          [1@l5:a_plus_a_plus_v:int,
                                           2@l6:ba_plus_a_plus_v:int]
                                         )@l3:a_plus_v:int,
                                      3@l7:ba_plus_v:int]
                                    ), l1, v:int,
                                 app((+)@l2:plus_v:(a_plus_v:int->b_plus_v:(ba_plus_v:int->v:int)),
                                     [app((+)@l4:plus_a_plus_v:(a_plus_a_plus_v:int->b_plus_a_plus_v:(ba_plus_a_plus_v:int->a_plus_v:int)),
                                          [1@l5:a_plus_a_plus_v:int,
                                           2@l6:ba_plus_a_plus_v:int]
                                         )@l3:a_plus_v:int --> ('A_PLUS_V'=1+2),
                                      3@l7:ba_plus_v:int]
                                    )@l1:v:int --> ('V'='A_PLUS_V'+3, 'A_PLUS_V'=1+2))).
ut("summ   (1+2)+3", p_e_to_c1(app((+)@l2:plus_v:(a_plus_v:int->b_plus_v:(ba_plus_v:int->v:int)),
                                     [app((+)@l4:plus_a_plus_v:(a_plus_a_plus_v:int->b_plus_a_plus_v:(ba_plus_a_plus_v:int->a_plus_v:int)),
                                          [1@l5:a_plus_a_plus_v:int,
                                           2@l6:ba_plus_a_plus_v:int]
                                         )@l3:a_plus_v:int --> ('A_PLUS_V'=1+2),
                                      3@l7:ba_plus_v:int]
                                  ), l1, v:int, true, ('V'='A_PLUS_V'+3, 'A_PLUS_V'=1+2), [])).
ut("Negative summ   (1+2)+3", \+ p_e_to_c1(app((+)@l2:plus_v:(a_plus_v:int->b_plus_v:(ba_plus_v:int->v:int)),
                                     [app((+)@l4:plus_a_plus_v:(a_plus_a_plus_v:int->b_plus_a_plus_v:(ba_plus_a_plus_v:int->a_plus_v:int)),
                                          [1@l5:a_plus_a_plus_v:int,
                                           2@l6:ba_plus_a_plus_v:int]
                                         )@l3:a_plus_v:int --> ('A_PLUS_V'=1+2),
                                      3@l7:ba_plus_v:int]
                                  ), l1, v:int, true, ('V'='A_PLUS_V'+3, 'A_PLUS_V'=1+2), [_])).

ut("PP path  (1+2)+3", pp(app((+)@l2:plus_v:(a_plus_v:int->b_plus_v:(ba_plus_v:int->v:int)),
                                     [app((+)@l4:plus_a_plus_v:(a_plus_a_plus_v:int->b_plus_a_plus_v:(ba_plus_a_plus_v:int->a_plus_v:int)),
                                          [1@l5:a_plus_a_plus_v:int,
                                           2@l6:ba_plus_a_plus_v:int]
                                         )@l3:a_plus_v:int --> ('A_PLUS_V'=1+2),
                                      3@l7:ba_plus_v:int]
                                    )@l1:v:int --> ('V'='A_PLUS_V'+3, 'A_PLUS_V'=1+2),
                          "(\n  (+):plus_v:(a_plus_v:int -> b_plus_v:(ba_plus_v:int -> v:int))\n  (\n    (+):plus_a_plus_v:(a_plus_a_plus_v:int -> b_plus_a_plus_v:(ba_plus_a_plus_v:int -> a_plus_v:int))\n    1:a_plus_a_plus_v:int\n    2:ba_plus_a_plus_v:int\n  ):a_plus_v:int --> A_PLUS_V=1+2\n  3:ba_plus_v:int\n):v:int --> V=A_PLUS_V+3, A_PLUS_V=1+2")).

ut("naming  (1+2)=3", t_e_to_n_e1(app((=)@l2:(int->int->bool),
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
ut("path    (1+2)=3", n_e_to_p_e1(app((=)@l2:eq_v:(a_eq_v:int->b_eq_v:(ba_eq_v:int->v:bool)),
                                      [app((+)@l4:plus_a_eq_v:(a_plus_a_eq_v:int -> b_plus_a_eq_v:(ba_plus_a_eq_v:int -> a_eq_v:int)),
                                          [1@l5:a_plus_a_eq_v:int,
                                           2@l6:ba_plus_a_eq_v:int]
                                         )@l3:a_eq_v:int,
                                      3@l7:ba_eq_v:int]
                                  ), l1, v:bool,
                                  app((=)@l2:eq_v:(a_eq_v:int->b_eq_v:(ba_eq_v:int->v:bool)),
                                      [app((+)@l4:plus_a_eq_v:(a_plus_a_eq_v:int -> b_plus_a_eq_v:(ba_plus_a_eq_v:int -> a_eq_v:int)),
                                           [1@l5:a_plus_a_eq_v:int,
                                            2@l6:ba_plus_a_eq_v:int]
                                          )@l3:a_eq_v:int --> ('A_EQ_V'=1+2),
                                       3@l7:ba_eq_v:int]
                                     )@l1:v:bool --> ('A_EQ_V'=3, 'A_EQ_V'=1+2))).
ut("summ    (1+2)=3", p_e_to_c1(app((=)@l2:eq_v:(a_eq_v:int->b_eq_v:(ba_eq_v:int->v:bool)),
                                    [app((+)@l4:eq_a_eq_v:(a_eq_a_eq_v:int->b_eq_a_eq_v:(ba_eq_a_eq_v:int->a_eq_v:int)),
                                         [1@l5:a_eq_a_eq_v:int,
                                          2@l6:ba_eq_a_eq_v:int]
                                        )@l3:a_eq_v:int --> ('A_EQ_V'=1+2),
                                     3@l7:ba_eq_v:int]
                                   ), l1, v:bool, true, ('A_EQ_V'=3, 'A_EQ_V'=1+2), [])).
ut("Negative summ   (1+2)=3", \+ p_e_to_c1(app((=)@l2:eq_v:(a_eq_v:int->b_eq_v:(ba_eq_v:int->v:bool)),
                                               [app((+)@l4:eq_a_eq_v:(a_eq_a_eq_v:int->b_eq_a_eq_v:(ba_eq_a_eq_v:int->a_eq_v:int)),
                                                    [1@l5:a_eq_a_eq_v:int,
                                                     2@l6:ba_eq_a_eq_v:int]
                                                   )@l3:a_eq_v:int --> ('A_EQ_V'=1+2),
                                                3@l7:ba_eq_v:int]
                                              ), l1, v:bool, true, ('A_EQ_V'=3, 'A_EQ_V'=1+2), [_])).

ut("naming (+) 1", t_e_to_n_e1(app((+)@l2:(int->int->int), [1@l3:int]), l1, (int->int), v, empty, app((+)@l2:plus_v:(a_plus_v:int -> v:(ba_plus_v:int -> bb_plus_v:int)), [1@l3:a_plus_v:int])@l1:v:(ba_plus_v:int -> bb_plus_v:int))).
ut("path   (+) 1", n_e_to_p_e1(app((+)@l2:plus_v:(a_plus_v:int -> v:(ba_plus_v:int -> bb_plus_v:int)),
                                   [1@l3:a_plus_v:int]
                                  ), l1, v:(ba_plus_v:int -> bb_plus_v:int),
                               app((+)@l2:plus_v:(a_plus_v:int -> v:(ba_plus_v:int -> bb_plus_v:int)),
                                   [1@l3:a_plus_v:int]
                                  )@l1:v:(ba_plus_v:int -> bb_plus_v:int)-->('BB_PLUS_V'=1+'BA_PLUS_V'))).
ut("summ   (+) 1", p_e_to_c1(app((+)@l2:plus_v:(a_plus_v:int -> v:(ba_plus_v:int -> bb_plus_v:int)),
                                 [1@l3:a_plus_v:int]
                                ), l1, v:(ba_plus_v:int -> bb_plus_v:int), true, 'BB_PLUS_V'=1+'BA_PLUS_V',
                             [( 'v_int->int'('BA_PLUS_V', 'BB_PLUS_V') :- 'BB_PLUS_V'=1+'BA_PLUS_V' )])).
ut("Negative summ   (+) 1", \+ p_e_to_c1(app((+)@l2:plus_v:(a_plus_v:int -> v:(ba_plus_v:int -> bb_plus_v:int)),
                                             [1@l3:a_plus_v:int]
                                            ), l1, v:(ba_plus_v:int -> bb_plus_v:int), true, ('V'=1+'BB_PLUS_V'), [])).

ut("PP path  (+) 1", pp(app((+)@l2:plus_v:(a_plus_v:int -> v:(ba_plus_v:int -> bb_plus_v:int)),
                                   [1@l3:a_plus_v:int]
                                  )@l1:v:(ba_plus_v:int -> bb_plus_v:int)-->('BB_PLUS_V'=1+'BA_PLUS_V'),
                        "(\n  (+):plus_v:(a_plus_v:int -> v:(ba_plus_v:int -> bb_plus_v:int))\n  1:a_plus_v:int\n):v:(ba_plus_v:int -> bb_plus_v:int) --> BB_PLUS_V=1+BA_PLUS_V")).

ut("naming (<) true", t_e_to_n_e1(app((<)@l2:(bool->bool->bool), [true@l3:bool]), l1, (bool->bool), v, empty, app((<)@l2:lt_v:(a_lt_v:bool->v:(ba_lt_v:bool->bb_lt_v:bool)), [true@l3:a_lt_v:bool])@l1:v:(ba_lt_v:bool -> bb_lt_v:bool))).
ut("path   (<) true", n_e_to_p_e1(app((<)@l2:lt_v:(a_lt_v:bool->v:(ba_lt_v:bool->bb_lt_v:bool)), [true@l3:a_lt_v:bool]), l1, v:(ba_lt_v:bool -> bb_lt_v:bool),
                                  app((<)@l2:lt_v:(a_lt_v:bool->v:(ba_lt_v:bool->bb_lt_v:bool)),
                                      [true@l3:a_lt_v:bool]
                                     )@l1:v:(ba_lt_v:bool -> bb_lt_v:bool)-->(true<'BA_LT_V'))).
ut("summ   (<) true", p_e_to_c1(app((<)@l2:lt_v:(a_lt_v:bool->v:(ba_lt_v:bool->bb_lt_v:bool)),
                                    [true@l3:a_lt_v:bool]
                                   ), l1, v:(ba_lt_v:bool -> bb_lt_v:bool), true, (true<'BA_LT_V'),
                                [( 'v_bool->bool'('BA_LT_V') :- true<'BA_LT_V' )])).
ut("Negative summ   (<) true", \+ p_e_to_c1(app((<)@l2:lt_v:(a_lt_v:bool->v:(ba_lt_v:bool->bb_lt_v:bool)),
                                    [true@l3:a_lt_v:bool]
                                   ), l1, v:(ba_lt_v:bool -> bb_lt_v:bool), true, (true<'BA_LT_V'), [])).

ut("PP path  (<) 1", pp(app((<)@l2:lt_v:(a_lt_v:bool->v:(ba_lt_v:bool->bb_lt_v:bool)),
                            [true@l3:a_lt_v:bool]
                           )@l1:v:(ba_lt_v:bool -> bb_lt_v:bool) --> (true<'BA_LT_V'),
                        "(\n  (<):lt_v:(a_lt_v:bool -> v:(ba_lt_v:bool -> bb_lt_v:bool))\n  true:a_lt_v:bool\n):v:(ba_lt_v:bool -> bb_lt_v:bool) --> true<BA_LT_V")).



% **********************************************************************
% | c                                                      Constant
% | if e then e else e                                     If

ut("naming   if true then 1 else 0", t_e_to_n_e1(ite(true@l2:bool, 1@l2:int, 0@l3:int), l1, int, res, empty, ite(true@l2:c_res:bool, 1@l2:res:int, 0@l3:res:int)@l1:res:int)).
ut("path     if true then 1 else 0", n_e_to_p_e1(ite(true@l2:c_res:bool, 1@l2:res:int, 0@l3:res:int), l1, res:int,
                                               ite(true@l2:c_res:bool --> true,
                                                   1@l2:res:int --> ('RES'=1),
                                                   0@l3:res:int --> ('RES'=0)
                                                  )@l1:res:int --> (true -> 'RES'=1 ; 'RES'=0))).
ut("summ     if true then 1 else 0", p_e_to_c1(ite(true@l2:c_res:bool --> true,
                                                   1@l2:res:int --> ('RES'=1),
                                                   0@l3:res:int --> ('RES'=0)
                                                  ), l1, res:int, true, (true -> 'RES'=1 ; 'RES'=0), [])).

ut("PP path  if true then 1 else 0", pp(ite(true@l2:c_res:bool --> true,
                                            1@l2:res:int --> ('RES'=1),
                                            0@l3:res:int --> ('RES'=0)
                                           )@l1:res:int --> (true -> 'RES'=1 ; 'RES'=0),
                                        "(if\n  true:c_res:bool --> true\nthen\n  1:res:int --> RES=1\nelse\n  0:res:int --> RES=0\n):res:int --> (true -> RES=1 ; RES=0)")).

ut("naming   if true then false else true", t_e_to_n_e1(ite(true@l2:bool, false@l2:bool, true@l3:bool), l1, bool, res, empty, ite(true@l2:c_res:bool, false@l2:res:bool, true@l3:res:bool)@l1:res:bool)).
ut("path     if true then false else true", n_e_to_p_e1(ite(true@l2:c_res:bool, false@l2:res:bool, true@l3:res:bool), l1, res:bool,
                                                      ite(true@l2:c_res:bool --> true,
                                                          false@l2:res:bool --> false,
                                                          true@l3:res:bool --> true
                                                         )@l1:res:bool --> (true -> false ; true))).
ut("Negative path     if true then false else true", \+ n_e_to_p_e1(ite(true@l2:c_res:bool, false@l2:res:bool, true@l3:res:bool), l1, res:bool,
                                                                    ite(true@l2:c_res:bool --> true,
                                                                        false@l2:res:bool --> 'RES'=0,
                                                                        true@l3:res:bool --> 'RES'=1
                                                                       )@l1:res:bool --> _)).
ut("Negative path     if true then false else true", \+ n_e_to_p_e1(ite(true@l2:c_res:bool, false@l2:res:bool, true@l3:res:bool), l1, res:bool,
                                                                    ite(true@l2:c_res:bool --> true,
                                                                        false@l2:res:bool --> _,
                                                                        true@l3:res:bool --> _
                                                                       )@l1:res:bool --> (true -> 'RES'=1 ; 'RES'=0))).
ut("summ     if true then false else true", p_e_to_c1(ite(true@l2:c_res:bool --> true,
                                                          false@l2:res:bool --> false,
                                                          true@l3:res:bool --> true
                                                         ), l1, res:bool, true, (true -> false ; true), [])).

ut("PP path  if true then false else true", pp(ite(true@l2:c_res:bool --> true,
                                                   false@l2:res:bool --> false,
                                                   true@l3:res:bool --> true
                                                  )@l1:res:bool --> (true -> false ; true),
                                               "(if\n  true:c_res:bool --> true\nthen\n  false:res:bool --> false\nelse\n  true:res:bool --> true\n):res:bool --> (true -> false ; true)")).

ut("naming   if false then (+) else (-)", t_e_to_n_e1(ite(false@l2:bool, (+)@l2:(int->int->int), (-)@l3:(int->int->int)), l1, (int->int->int), f, empty,
                                                      ite(false@l2:c_f:bool,
                                                          (+)@l2:f:(a_f:int -> b_f:(ba_f:int -> bb_f:int)),
                                                          (-)@l3:f:(a_f:int -> b_f:(ba_f:int -> bb_f:int))
                                                         )@l1:f:(a_f:int -> b_f:(ba_f:int -> bb_f:int)))).
ut("path     if false then (+) else (-)", n_e_to_p_e1(ite(false@l2:c_f:bool,
                                                          (+)@l2:f:(a_f:int -> b_f:(ba_f:int -> bb_f:int)),
                                                          (-)@l3:f:(a_f:int -> b_f:(ba_f:int -> bb_f:int))
                                                         ), l1, f:(a_f:int -> b_f:(ba_f:int -> bb_f:int)),
                                                      ite(false@l2:c_f:bool --> false,
                                                          (+)@l2:f:(a_f:int -> b_f:(ba_f:int -> bb_f:int)) --> true,
                                                          (-)@l3:f:(a_f:int -> b_f:(ba_f:int -> bb_f:int)) --> true
                                                         )@l1:f:(a_f:int -> b_f:(ba_f:int -> bb_f:int)) --> true)).
ut("summ     if false then (+) else (-)", p_e_to_c1(ite(false@l2:c_f:bool --> false,
                                                        (+)@l2:f:(a_f:int -> b_f:(ba_f:int -> bb_f:int)) --> true,
                                                        (-)@l3:f:(a_f:int -> b_f:(ba_f:int -> bb_f:int)) --> true
                                                       ), l1, f:(a_f:int -> b_f:(ba_f:int -> bb_f:int)), true, true,
                                                    [('f_int->int->int'('A_F','BA_F','BB_F'):-'BB_F'='A_F'+'BA_F',false),
                                                     ('f_int->int->int'('A_F','BA_F','BB_F'):-'BB_F'='A_F'-'BA_F',\+false)])).
/*
(if
  false:c_f:bool --> false ==> {}
then
  (+):f:(a_f:int -> b_f:(ba_f:int -> bb_f:int)) --> true ==> { 'f_int->int->int'(A_F,BA_F,BB_F) :- BB_F=A_F+BA_F, false }
else
  (-):f:(a_f:int -> b_f:(ba_f:int -> bb_f:int)) --> true ==> { 'f_int->int->int'(A_F,BA_F,BB_F) :- BB_F=A_F-BA_F, \+ false }
):f:(a_f:int -> b_f:(ba_f:int -> bb_f:int)) --> true ==> {}
*/

ut("PP path   if false then (+) else (-)", pp(ite(false@l2:c_f:bool --> false,
                                                          (+)@l2:f:(a_f:int -> b_f:(ba_f:int -> bb_f:int)) --> true,
                                                          (-)@l3:f:(a_f:int -> b_f:(ba_f:int -> bb_f:int)) --> true
                                                 )@l1:f:(a_f:int -> b_f:(ba_f:int -> bb_f:int)) --> true,
                                              "(if\n  false:c_f:bool --> false\nthen\n  (+):f:(a_f:int -> b_f:(ba_f:int -> bb_f:int)) --> true\nelse\n  (-):f:(a_f:int -> b_f:(ba_f:int -> bb_f:int)) --> true\n):f:(a_f:int -> b_f:(ba_f:int -> bb_f:int)) --> true")).

% **********************************************************************
% | c                                                      Constant
% | e e ... e                                              Application
% | assert(e)                                              Assert
% | assume(e)                                              Assume

% assume(1>0)
ut("path      assume(1>0)", false).
ut("summ      assume(1>0)", false).

% assert(true || false)
ut("path      assert(true || false)", false).
ut("summ      assert(true || false)", false).

% assert(1>0 && false)
ut("path      assert(1>0 && false)", false).
ut("summ      assert(1>0 && false)", false).



% **********************************************************************
% | c                                                      Constant
% | e e ... e                                              Application
% | if e then e else e                                     If

% if true then (+) 2 else (-) 1
ut("path      if true then (+) 2 else (-) 1", false).
ut("summ      if true then (+) 2 else (-) 1", false).



% **********************************************************************
% | x                                                      Identifier
% | c                                                      Constant
% | e e ... e                                              Application
% | let x = e in e                                         Let

% let x = 1 in x
ut("path      let x = 1 in x", false).
ut("summ      let x = 1 in x", false).

% let f = (+) in f
ut("path      let f = (+) in f", false).
ut("summ      let f = (+) in f", false).

% let f = let g = (+) in g in f
ut("path      let f = let g = (+) in g in f", false).
ut("summ      let f = let g = (+) in g in f", false).

% let g = (+) in let f = g in f
ut("path      let g = (+) in let f = g in f", false).
ut("summ      let g = (+) in let f = g in f", false).

% let f = (+) 1 in f
ut("path      let f = (+) 1 in f", false).
ut("summ      let f = (+) 1 in f", false).

% let f = (+) 1 in f 2
ut("path      let f = (+) 1 in f 2", false).
ut("summ      let f = (+) 1 in f 2", false).



% **********************************************************************
% | x                                                      Identifier
% | c                                                      Constant
% | e e ... e                                              Application

% (fun x y -> x + y) 1 2
ut("summ      (fun x y -> x + y) 1 2", false).

% (fun x y -> x + y) 1
ut("summ      (fun x y -> x + y) 1", false).

% (fun x -> (+) x)
ut("summ      (fun x -> (+) x)", false).

% (fun f x -> f x) (+) 1
ut("summ      (fun f x -> f x) (+) 1", false).

% (fun f x -> f x) (fun y -> (+) y) 1
ut("summ      (fun f x -> f x) (fun y -> (+) y) 1", false).



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
                  (   for(_, L, 65)
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
