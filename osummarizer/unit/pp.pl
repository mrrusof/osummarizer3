:- ['../pp.pl'].
:- multifile ut/2.

% **********************************************************************
% Pretty printing of typed expressions

ut("PP typed const 10", pp(10@loc('max.ml', 0, 0, 0, 0, 0, 0):int, "10:int")).
ut("PP typed const \"hola\"", pp("hola"@loc('max.ml', 0, 0, 0, 0, 0, 0):string, "\"hola\":string")).
ut("PP typed const (+):(int->int->int)", pp((+)@loc('max.ml', 0, 0, 0, 0, 0, 0):(int -> int -> int), "(+):(int -> int -> int)")).
ut("PP typed const (>):(int->int->int)", pp((>)@loc('max.ml', 0, 0, 0, 0, 0, 0):(int -> int -> bool), "(>):(int -> int -> bool)")).
ut("PP typed id x", pp(x@loc('max.ml', 0, 0, 0, 0, 0, 0):int, "x:int")).
ut("PP typed id inc", pp(inc@loc('max.ml', 0, 0, 0, 0, 0, 0):(int -> int), "inc:(int -> int)")).
ut("PP typed id max1", pp(max1@loc('max.ml', 0, 0, 0, 0, 0, 0):(int -> int -> int), "max1:(int -> int -> int)")).

ut("PP typed app (>):(int->int->int)", pp(app(
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
ut("PP named const (>):(int->int->int)", pp((>)@loc('max.ml', 0, 0, 0, 0, 0, 0):'gt_c_ret_max1':('a_gt_c_ret_max1':int -> 'b_gt_c_ret_max1':('ba_gt_c_ret_max1':int -> 'bb_gt_c_ret_max1':bool)),
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
ut("PP named app (>):(int->int->int)", pp(app(
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
ut("PP named assert (>):(int->int->int)", pp(assert(app('>'@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):gt_ase_ret_f1:(a_gt_ase_ret_f1:int -> b_gt_ase_ret_f1:(ba_gt_ase_ret_f1:int -> ase_ret_f1:bool)),
                                        ['x2'@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):a_gt_ase_ret_f1:int,
                                         0@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):ba_gt_ase_ret_f1:int]
                                       )@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):ase_ret_f1:bool
                                   )@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):ret_f1:unit,
       "(assert\n  (\n    (>):gt_ase_ret_f1:(a_gt_ase_ret_f1:int -> b_gt_ase_ret_f1:(ba_gt_ase_ret_f1:int -> ase_ret_f1:bool))\n    x2:a_gt_ase_ret_f1:int\n    0:ba_gt_ase_ret_f1:int\n  ):ase_ret_f1:bool\n):ret_f1:unit")).
ut("PP named assume true", pp(assume(
                                      true@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):asu__3:bool
                                     )@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):'_3':unit,
       "(assume(\n  true:asu__3:bool\n)):_3:unit")).
ut("PP named assume (>):(int->int->int)", pp(assume(
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
