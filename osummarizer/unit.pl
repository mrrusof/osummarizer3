%
% The unit tests cover the following modules of osummarizer.
%
% 1. Well-formedness of typed expressions
% 2. Pretty printing of typed expressions
% 3. Naming of typed expressions
% 4. Pretty printing of named expressions
% 5. Summarization of named expressions
% 6. Pretty printing of constraints
%

:- ['osummarizer.pl'].
:- use_module(library(ordsets), [list_to_ord_set/2]).
:- use_module(library(codesio), [format_to_codes/3]).


% **********************************************************************
% Unit testing framework

unit_test(TestName, TestGoal) :-
        length(TestName, L),
        format('~s', [TestName]),
        write(' '),
        (   for(_, L, 65)
        do  write('.')
        ),
        write(' '),
        TestGoal,
        !,
        print('PASSED\n').
unit_test(_, _) :-
        print('FAILED\n').


% **********************************************************************
% Well-formedness of typed expressions

pos_wf_t_e_const_true :-
        unit_test("Positive test WF const true",
                  wf_typed_exp(true@loc('max.ml', 0, 0, 0, 0, 0, 0):bool)).
pos_wf_t_e_const_10 :-
        unit_test("Positive test WF const 10",
                  wf_typed_exp('10'@loc('max.ml', 0, 0, 0, 0, 0, 0):int)).
pos_wf_t_e_const_hola :-
        unit_test("Positive test WF const \"hola\"",
                  wf_typed_exp("hola"@loc('max.ml', 0, 0, 0, 0, 0, 0):string)).
pos_wf_t_e_const_plus :-
        unit_test("Positive test WF const +",
                  wf_typed_exp('+'@loc('max.ml', 0, 0, 0, 0, 0, 0):(int -> int -> int))).
pos_wf_t_e_const_gt :-
        unit_test("Positive test WF const >",
                  wf_typed_exp('>'@loc('max.ml', 0, 0, 0, 0, 0, 0):(int -> int -> bool))).
pos_wf_t_e_id_x :-
        unit_test("Positive test WF id x",
                  wf_typed_exp(x@loc('max.ml', 0, 0, 0, 0, 0, 0):int)).
pos_wf_t_e_id_inc :-
        unit_test("Positive test WF id inc",
                  wf_typed_exp(inc@loc('max.ml', 0, 0, 0, 0, 0, 0):(int -> int))).
pos_wf_t_e_id_max1 :-
        unit_test("Positive test WF id max1",
                  wf_typed_exp(max1@loc('max.ml', 0, 0, 0, 0, 0, 0):(int -> int -> int))).
pos_wf_t_e_app_gt :-
        unit_test("Positive test WF app >",
                  wf_typed_exp(
                     app(
                         '>'@loc('max.ml', 0, 0, 0, 0, 0, 0):(int -> int -> bool),
                         [x@loc('max.ml', 0, 0, 0, 0, 0, 0):int,
                          y@loc('max.ml', 0, 0, 0, 0, 0, 0):int]
                        )@loc('max.ml', 0, 0, 0, 0, 0, 0):bool
                    )).
pos_wf_t_e_abs_id :-
        unit_test("Positive test WF abs id",
                  wf_typed_exp(
                     abs([x@loc('id.ml', 0, 0, 0, 0, 0, 0):int],
                         x@loc('id.ml', 0, 0, 0, 0, 0, 0):int
                         )@loc('id.ml', 0, 0, 0, 0, 0, 0):(int -> int)
                    )).
pos_wf_t_e_abs_snd :-
        unit_test("Positive test WF abs snd",
                  wf_typed_exp( abs([x@loc('snd.ml', 0, 0, 0, 0, 0, 0):int, y@loc('snd.ml', 0, 0, 0, 0, 0, 0):int],
                                    x@loc('snd.ml', 0, 0, 0, 0, 0, 0):int
                                   )@loc('snd.ml', 0, 0, 0, 0, 0, 0):(int -> int -> int) )).
pos_wf_t_e_ite :-
        unit_test("Positive test WF ite",
                  wf_typed_exp( ite(true@loc('max.ml', 0, 0, 0, 0, 0, 0):bool,
                                    x@loc('max.ml', 0, 0, 0, 0, 0, 0):int,
                                    y@loc('max.ml', 0, 0, 0, 0, 0, 0):int
                                   )@loc('max.ml', 0, 0, 0, 0, 0, 0):int )).
pos_wf_t_e_let :-
        unit_test("Positive test WF let",
                  wf_typed_exp( let(x@loc('max.ml', 0, 0, 0, 0, 0, 0):int,
                                    1@loc('max.ml', 0, 0, 0, 0, 0, 0):int,
                                    x@loc('max.ml', 0, 0, 0, 0, 0, 0):int
                                   )@loc('max.ml', 0, 0, 0, 0, 0, 0):int )).
pos_wf_t_e_assert :-
        unit_test("Positive test WF assert",
                  wf_typed_exp( assert(true@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):bool
                                      )@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):unit )).
pos_wf_t_e_assume :-
        unit_test("Positive test WF assume",
                  wf_typed_exp( assume(true@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):bool
                                      )@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):unit )).
pos_wf_t_e_max :-
        unit_test("Positive test WF max",
                  wf_typed_exp( let('max1'@loc('max.ml', 0, 0, 0, 0, 0, 0):(int -> int -> int),
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
neg_wf_t_e_app_no_param :-
        unit_test("Negative test WF app no param",
                  \+ wf_typed_exp( app(
                                       '>'@loc('max.ml', 0, 0, 0, 0, 0, 0):(int -> int -> bool),
                                       []
                                      )@loc('max.ml', 0, 0, 0, 0, 0, 0):bool )).
neg_wf_t_e_abs_no_param :-
        unit_test("Negative test WF abs no param",
                  \+ wf_typed_exp( abs([],
                                       x@loc('id.ml', 0, 0, 0, 0, 0, 0):int
                                      )@loc('id.ml', 0, 0, 0, 0, 0, 0):(int -> int) )).
neg_wf_t_e_assert_1 :-
        unit_test("Negative test WF assert 1",
                  \+ wf_typed_exp( assert(true@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):bool
                                      )@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):bool )).
neg_wf_t_e_assert_2 :-
        unit_test("Negative test WF assert 2",
                  \+ wf_typed_exp( assert(true@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):bool
                                      )@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):int )).
neg_wf_t_e_assert_3 :-
        unit_test("Negative test WF assert 3",
                  \+ wf_typed_exp( assert(true@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):bool
                                      )@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):_ )).
neg_wf_t_e_assume_1 :-
        unit_test("Negative test WF assume 1",
                  \+ wf_typed_exp( assume(true@loc('assume_assume.ml', 0, 0, 0, 0, 0, 0):bool
                                      )@loc('assume_assume.ml', 0, 0, 0, 0, 0, 0):bool )).
neg_wf_t_e_assume_2 :-
        unit_test("Negative test WF assume 2",
                  \+ wf_typed_exp( assume(true@loc('assume_assume.ml', 0, 0, 0, 0, 0, 0):bool
                                      )@loc('assume_assume.ml', 0, 0, 0, 0, 0, 0):int )).
neg_wf_t_e_assume_3 :-
        unit_test("Negative test WF assume 3",
                  \+ wf_typed_exp( assume(true@loc('assume_assume.ml', 0, 0, 0, 0, 0, 0):bool
                                      )@loc('assume_assume.ml', 0, 0, 0, 0, 0, 0):_ )).

wf_t_e :-
        pos_wf_t_e_const_true,
        pos_wf_t_e_const_10,
        pos_wf_t_e_const_hola,
        pos_wf_t_e_const_plus,
        pos_wf_t_e_const_gt,
        pos_wf_t_e_id_x,
        pos_wf_t_e_id_inc,
        pos_wf_t_e_id_max1,
        pos_wf_t_e_app_gt,
        pos_wf_t_e_abs_id,
        pos_wf_t_e_abs_snd,
        pos_wf_t_e_ite,
        pos_wf_t_e_let,
        pos_wf_t_e_assert,
        pos_wf_t_e_assume,
        pos_wf_t_e_max,
        neg_wf_t_e_app_no_param,
        neg_wf_t_e_abs_no_param,
        neg_wf_t_e_assert_1,
        neg_wf_t_e_assert_2,
        neg_wf_t_e_assert_3,
        neg_wf_t_e_assume_1,
        neg_wf_t_e_assume_2,
        neg_wf_t_e_assume_3.


% **********************************************************************
% Pretty printing of typed expressions

pos_pp_typed_const_true :-
        unit_test("Positive test PP typed const true",
                  (   format_to_codes("~p", [true@loc('max.ml', 0, 0, 0, 0, 0, 0):bool], Codes),
                      atom_codes('true:bool', Codes) )).
pos_pp_typed_const_10 :-
        unit_test("Positive test PP typed const 10",
                  (   format_to_codes("~p", [10@loc('max.ml', 0, 0, 0, 0, 0, 0):int], Codes),
                      atom_codes('10:int', Codes) )).
pos_pp_typed_const_hola :-
        unit_test("Positive test PP typed const \"hola\"",
                  (   format_to_codes("~p", ["hola"@loc('max.ml', 0, 0, 0, 0, 0, 0):string], Codes),
                      atom_codes('"hola":string', Codes) )).
pos_pp_typed_const_plus :-
        unit_test("Positive test PP typed const +",
                  (   format_to_codes("~p", ['+'@loc('max.ml', 0, 0, 0, 0, 0, 0):(int -> int -> int)], Codes),
                      atom_codes('(+):(int -> int -> int)', Codes) )).
pos_pp_typed_const_gt :-
        unit_test("Positive test PP typed const >",
                  (   format_to_codes("~p", ['>'@loc('max.ml', 0, 0, 0, 0, 0, 0):(int -> int -> bool)], Codes),
                      atom_codes('(>):(int -> int -> bool)', Codes) )).
pos_pp_typed_id_x :-
        unit_test("Positive test PP typed id x",
                  (   format_to_codes("~p", [x@loc('max.ml', 0, 0, 0, 0, 0, 0):int], Codes),
                      atom_codes('x:int', Codes) )).
pos_pp_typed_id_inc :-
        unit_test("Positive test PP typed id inc",
                  (   format_to_codes("~p", [inc@loc('max.ml', 0, 0, 0, 0, 0, 0):(int -> int)], Codes),
                      atom_codes('inc:(int -> int)', Codes) )).
pos_pp_typed_id_max1 :-
        unit_test("Positive test PP typed id max1",
                  (   format_to_codes("~p", [max1@loc('max.ml', 0, 0, 0, 0, 0, 0):(int -> int -> int)], Codes),
                      atom_codes('max1:(int -> int -> int)', Codes) )).
pos_pp_typed_app_plus :-
        unit_test("Positive test PP typed app +",
                  (   Exp = app(
                                '+'@loc('max.ml', 0, 0, 0, 0, 0, 0):(int -> int -> bool),
                                [1@loc('max.ml', 0, 0, 0, 0, 0, 0):int,
                                 2@loc('max.ml', 0, 0, 0, 0, 0, 0):int]
                               )@loc('max.ml', 0, 0, 0, 0, 0, 0):bool,
                      format_to_codes("~p", [Exp], Codes),
                      atom_codes('(\n  (+):(int -> int -> bool)\n  1:int\n  2:int\n):bool', Codes) )).
pos_pp_typed_app_gt :-
        unit_test("Positive test PP typed app >",
                  (   Exp = app(
                                '>'@loc('max.ml', 0, 0, 0, 0, 0, 0):(int -> int -> bool),
                                [x@loc('max.ml', 0, 0, 0, 0, 0, 0):int,
                                 y@loc('max.ml', 0, 0, 0, 0, 0, 0):int]
                               )@loc('max.ml', 0, 0, 0, 0, 0, 0):bool,
                      format_to_codes("~p", [Exp], Codes),
                      atom_codes('(\n  (>):(int -> int -> bool)\n  x:int\n  y:int\n):bool', Codes) )).
pos_pp_typed_abs_id :-
        unit_test("Positive test PP typed abs id",
                  (   Exp = abs([x@loc('id.ml', 0, 0, 0, 0, 0, 0):int],
                                x@loc('id.ml', 0, 0, 0, 0, 0, 0):int
                               )@loc('id.ml', 0, 0, 0, 0, 0, 0):(int -> int),
                      format_to_codes("~p", [Exp], Codes),
                      atom_codes('(fun\n  x:int\n->\n  x:int\n):(int -> int)', Codes) )).
pos_pp_typed_abs_snd :-
        unit_test("Positive test PP typed abs snd",
                  (  Exp = abs([x@loc('snd.ml', 0, 0, 0, 0, 0, 0):int, y@loc('snd.ml', 0, 0, 0, 0, 0, 0):int],
                               y@loc('snd.ml', 0, 0, 0, 0, 0, 0):int
                              )@loc('snd.ml', 0, 0, 0, 0, 0, 0):(int -> int -> int),
                      format_to_codes("~p", [Exp], Codes),
                      atom_codes('(fun\n  x:int\n  y:int\n->\n  y:int\n):(int -> int -> int)', Codes) )).
pos_pp_typed_ite :-
        unit_test("Positive test PP typed ite",
                  (   Exp = ite(true@loc('max.ml', 0, 0, 0, 0, 0, 0):bool,
                                x@loc('max.ml', 0, 0, 0, 0, 0, 0):int,
                                y@loc('max.ml', 0, 0, 0, 0, 0, 0):int
                               )@loc('max.ml', 0, 0, 0, 0, 0, 0):int,
                      format_to_codes("~p", [Exp], Codes),
                      atom_codes('(if\n  true:bool\nthen\n  x:int\nelse\n  y:int\n):int', Codes) )).
pos_pp_typed_let :-
        unit_test("Positive test PP typed let",
                  (   Exp = let(x@loc('max.ml', 0, 0, 0, 0, 0, 0):int,
                                1@loc('max.ml', 0, 0, 0, 0, 0, 0):int,
                                x@loc('max.ml', 0, 0, 0, 0, 0, 0):int
                               )@loc('max.ml', 0, 0, 0, 0, 0, 0):int,
                      format_to_codes("~p", [Exp], Codes),
                      atom_codes('(let\n  x:int\n=\n  1:int\nin\n  x:int\n):int', Codes) )).
pos_pp_typed_assert_true :-
        unit_test("Positive test PP typed assert true",
                  (   Exp = assert(
                                   true@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):bool
                                  )@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):unit,
                      format_to_codes("~p", [Exp], Codes),
                      atom_codes('(assert(\n  true:bool\n)):unit', Codes) )).
pos_pp_typed_assert_gt :-
        unit_test("Positive test PP typed assert gt",
                  (   Exp = assert(
                                   app(
                                       '>'@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):(int -> int -> bool),
                                       [x@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):int,
                                        y@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):int]
                                      )@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):bool
                                  )@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):unit,
                      format_to_codes("~p", [Exp], Codes),
                      atom_codes('(assert\n  (\n    (>):(int -> int -> bool)\n    x:int\n    y:int\n  ):bool\n):unit', Codes) )).
pos_pp_typed_assume_true :-
        unit_test("Positive test PP typed assume true",
                  (   Exp = assume(
                                   true@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):bool
                                  )@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):unit,
                      format_to_codes("~p", [Exp], Codes),
                      atom_codes('(assume(\n  true:bool\n)):unit', Codes) )).
pos_pp_typed_assume_gt :-
        unit_test("Positive test PP typed assume gt",
                  (   Exp = assume(
                                   app(
                                       '>'@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):(int -> int -> bool),
                                       [x@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):int,
                                        y@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):int]
                                      )@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):bool
                                  )@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):unit,
                      format_to_codes("~p", [Exp], Codes),
                      atom_codes('(assume\n  (\n    (>):(int -> int -> bool)\n    x:int\n    y:int\n  ):bool\n):unit', Codes) )).
pos_pp_typed_max :-
        unit_test("Positive test PP typed max",
                  (   Exp = let('max1'@loc('max.ml', 0, 0, 0, 0, 0, 0):(int -> int -> int),
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
                      format_to_codes("~p", [Exp], Codes),
                      atom_codes('(let\n  max1:(int -> int -> int)\n=\n  (fun\n    x2:int\n    y3:int\n  ->\n    (if\n      (\n        (>):(int -> int -> bool)\n        x2:int\n        y3:int\n      ):bool\n    then\n      x2:int\n    else\n      y3:int\n    ):int\n  ):(int -> int -> int)\nin\n  (\n    max1:(int -> int -> int)\n    3:int\n    1:int\n  ):int\n):int', Codes) )).

pp_typed :-
        pos_pp_typed_const_true,
        pos_pp_typed_const_10,
        pos_pp_typed_const_hola,
        pos_pp_typed_const_plus,
        pos_pp_typed_const_gt,
        pos_pp_typed_id_x,
        pos_pp_typed_id_inc,
        pos_pp_typed_id_max1,
        pos_pp_typed_app_plus,
        pos_pp_typed_app_gt,
        pos_pp_typed_abs_id,
        pos_pp_typed_abs_snd,
        pos_pp_typed_ite,
        pos_pp_typed_let,
        pos_pp_typed_assert_true,
        pos_pp_typed_assert_gt,
        pos_pp_typed_assume_true,
        pos_pp_typed_assume_gt,
        pos_pp_typed_max.


% **********************************************************************
% Naming of typed expressions

pos_name_type_fun_formals :-
        unit_test("Positive test name type with function formals",
                  (   X = ((i -> i) -> ((i -> i) -> i)),
                      name_type(f, X, Y),
                      Y == f:(a_f:(aa_f:i -> ab_f:i) -> b_f:(ba_f:(baa_f:i -> bab_f:i) -> bb_f:i)) )).
pos_name_type_tyvar_formals :-
        unit_test("Positive test name type with type variable formals",
                  (   X = (i -> ((i -> B) -> B)),
                      name_type(f, X, Y),
                      Y == f:(a_f:i -> b_f:(ba_f:(baa_f:i -> bab_f:B) -> bb_f:B)) )).
pos_choose_names_1 :-
        unit_test("Positive test choose_names 1",
                  (   choose_names(inc:(x:int -> r:int),
                                   inc_x:(a_inc_x:int -> b_inc_x:int),
                                   inc:(a_inc_x:int -> b_inc_x:int)) )).
pos_choose_names_2 :-
        unit_test("Positive test choose_names 2",
                  (   choose_names(max1:(x2:int -> f1_max1:(y3:int -> ret_max1:int)),
                                   max1_v:(a_max1_v:int -> f1_max1:(ba_max1_v:int -> bb_max1_v:int)),
                                   max1:(a_max1_v:int -> f1_max1:(ba_max1_v:int -> bb_max1_v:int))) )).
pos_formals_1 :-
        unit_test("Positive test formals 1",
                  (   formals(f:(a_f:(aa_f:i -> ab_f:i) -> b_f:(ba_f:(baa_f:i -> bab_f:i) -> bb_f:i)), R),
                      R == [a_f:(aa_f:i -> ab_f:i), ba_f:(baa_f:i -> bab_f:i)] )).
pos_formals_2 :-
        unit_test("Positive test formals 2",
                  (   formals(f:(g:(x:int -> y:bool) -> h:(z:int -> u:bool)), R),
                      R == [g:(x:int->y:bool), z:int] )).
pos_rename_return_1 :-
        unit_test("Positive test rename_return 1",
                  (   rename_return(1, w, f:(g:(x:int -> y:bool) -> w:(z:int -> u:bool)), R),
                      R == f:(g:(x:int -> y:bool) -> w:(z:int -> u:bool)) )).
pos_rename_return_2 :-
        unit_test("Positive test rename_return 1",
                  (   rename_return(2, w, f:(g:(x:int -> y:bool) -> h:(z:int -> u:bool)), R),
                      R == f:(g:(x:int -> y:bool) -> h:(z:int -> w:bool)) )).
pos_rename_return_3 :-
        unit_test("Positive test rename_return 2",
                  (   rename_return(2, w, f:(a_f:(aa_f:i -> ab_f:i) -> b_f:(ba_f:(baa_f:i -> bab_f:i) -> bb_f:i)), R),
                      R == f:(a_f:(aa_f:i -> ab_f:i) -> b_f:(ba_f:(baa_f:i -> bab_f:i) -> w:i)) )).
pos_rename_return_4 :-
        unit_test("Positive test rename_return 3",
                  (   rename_return(1, w, f:(a_f:(aa_f:i -> ab_f:i) -> b_f:(ba_f:(baa_f:i -> bab_f:i) -> bb_f:i)), R),
                      R == f:(a_f:(aa_f:i -> ab_f:i) -> w:(ba_f:(baa_f:i -> bab_f:i) -> bb_f:i)) )).
pos_remove_formals_1 :-
        unit_test("Positive test remove_formals 1",
                  (   remove_formals(1, f:(a_f:(aa_f:i -> ab_f:i) -> b_f:(ba_f:(baa_f:i -> bab_f:i) -> bb_f:i)), R),
                      R == b_f:(ba_f:(baa_f:i -> bab_f:i) -> bb_f:i) )).
pos_remove_formals_2 :-
        unit_test("Positive test remove_formals 2",
                  (   remove_formals(2, f:(a_f:(aa_f:i -> ab_f:i) -> b_f:(ba_f:(baa_f:i -> bab_f:i) -> bb_f:i)), R),
                      R == bb_f:i )).
pos_remove_formals_3 :-
        unit_test("Positive test remove_formals 3",
                  (   remove_formals(1, f:(g:(x:int -> y:bool) -> h:(z:int -> u:bool)), R),
                      R == h:(z:int -> u:bool) )).
pos_remove_formals_4 :-
        unit_test("Positive rest remove_formals 4",
                  (   remove_formals(2, f:(g:(x:int -> y:bool) -> h:(z:int -> u:bool)), R),
                      R == u:bool )).
pos_naming_const_true :-
        unit_test("Positive test naming const true",
                  (   t_e_to_n_e1(true, loc('max.ml', 0, 0, 0, 0, 0, 0), bool, r, empty, R),
                      R == true@loc('max.ml', 0, 0, 0, 0, 0, 0):r:bool )).
pos_naming_const_10 :-
        unit_test("Positive test naming const 10",
                  (   t_e_to_n_e1(10, loc('max.ml', 0, 0, 0, 0, 0, 0), int, r, empty, R),
                      R == 10@loc('max.ml', 0, 0, 0, 0, 0, 0):r:int )).
pos_naming_const_hola :-
        unit_test("Positive test naming const \"hola\"",
                  (   t_e_to_n_e1("hola", loc('max.ml', 0, 0, 0, 0, 0, 0), string, str, empty, R),
                      R == "hola"@loc('max.ml', 0, 0, 0, 0, 0, 0):str:string )).
pos_naming_const_plus :-
        unit_test("Positive test naming const +",
                  (   t_e_to_n_e1('+', loc('max.ml', 0, 0, 0, 0, 0, 0), (int -> int -> int), r, empty, R),
                      R == '+'@loc('max.ml', 0, 0, 0, 0, 0, 0):'plus_r':('a_plus_r':int -> 'b_plus_r':('ba_plus_r':int -> 'bb_plus_r':int)) )).
pos_naming_const_gt :-
        unit_test("Positive test naming const >",
                  (   t_e_to_n_e1('>', loc('max.ml', 0, 0, 0, 0, 0, 0), (int -> int -> bool), c_ret_max1, empty, R),
                      R == '>'@loc('max.ml', 0, 0, 0, 0, 0, 0):'gt_c_ret_max1':('a_gt_c_ret_max1':int -> 'b_gt_c_ret_max1':('ba_gt_c_ret_max1':int -> 'bb_gt_c_ret_max1':bool)) )).
pos_naming_id_x :-
        unit_test("Positive test naming id x",
                  (   t_e_to_n_e1(x, loc('max.ml', 0, 0, 0, 0, 0, 0), int, r, empty, R),
                      R == x@loc('max.ml', 0, 0, 0, 0, 0, 0):r:int )).
pos_naming_id_inc :-
        unit_test("Positive test naming id inc",
                  (   avl_store(inc, empty, inc:(x:int -> r:int), Env),
                      t_e_to_n_e1(inc, loc('max.ml', 0, 0, 0, 0, 0, 0), (int -> int), x, Env, R),
                      R == inc@loc('max.ml', 0, 0, 0, 0, 0, 0):inc:(a_inc_x:int -> b_inc_x:int) )).
pos_naming_id_max1 :-
        unit_test("Positive test naming id max1",
                  (   avl_store(max1, empty, max1:(x2:int -> f1_max1:(y3:int -> ret_max1:int)), Env),
                      t_e_to_n_e1(max1, loc('max.ml', 0, 0, 0, 0, 0, 0), (int -> int -> int), v, Env, R),
                      R == max1@loc('max.ml', 0, 0, 0, 0, 0, 0):max1:(a_max1_v:int -> f1_max1:(ba_max1_v:int -> bb_max1_v:int)) )).
pos_naming_app_plus :-
        unit_test("Positive test naming app +",
                  (   E@L:T = app(
                                  '+'@loc('max.ml', 0, 0, 0, 0, 0, 0):(int -> int -> int),
                                  [1@loc('max.ml', 0, 0, 0, 0, 0, 0):int,
                                   2@loc('max.ml', 0, 0, 0, 0, 0, 0):int]
                                 )@loc('max.ml', 0, 0, 0, 0, 0, 0):bool,
                      t_e_to_n_e1(E, L, T, ret, empty, R),
                      R == app(
                               '+'@loc('max.ml', 0, 0, 0, 0, 0, 0):'plus_ret':('a_plus_ret':int -> 'b_plus_ret':('ba_plus_ret':int -> ret:int)),
                               [1@loc('max.ml', 0, 0, 0, 0, 0, 0):'a_plus_ret':int,
                                2@loc('max.ml', 0, 0, 0, 0, 0, 0):'ba_plus_ret':int]
                              )@loc('max.ml', 0, 0, 0, 0, 0, 0):ret:int )).
pos_naming_app_gt :-
        unit_test("Positive test naming app >",
                  (   E@L:T = app(
                                  '>'@loc('max.ml', 0, 0, 0, 0, 0, 0):(int -> int -> bool),
                                  [x2@loc('max.ml', 0, 0, 0, 0, 0, 0):int,
                                   y3@loc('max.ml', 0, 0, 0, 0, 0, 0):int]
                                 )@loc('max.ml', 0, 0, 0, 0, 0, 0):bool,
                      t_e_to_n_e1(E, L, T, c_ret_max1, empty, R),
                      R == app(
                               '>'@loc('max.ml', 0, 0, 0, 0, 0, 0):'gt_c_ret_max1':('a_gt_c_ret_max1':int -> 'b_gt_c_ret_max1':('ba_gt_c_ret_max1':int -> c_ret_max1:bool)),
                               [x2@loc('max.ml', 0, 0, 0, 0, 0, 0):'a_gt_c_ret_max1':int,
                                y3@loc('max.ml', 0, 0, 0, 0, 0, 0):'ba_gt_c_ret_max1':int]
                              )@loc('max.ml', 0, 0, 0, 0, 0, 0):c_ret_max1:bool )).
pos_naming_abs_id :-
        unit_test("Positive test naming abs id",
                  (   E@L:T = abs([x@loc('id.ml', 0, 0, 0, 0, 0, 0):int],
                                  x@loc('id.ml', 0, 0, 0, 0, 0, 0):int
                                 )@loc('id.ml', 0, 0, 0, 0, 0, 0):(int -> int),
                      t_e_to_n_e1(E, L, T, id, empty, R),
                      R == abs([x@loc('id.ml', 0, 0, 0, 0, 0, 0):x:int],
                               x@loc('id.ml', 0, 0, 0, 0, 0, 0):ret_id:int
                              )@loc('id.ml', 0, 0, 0, 0, 0, 0):id:(x:int -> ret_id:int) )).
pos_naming_abs_snd :-
        unit_test("Positive test naming abs snd",
                  (   E@L:T = abs([x@loc('snd.ml', 0, 0, 0, 0, 0, 0):int, y@loc('snd.ml', 0, 0, 0, 0, 0, 0):int],
                                  y@loc('snd.ml', 0, 0, 0, 0, 0, 0):int
                                 )@loc('snd.ml', 0, 0, 0, 0, 0, 0):(int -> int -> int),
                      t_e_to_n_e1(E, L, T, snd, empty, R),
                      R == abs([x@loc('snd.ml', 0, 0, 0, 0, 0, 0):x:int, y@loc('snd.ml', 0, 0, 0, 0, 0, 0):y:int],
                               y@loc('snd.ml', 0, 0, 0, 0, 0, 0):ret_snd:int
                              )@loc('snd.ml', 0, 0, 0, 0, 0, 0):snd:(x:int -> f1_snd:(y:int -> ret_snd:int)) )).
pos_naming_ite :-
        unit_test("Positive test naming ite",
                  (   E@L:T = ite(true@loc('max.ml', 0, 0, 0, 0, 0, 0):bool,
                                  x@loc('max.ml', 0, 0, 0, 0, 0, 0):int,
                                  y@loc('max.ml', 0, 0, 0, 0, 0, 0):int
                                 )@loc('max.ml', 0, 0, 0, 0, 0, 0):int,
                      t_e_to_n_e1(E, L, T, ret, empty, R),
                      R == ite(true@loc('max.ml', 0, 0, 0, 0, 0, 0):c_ret:bool,
                               x@loc('max.ml', 0, 0, 0, 0, 0, 0):ret:int,
                               y@loc('max.ml', 0, 0, 0, 0, 0, 0):ret:int
                              )@loc('max.ml', 0, 0, 0, 0, 0, 0):ret:int )).
pos_naming_let :-
        unit_test("Positive test naming let",
                  (   E@L:T = let(x@loc('max.ml', 0, 0, 0, 0, 0, 0):int,
                                  1@loc('max.ml', 0, 0, 0, 0, 0, 0):int,
                                  x@loc('max.ml', 0, 0, 0, 0, 0, 0):int
                                 )@loc('max.ml', 0, 0, 0, 0, 0, 0):int,
                      t_e_to_n_e1(E, L, T, v, empty, R),
                      R == let(x@loc('max.ml', 0, 0, 0, 0, 0, 0):x:int,
                               1@loc('max.ml', 0, 0, 0, 0, 0, 0):x:int,
                               x@loc('max.ml', 0, 0, 0, 0, 0, 0):v:int
                              )@loc('max.ml', 0, 0, 0, 0, 0, 0):v:int )).
pos_naming_assert_true :-
        unit_test("Positive test naming assert true",
                  (   E@L:T = assert(
                                     true@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):bool
                                    )@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):unit,
                      t_e_to_n_e1(E, L, T, ret_f1, empty, R),
                      R == assert(
                                  true@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):ase_ret_f1:bool
                                 )@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):ret_f1:unit )).
pos_naming_assert_gt :-
        unit_test("Positive test naming assert gt",
                  (   E@L:T = assert(app('>'@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):(int -> int -> bool),
                                         ['x2'@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):int,
                                          0@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):int
                                         ]
                                        )@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):bool                                     
                                    )@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):unit,
                      t_e_to_n_e1(E, L, T, ret_f1, empty, R),
                      R == assert(app('>'@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):gt_ase_ret_f1:(a_gt_ase_ret_f1:int -> b_gt_ase_ret_f1:(ba_gt_ase_ret_f1:int -> ase_ret_f1:bool)),
                                      ['x2'@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):a_gt_ase_ret_f1:int,
                                       0@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):ba_gt_ase_ret_f1:int
                                      ]
                                     )@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):ase_ret_f1:bool
                                 )@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):ret_f1:unit )).
pos_naming_assume_true :-
        unit_test("Positive test naming assume true",
                  (   E@L:T = assume(
                                     true@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):bool
                                    )@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):unit,
                      t_e_to_n_e1(E, L, T, '_3', empty, R),
                      R == assume(
                                  true@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):asu__3:bool
                                 )@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):'_3':unit )).
pos_naming_assume_gt :-
        unit_test("Positive test naming assume gt",
                  (   E@L:T = assume(app('>'@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):(int -> int -> bool),
                                         ['x2'@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):int,
                                          1@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):int
                                         ]
                                        )@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):bool                                     
                                    )@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):unit,
                      t_e_to_n_e1(E, L, T, '_3', empty, R),
                      R == assume(app(> @loc('assume_assert.ml',0,0,0,0,0,0):gt_asu__3:(a_gt_asu__3:int->b_gt_asu__3:(ba_gt_asu__3:int->asu__3:bool)),
                                      [x2@loc('assume_assert.ml',0,0,0,0,0,0):a_gt_asu__3:int,
                                       1@loc('assume_assert.ml',0,0,0,0,0,0):ba_gt_asu__3:int
                                      ]
                                     )@loc('assume_assert.ml',0,0,0,0,0,0):asu__3:bool
                                 )@loc('assume_assert.ml',0,0,0,0,0,0):'_3':unit )).
pos_naming_max :-
        unit_test("Positive test naming max",
                  (   E@L:T = let('max1'@loc('max.ml', 0, 0, 0, 0, 0, 0):(int -> int -> int),
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
                      t_e_to_n_e1(E, L, T, v, empty, R),
                      R == let('max1'@loc('max.ml', 0, 0, 0, 0, 0, 0):max1:(x2:int -> f1_max1:(y3:int -> ret_max1:int)),
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
                              )@loc('max.ml', 0, 0, 0, 0, 0, 0):v:int )).

naming :-
        pos_name_type_fun_formals,
        pos_name_type_tyvar_formals,
        pos_choose_names_1,
        pos_choose_names_2,
        pos_formals_1,
        pos_formals_2,
        pos_rename_return_1,
        pos_rename_return_2,
        pos_rename_return_3,
        pos_rename_return_4,
        pos_remove_formals_1,
        pos_remove_formals_2,
        pos_remove_formals_3,
        pos_remove_formals_4,
        pos_naming_const_true,
        pos_naming_const_10,
        pos_naming_const_hola,
        pos_naming_const_plus,
        pos_naming_const_gt,
        pos_naming_id_x,
        pos_naming_id_inc,
        pos_naming_id_max1,
        pos_naming_app_plus,
        pos_naming_app_gt,
        pos_naming_abs_id,
        pos_naming_abs_snd,
        pos_naming_ite,
        pos_naming_let,
        pos_naming_assert_true,
        pos_naming_assert_gt,
        pos_naming_assume_true,
        pos_naming_assume_gt,
        pos_naming_max.


% **********************************************************************
% Pretty printing of named expressions

pos_pp_named_const_true :-
        unit_test("Positive test PP named const true",
                  (   format_to_codes("~p", [true@loc('max.ml', 0, 0, 0, 0, 0, 0):r:bool], Codes),
                      atom_codes('true:r:bool', Codes) )).
pos_pp_named_const_10 :-
        unit_test("Positive test PP named const 10",
                  (   format_to_codes("~p", [10@loc('max.ml', 0, 0, 0, 0, 0, 0):r:int], Codes),
                      atom_codes('10:r:int', Codes) )).
pos_pp_named_const_hola :-
        unit_test("Positive test PP named const \"hola\"",
                  (   format_to_codes("~p", ["hola"@loc('max.ml', 0, 0, 0, 0, 0, 0):str:string], Codes),
                      atom_codes('"hola":str:string', Codes) )).
pos_pp_named_const_plus :-
        unit_test("Positive test PP named const +",
                  (   format_to_codes("~p", ['+'@loc('max.ml', 0, 0, 0, 0, 0, 0):'plus_r':('a_plus_r':int -> 'b_plus_r':('ba_plus_r':int -> 'bb_plus_r':int))], Codes),
                      atom_codes('(+):plus_r:(a_plus_r:int -> b_plus_r:(ba_plus_r:int -> bb_plus_r:int))', Codes) )).
pos_pp_named_const_gt :-
        unit_test("Positive test PP named const >",
                  (   format_to_codes("~p", ['>'@loc('max.ml', 0, 0, 0, 0, 0, 0):'gt_c_ret_max1':('a_gt_c_ret_max1':int -> 'b_gt_c_ret_max1':('ba_gt_c_ret_max1':int -> 'bb_gt_c_ret_max1':bool))], Codes),
                      atom_codes('(>):gt_c_ret_max1:(a_gt_c_ret_max1:int -> b_gt_c_ret_max1:(ba_gt_c_ret_max1:int -> bb_gt_c_ret_max1:bool))', Codes) )).
pos_pp_named_id_x :-
        unit_test("Positive test PP named id x",
                  (   format_to_codes("~p", [x@loc('max.ml', 0, 0, 0, 0, 0, 0):r:int], Codes),
                      atom_codes('x:r:int', Codes) )).
pos_pp_named_id_inc :-
        unit_test("Positive test PP named id inc",
                  (   format_to_codes("~p", [inc@loc('max.ml', 0, 0, 0, 0, 0, 0):inc:(a_inc_x:int -> b_inc_x:int)], Codes),
                      atom_codes('inc:inc:(a_inc_x:int -> b_inc_x:int)', Codes) )).
pos_pp_named_id_max1 :-
        unit_test("Positive test PP named id max1",
                  (   format_to_codes("~p", [max1@loc('max.ml', 0, 0, 0, 0, 0, 0):max1:(a_max1_v:int -> f1_max1:(ba_max1_v:int -> bb_max1_v:int))], Codes),
                      atom_codes('max1:max1:(a_max1_v:int -> f1_max1:(ba_max1_v:int -> bb_max1_v:int))', Codes) )).
pos_pp_named_app_plus :-
        unit_test("Positive test PP named app +",
                  (   Exp = app(
                                '+'@loc('max.ml', 0, 0, 0, 0, 0, 0):'plus_ret':('a_plus_ret':int -> 'b_plus_ret':('ba_plus_ret':int -> ret:int)),
                                [1@loc('max.ml', 0, 0, 0, 0, 0, 0):'a_plus_ret':int,
                                 2@loc('max.ml', 0, 0, 0, 0, 0, 0):'ba_plus_ret':int]
                               )@loc('max.ml', 0, 0, 0, 0, 0, 0):ret:int,
                      format_to_codes("~p", [Exp], Codes),
                      atom_codes('(\n  (+):plus_ret:(a_plus_ret:int -> b_plus_ret:(ba_plus_ret:int -> ret:int))\n  1:a_plus_ret:int\n  2:ba_plus_ret:int\n):ret:int', Codes) )).
pos_pp_named_app_gt :-
        unit_test("Positive test PP named app >",
                  (   Exp = app(
                               '>'@loc('max.ml', 0, 0, 0, 0, 0, 0):'gt_c_ret_max1':('a_gt_c_ret_max1':int -> 'b_gt_c_ret_max1':('ba_gt_c_ret_max1':int -> c_ret_max1:bool)),
                               [x2@loc('max.ml', 0, 0, 0, 0, 0, 0):'a_gt_c_ret_max1':int,
                                y3@loc('max.ml', 0, 0, 0, 0, 0, 0):'ba_gt_c_ret_max1':int]
                              )@loc('max.ml', 0, 0, 0, 0, 0, 0):c_ret_max1:bool,
                      format_to_codes("~p", [Exp], Codes),
                      atom_codes('(\n  (>):gt_c_ret_max1:(a_gt_c_ret_max1:int -> b_gt_c_ret_max1:(ba_gt_c_ret_max1:int -> c_ret_max1:bool))\n  x2:a_gt_c_ret_max1:int\n  y3:ba_gt_c_ret_max1:int\n):c_ret_max1:bool', Codes) )).
pos_pp_named_abs_id :-
        unit_test("Positive test PP named abs id",
                  (   Exp = abs([x@loc('id.ml', 0, 0, 0, 0, 0, 0):x:int],
                               x@loc('id.ml', 0, 0, 0, 0, 0, 0):ret_id:int
                              )@loc('id.ml', 0, 0, 0, 0, 0, 0):id:(x:int -> ret_id:int),
                      format_to_codes("~p", [Exp], Codes),
                      atom_codes('(fun\n  x:x:int\n->\n  x:ret_id:int\n):id:(x:int -> ret_id:int)', Codes) )).
pos_pp_named_abs_snd :-
        unit_test("Positive test PP named abs snd",
                  (   Exp = abs([x@loc('snd.ml', 0, 0, 0, 0, 0, 0):x:int, y@loc('snd.ml', 0, 0, 0, 0, 0, 0):y:int],
                                y@loc('snd.ml', 0, 0, 0, 0, 0, 0):ret_snd:int
                               )@loc('snd.ml', 0, 0, 0, 0, 0, 0):snd:(x:int -> f1_snd:(y:int -> ret_snd:int)),
                      format_to_codes("~p", [Exp], Codes),
                      atom_codes('(fun\n  x:x:int\n  y:y:int\n->\n  y:ret_snd:int\n):snd:(x:int -> f1_snd:(y:int -> ret_snd:int))', Codes) )).
pos_pp_named_ite :-
        unit_test("Positive test PP named ite",
                  (   Exp = ite(true@loc('max.ml', 0, 0, 0, 0, 0, 0):c_ret:bool,
                               x@loc('max.ml', 0, 0, 0, 0, 0, 0):ret:int,
                               y@loc('max.ml', 0, 0, 0, 0, 0, 0):ret:int
                              )@loc('max.ml', 0, 0, 0, 0, 0, 0):ret:int,
                      format_to_codes("~p", [Exp], Codes),
                      atom_codes('(if\n  true:c_ret:bool\nthen\n  x:ret:int\nelse\n  y:ret:int\n):ret:int', Codes)
                  )).
pos_pp_named_let :-
        unit_test("Positive test PP named let",
                  (   Exp = let(x@loc('max.ml', 0, 0, 0, 0, 0, 0):x:int,
                                1@loc('max.ml', 0, 0, 0, 0, 0, 0):x:int,
                                x@loc('max.ml', 0, 0, 0, 0, 0, 0):v:int
                               )@loc('max.ml', 0, 0, 0, 0, 0, 0):v:int,
                      format_to_codes("~p", [Exp], Codes),
                      atom_codes('(let\n  x:x:int\n=\n  1:x:int\nin\n  x:v:int\n):v:int', Codes) )).
pos_pp_named_assert_true :-
        unit_test("Positive test PP named assert true",
                  (   Exp = assert(
                                   true@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):ase_ret_f1:bool
                                  )@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):ret_f1:unit,
                      format_to_codes("~p", [Exp], Codes),
                      atom_codes('(assert(\n  true:ase_ret_f1:bool\n)):ret_f1:unit', Codes) )).
pos_pp_named_assert_gt :-
        unit_test("Positive test PP named assert gt",
                  (   Exp = assert(app('>'@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):gt_ase_ret_f1:(a_gt_ase_ret_f1:int -> b_gt_ase_ret_f1:(ba_gt_ase_ret_f1:int -> ase_ret_f1:bool)),
                                      ['x2'@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):a_gt_ase_ret_f1:int,
                                       0@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):ba_gt_ase_ret_f1:int
                                      ]
                                     )@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):ase_ret_f1:bool
                                 )@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):ret_f1:unit,
                      format_to_codes("~p", [Exp], Codes),
                      atom_codes('(assert\n  (\n    (>):gt_ase_ret_f1:(a_gt_ase_ret_f1:int -> b_gt_ase_ret_f1:(ba_gt_ase_ret_f1:int -> ase_ret_f1:bool))\n    x2:a_gt_ase_ret_f1:int\n    0:ba_gt_ase_ret_f1:int\n  ):ase_ret_f1:bool\n):ret_f1:unit', Codes)  )).
pos_pp_named_assume_true :-
        unit_test("Positive test PP named assume true",
                  (   Exp = assume(
                                   true@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):asu__3:bool
                                  )@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):'_3':unit,
                      format_to_codes("~p", [Exp], Codes),
                      atom_codes('(assume(\n  true:asu__3:bool\n)):_3:unit', Codes) )).
pos_pp_named_assume_gt :-
        unit_test("Positive test PP named assume gt",
                  (   Exp = assume(app('>'@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):gt_asu__3:(a_gt_asu__3:int -> b_gt_asu__3:(ba_gt_asu__3:int -> asu__3:bool)),
                                      ['x2'@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):a_gt_asu__3:int,
                                       1@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):ba_gt_asu__3:int
                                      ]
                                     )@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):asu__3:bool
                                 )@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):'_3':unit,
                      format_to_codes("~p", [Exp], Codes),
                      atom_codes('(assume\n  (\n    (>):gt_asu__3:(a_gt_asu__3:int -> b_gt_asu__3:(ba_gt_asu__3:int -> asu__3:bool))\n    x2:a_gt_asu__3:int\n    1:ba_gt_asu__3:int\n  ):asu__3:bool\n):_3:unit', Codes)  )).
pos_pp_named_max :-
        unit_test("Positive test PP named max",
                  (   Exp = let('max1'@loc('max.ml', 0, 0, 0, 0, 0, 0):max1:(x2:int -> f1_max1:(y3:int -> ret_max1:int)),
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
                      format_to_codes("~p", [Exp], Codes),
                      atom_codes('(let\n  max1:max1:(x2:int -> f1_max1:(y3:int -> ret_max1:int))\n=\n  (fun\n    x2:x2:int\n    y3:y3:int\n  ->\n    (if\n      (\n        (>):gt_c_ret_max1:(a_gt_c_ret_max1:int -> b_gt_c_ret_max1:(ba_gt_c_ret_max1:int -> c_ret_max1:bool))\n        x2:a_gt_c_ret_max1:int\n        y3:ba_gt_c_ret_max1:int\n      ):c_ret_max1:bool\n    then\n      x2:ret_max1:int\n    else\n      y3:ret_max1:int\n    ):ret_max1:int\n  ):max1:(x2:int -> f1_max1:(y3:int -> ret_max1:int))\nin\n  (\n    max1:max1:(a_max1_v:int -> f1_max1:(ba_max1_v:int -> v:int))\n    3:a_max1_v:int\n    1:ba_max1_v:int\n  ):v:int\n):v:int', Codes)
                  )).

pp_named :-
        pos_pp_named_const_true,
        pos_pp_named_const_10,
        pos_pp_named_const_hola,
        pos_pp_named_const_plus,
        pos_pp_named_const_gt,
        pos_pp_named_id_x,
        pos_pp_named_id_inc,
        pos_pp_named_id_max1,
        pos_pp_named_app_plus,
        pos_pp_named_app_gt,
        pos_pp_named_abs_id,
        pos_pp_named_abs_snd,
        pos_pp_named_ite,
        pos_pp_named_let,
        pos_pp_named_assert_true,
        pos_pp_named_assert_gt,
        pos_pp_named_assume_true,
        pos_pp_named_assume_gt,
        pos_pp_named_max.


% **********************************************************************
% Summarization of named expressions

pos_remove_true_1 :-
        unit_test("Positive test remove_true 1",
                  remove_true(('X2'>1,'ctx_f1_int->unit'('X2'),true), ('X2'>1,'ctx_f1_int->unit'('X2'))) ).
pos_remove_true_2 :-
        unit_test("Positive test remove_true 2",
                  remove_true(('X2'>1,true,'ctx_f1_int->unit'('X2')), ('X2'>1,'ctx_f1_int->unit'('X2'))) ).
pos_remove_true_3 :-
        unit_test("Positive test remove_true 3",
                  remove_true((true,'X2'>1,'ctx_f1_int->unit'('X2')), ('X2'>1,'ctx_f1_int->unit'('X2'))) ).
pos_return_1 :-
        unit_test("Positive test return 1",
                  return(f:(a_f:(aa_f:i -> ab_f:i) -> b_f:(ba_f:(baa_f:i -> bab_f:i) -> bb_f:i)), bb_f:i) ).
pos_return_2 :-
        unit_test("Positive test return 2",
                  return(f:(g:(x:int -> y:bool) -> h:(z:int -> u:bool)), u:bool) ).
pos_formals_return_1 :-
        unit_test("Positive test formals_return 1",
                  (   formals_return(f:(a_f:(aa_f:i -> ab_f:i) -> b_f:(ba_f:(baa_f:i -> bab_f:i) -> bb_f:i)), R),
                      R == [a_f:(aa_f:i -> ab_f:i), ba_f:(baa_f:i -> bab_f:i), bb_f:i] )).
pos_formals_return_2 :-
        unit_test("Positive test formals_return 2",
                  (   formals_return(f:(g:(x:int -> y:bool) -> h:(z:int -> u:bool)), R),
                      R == [g:(x:int->y:bool), z:int, u:bool] )).
pos_uppercase_atom_1 :-
        unit_test("Positive test uppercase_atom 1",
                  uppercase_atom(str, 'STR') ).
pos_unname_type_1 :-
        unit_test("Positive test unname_type 1",
                  unname_type(v1:(v11:(v111:i->v112:i)->v12:(v121:(v1211:i->v1212:i)->v122:i)),
                                 (    (     i->     i)->          (      i->      i)->     i ) )).
pos_summ_const_true :-
        unit_test("Positive test summarizing const true",
                  n_e_to_c1(true, loc('max.ml', 0, 0, 0, 0, 0, 0), r:bool, empty, true, true, []) ).
pos_summ_const_10 :-
        unit_test("Positive test summarizing const 10",
                  n_e_to_c1(10, loc('max.ml', 0, 0, 0, 0, 0, 0), r:int, empty, true, 'R'=10, []) ).
pos_summ_const_hola :-
        unit_test("Positive test summarizing const \"hola\"",
                  n_e_to_c1("hola", loc('max.ml', 0, 0, 0, 0, 0, 0), str:string, empty, true, 'STR'="hola", []) ).
pos_summ_const_plus :-
        unit_test("Positive test summarizing const +",
                  n_e_to_c1('+', loc('max.ml', 0, 0, 0, 0, 0, 0),
                            'plus_r':('a_plus_r':int -> 'b_plus_r':('ba_plus_r':int -> 'bb_plus_r':int)),
                            empty, true, ('BB_PLUS_R' = 'A_PLUS_R' + 'BA_PLUS_R'), []) ).
pos_summ_const_gt :-
        unit_test("Positive test summarizing const >",
                  n_e_to_c1('>', loc('max.ml', 0, 0, 0, 0, 0, 0),
                            'gt_c_ret_max1':('a_gt_c_ret_max1':int -> 'b_gt_c_ret_max1':('ba_gt_c_ret_max1':int -> 'bb_gt_c_ret_max1':bool)),
                            empty, true, ('A_GT_C_RET_MAX1' > 'BA_GT_C_RET_MAX1'), []) ).
pos_summ_id_x :-
        unit_test("Positive test summarizing id x",
                  n_e_to_c1(x, loc('max.ml', 0, 0, 0, 0, 0, 0), r:int, empty, true, ('R'='X'), []) ).
pos_summ_id_inc :-
        unit_test("Positive test summarizing id inc",
                  n_e_to_c1(inc, loc('max.ml', 0, 0, 0, 0, 0, 0),
                            inc:(a_inc_x:int -> b_inc_x:int), empty, true, 'inc_int->int'('A_INC_X', 'B_INC_X'), []) ).
pos_summ_id_pos :-
        unit_test("Positive test summarizing id pos",
                  n_e_to_c1(pos, loc('max.ml', 0, 0, 0, 0, 0, 0), pos:(a_pos_x:int -> b_pos_x:bool), empty, true, 'pos_int->bool'('A_POS_X'), []) ).
pos_summ_id_max1 :-
        unit_test("Positive test summarizing id max1",
                  n_e_to_c1(max1, loc('max.ml', 0, 0, 0, 0, 0, 0), max1:(a_max1_v:int -> f1_max1:(ba_max1_v:int -> bb_max1_v:int)),
                            empty, true, 'max1_int->int->int'('A_MAX1_V', 'BA_MAX1_V', 'BB_MAX1_V'), []) ).
pos_summ_app_gt :-
        unit_test("Positive test summarizing app >",
                  (   E@L:N = app(
                                  '>'@loc('max.ml', 0, 0, 0, 0, 0, 0):'gt_c_ret_max1':('a_gt_c_ret_max1':int -> 'b_gt_c_ret_max1':('ba_gt_c_ret_max1':int -> c_ret_max1:bool)),
                                  [x2@loc('max.ml', 0, 0, 0, 0, 0, 0):'a_gt_c_ret_max1':int,
                                   y3@loc('max.ml', 0, 0, 0, 0, 0, 0):'ba_gt_c_ret_max1':int]
                                 )@loc('max.ml', 0, 0, 0, 0, 0, 0):c_ret_max1:bool,
                      n_e_to_c1(E, L, N, empty, true, ('X2' > 'Y3'), []) )).
pos_summ_app_obj_magic :-
        unit_test("Positive test summarizing app Obj.magic",
                  (   E@L:N = app('Obj.magic'@loc('assume_assert.ml',0,0,0,0,0,0):magic_a_f1_v:(a_magic_a_f1_v:unit->a_f1_v:int),
                                  [unit@loc('assume_assert.ml',0,0,0,0,0,0):a_magic_a_f1_v:unit]
                                 )@loc('assume_assert.ml',0,0,0,0,0,0):a_f1_v:int,
                      n_e_to_c1(E, L, N, empty, true, 'A_F1_V'='_', []) )).
pos_summ_app_max1 :-
        unit_test("Positive test summarizing app max1",
                  (   E@L:N = app('max1'@loc('max.ml', 0, 0, 0, 0, 0, 0):max1:(a_max1_v:int -> f1_max1:(ba_max1_v:int -> v:int)),
                                   [3@loc('max.ml', 0, 0, 0, 0, 0, 0):a_max1_v:int,
                                    1@loc('max.ml', 0, 0, 0, 0, 0, 0):ba_max1_v:int]
                                  )@loc('max.ml', 0, 0, 0, 0, 0, 0):v:int,
                      n_e_to_c1(E, L, N, empty, true, 'max1_int->int->int'(3, 1, 'V'), S),
                      S == [('ctx_max1_int->int->int'('A_MAX1_V', 'BA_MAX1_V') :- 'A_MAX1_V'=3, 'BA_MAX1_V'=1)] )).
pos_summ_abs_id :-
        unit_test("Positive test summarizing abs id",
                  (   E@L:N = abs([x2@loc('id.ml', 0, 0, 0, 0, 0, 0):x2:int],
                                  x2@loc('id.ml', 0, 0, 0, 0, 0, 0):ret_id1:int
                                 )@loc('id.ml', 0, 0, 0, 0, 0, 0):id1:(x2:int -> ret_id1:int),
                      n_e_to_c1(E, L, N, empty, true, true, S),
                      S == [('id1_int->int'('X2', 'RET_ID1') :- 'RET_ID1'='X2', 'ctx_id1_int->int'('X2')) ] )).
pos_summ_abs_snd :-
        unit_test("Positive test summarizing abs snd",
                  (   E@L:N = abs([x@loc('snd.ml', 0, 0, 0, 0, 0, 0):x:int, y@loc('snd.ml', 0, 0, 0, 0, 0, 0):y:int],
                                  y@loc('snd.ml', 0, 0, 0, 0, 0, 0):ret_snd:int
                                 )@loc('snd.ml', 0, 0, 0, 0, 0, 0):snd:(x:int -> f1_snd:(y:int -> ret_snd:int)),
                      n_e_to_c1(E, L, N, empty, true, true, S),
                      S == [('snd_int->int->int'('X','Y','RET_SND') :- 'RET_SND'='Y', 'ctx_snd_int->int->int'('X','Y'))] )).
pos_summ_ite_nullary :-
        unit_test("Positive test summarizing ite nullary",
                  (   E@L:N = ite(true@loc('max.ml', 0, 0, 0, 0, 0, 0):c_ret:bool,
                               x@loc('max.ml', 0, 0, 0, 0, 0, 0):ret:int,
                               y@loc('max.ml', 0, 0, 0, 0, 0, 0):ret:int
                              )@loc('max.ml', 0, 0, 0, 0, 0, 0):ret:int,
                      n_e_to_c1(E, L, N, empty, true, (true, 'RET'='X'; \+true, 'RET'='Y'), []) )).
pos_summ_ite_function :-
        unit_test("Positive test summarizing ite function",
                  false ).
pos_summ_let_nullary :-
        unit_test("Positive test summarizing let nullary",
                  (   E@L:N = let(x@loc('max.ml', 0, 0, 0, 0, 0, 0):x:int,
                                  1@loc('max.ml', 0, 0, 0, 0, 0, 0):x:int,
                                  x@loc('max.ml', 0, 0, 0, 0, 0, 0):v:int
                                 )@loc('max.ml', 0, 0, 0, 0, 0, 0):v:int,
                      n_e_to_c1(E, L, N, empty, true, ('V'='X', 'X'=1), []) )).
pos_summ_let_function :-
        unit_test("Positive test summarizing let function",
                  (   E@L:N = let('id1'@loc('id.ml', 0, 0, 0, 0, 0, 0):id1:(x2:int -> ret_id1:int),
                                  abs(['x2'@loc('id.ml', 0, 0, 0, 0, 0, 0):x2:int],
                                      'x2'@loc('id.ml', 0, 0, 0, 0, 0, 0):ret_id1:int
                                      )@loc('id.ml', 0, 0, 0, 0, 0, 0):id1:(x2:int -> ret_id1:int),
                                  app('id1'@loc('id.ml', 0, 0, 0, 0, 0, 0):id1:(a_id1_v:int -> v:int),
                                      [3@loc('id.ml', 0, 0, 0, 0, 0, 0):a_id1_v:int]
                                     )@loc('id.ml', 0, 0, 0, 0, 0, 0):v:int
                                 )@loc('id.ml', 0, 0, 0, 0, 0, 0):v:int,
                      n_e_to_c1(E, L, N, empty, true, 'id1_int->int'(3, 'V'), S),
                      S == [('ctx_id1_int->int'('A_ID1_V') :- 'A_ID1_V'=3),
                            ('id1_int->int'('X2','RET_ID1'):- 'RET_ID1'='X2','ctx_id1_int->int'('X2'))] )).
pos_summ_let_assume_assert :-
        unit_test("Positive test summarizing let assume-assert",
                  (   E@L:N = let('_3'@loc('assume_assert.ml',0,0,0,0,0,0):'_3':unit,
                                          assume(app(> @loc('assume_assert.ml',0,0,0,0,0,0):gt_asu__3:(a_gt_asu__3:int->b_gt_asu__3:(ba_gt_asu__3:int->asu__3:bool)),
                                                     [x2@loc('assume_assert.ml',0,0,0,0,0,0):a_gt_asu__3:int,
                                                      1@loc('assume_assert.ml',0,0,0,0,0,0):ba_gt_asu__3:int
                                                     ]
                                                    )@loc('assume_assert.ml',0,0,0,0,0,0):asu__3:bool
                                                )@loc('assume_assert.ml',0,0,0,0,0,0):'_3':unit,
                                          assert(app(> @loc('assume_assert.ml',0,0,0,0,0,0):gt_ase_ret_f1:(a_gt_ase_ret_f1:int->b_gt_ase_ret_f1:(ba_gt_ase_ret_f1:int->ase_ret_f1:bool)),
                                                     [x2@loc('assume_assert.ml',0,0,0,0,0,0):a_gt_ase_ret_f1:int,
                                                      0@loc('assume_assert.ml',0,0,0,0,0,0):ba_gt_ase_ret_f1:int
                                                     ]
                                                    )@loc('assume_assert.ml',0,0,0,0,0,0):ase_ret_f1:bool
                                                )@loc('assume_assert.ml',0,0,0,0,0,0):ret_f1:unit
                                         )@loc('assume_assert.ml',0,0,0,0,0,0):ret_f1:unit,
                      n_e_to_c1(E, L, N, empty, 'ctx_f1_int->unit'('X2'), ('X2'>0,'X2'>1), [('X2'>0 :- ('X2'>1, 'ctx_f1_int->unit'('X2')))]) )).
pos_summ_assert_true :-
        unit_test("Positive test summarizing assert true",
                  (   E@L:N = assert(
                                     true@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):ase_ret_f1:bool
                                    )@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):ret_f1:unit,
                       n_e_to_c1(E, L, N, empty, true, true, [ (true :- true) ]) )).
pos_summ_assert_gt :-
        unit_test("Positive test summarizing assert gt",
                  (   E@L:N = assert(app('>'@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):gt_ase_ret_f1:(a_gt_ase_ret_f1:int -> b_gt_ase_ret_f1:(ba_gt_ase_ret_f1:int -> ase_ret_f1:bool)),
                                      ['x2'@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):a_gt_ase_ret_f1:int,
                                       0@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):ba_gt_ase_ret_f1:int
                                      ]
                                     )@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):ase_ret_f1:bool
                                 )@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):ret_f1:unit,
                      n_e_to_c1(E, L, N, empty, ('X2'>1, 'ctx_f1_int->unit'('X2')), 'X2'>0, [('X2'>0 :- ('X2'>1, 'ctx_f1_int->unit'('X2')))]) )).
pos_summ_assume_true :-
        unit_test("Positive test summarizing assume true",
                  (   E@L:N = assume(
                                     true@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):asu_3:bool
                                    )@loc('assume_assert.ml', 0, 0, 0, 0, 0, 0):'_3':unit,
                       n_e_to_c1(E, L, N, empty, true, true, []) )).
pos_summ_assume_gt :-
        unit_test("Positive test summarizing assume gt",
                  (   E@L:N = assume(app(> @loc('assume_assert.ml',0,0,0,0,0,0):gt_asu__3:(a_gt_asu__3:int->b_gt_asu__3:(ba_gt_asu__3:int->asu__3:bool)),
                                         [x2@loc('assume_assert.ml',0,0,0,0,0,0):a_gt_asu__3:int,
                                          1@loc('assume_assert.ml',0,0,0,0,0,0):ba_gt_asu__3:int
                                         ]
                                        )@loc('assume_assert.ml',0,0,0,0,0,0):asu__3:bool
                                    )@loc('assume_assert.ml',0,0,0,0,0,0):'_3':unit,
                      n_e_to_c1(E, L, N, empty, true, 'X2'>1, []) )).
pos_summ_max :-
        unit_test("Positive test summarizing max",
                  (   E@L:N = let('max1'@loc('max.ml', 0, 0, 0, 0, 0, 0):max1:(x2:int -> f1_max1:(y3:int -> ret_max1:int)),
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
                      n_e_to_c1(E, L, N, empty, true, 'max1_int->int->int'(3, 1, 'V'), S),
                      S == [('ctx_max1_int->int->int'('A_MAX1_V','BA_MAX1_V'):-'A_MAX1_V'=3,'BA_MAX1_V'=1),
                            ('max1_int->int->int'('X2','Y3','RET_MAX1'):-('X2'>'Y3','RET_MAX1'='X2';\+'X2'>'Y3','RET_MAX1'='Y3'),'ctx_max1_int->int->int'('X2','Y3'))] )).
pos_summ_assume_assert :-
        unit_test("Positive test summarizing assume-assert",
                  (   E@L:N = let(f1@loc('assume_assert.ml',0,0,0,0,0,0):f1:(x2:int->ret_f1:unit),
                                  abs([x2@loc('assume_assert.ml',0,0,0,0,0,0):x2:int],
                                      let('_3'@loc('assume_assert.ml',0,0,0,0,0,0):'_3':unit,
                                          assume(app(> @loc('assume_assert.ml',0,0,0,0,0,0):gt_asu__3:(a_gt_asu__3:int->b_gt_asu__3:(ba_gt_asu__3:int->asu__3:bool)),
                                                     [x2@loc('assume_assert.ml',0,0,0,0,0,0):a_gt_asu__3:int,
                                                      1@loc('assume_assert.ml',0,0,0,0,0,0):ba_gt_asu__3:int
                                                     ]
                                                    )@loc('assume_assert.ml',0,0,0,0,0,0):asu__3:bool
                                                )@loc('assume_assert.ml',0,0,0,0,0,0):'_3':unit,
                                          assert(app(> @loc('assume_assert.ml',0,0,0,0,0,0):gt_ase_ret_f1:(a_gt_ase_ret_f1:int->b_gt_ase_ret_f1:(ba_gt_ase_ret_f1:int->ase_ret_f1:bool)),
                                                     [x2@loc('assume_assert.ml',0,0,0,0,0,0):a_gt_ase_ret_f1:int,
                                                      0@loc('assume_assert.ml',0,0,0,0,0,0):ba_gt_ase_ret_f1:int
                                                     ]
                                                    )@loc('assume_assert.ml',0,0,0,0,0,0):ase_ret_f1:bool
                                                )@loc('assume_assert.ml',0,0,0,0,0,0):ret_f1:unit
                                         )@loc('assume_assert.ml',0,0,0,0,0,0):ret_f1:unit
                                     )@loc('assume_assert.ml',0,0,0,0,0,0):f1:(x2:int->ret_f1:unit),
                                  app(f1@loc('assume_assert.ml',0,0,0,0,0,0):f1:(a_f1_v:int->v:unit),
                                      [app('Obj.magic'@loc('assume_assert.ml',0,0,0,0,0,0):magic_a_f1_v:(a_magic_a_f1_v:unit->a_f1_v:int),
                                           [unit@loc('assume_assert.ml',0,0,0,0,0,0):a_magic_a_f1_v:unit
                                           ]
                                          )@loc('assume_assert.ml',0,0,0,0,0,0):a_f1_v:int
                                      ]
                                     )@loc('assume_assert.ml',0,0,0,0,0,0):v:unit
                                 )@loc('assume_assert.ml',0,0,0,0,0,0):v:unit,
                      n_e_to_c1(E, L, N, empty, true, 'f1_int->unit'('_'), S),
                      S == [('ctx_f1_int->unit'('A_F1_V') :- 'A_F1_V'='_'),
                            ('f1_int->unit'('X2') :- ('X2'>0, 'X2'>1, 'ctx_f1_int->unit'('X2'))),
                            ('X2'>0 :- ('X2'>1, 'ctx_f1_int->unit'('X2')))] )).

summarization :-
        pos_remove_true_1,
        pos_remove_true_2,
        pos_remove_true_3,
        pos_return_1,
        pos_return_2,
        pos_formals_return_1,
        pos_formals_return_2,
        pos_uppercase_atom_1,
        pos_unname_type_1,
        pos_summ_const_true,
        pos_summ_const_10,
        pos_summ_const_hola,
        pos_summ_const_plus,
        pos_summ_const_gt,
        pos_summ_id_x,
        pos_summ_id_inc,
        pos_summ_id_pos,
        pos_summ_id_max1,
        pos_summ_app_gt,
        pos_summ_app_obj_magic,
        pos_summ_app_max1,
        pos_summ_abs_id,
        pos_summ_abs_snd,
        pos_summ_ite_nullary,
        pos_summ_ite_function,
        pos_summ_let_nullary,
        pos_summ_let_function,
        pos_summ_let_assume_assert,
        pos_summ_assert_true,
        pos_summ_assert_gt,
        pos_summ_assume_true,
        pos_summ_assume_gt,
        pos_summ_max,
        pos_summ_assume_assert.


% **********************************************************************
% Pretty printing of constraints

pos_pp_cstr_id1 :-
        unit_test("Positive test pp cstr 'id1_int->int'",
                  (   SC = ('id1_int->int'('X2', 'RET_ID1'):-('RET_ID1'='X2', 'ctx_id1_int->int'('X2'))),
                      format_to_codes("~p", [SC], Codes),
                      format_to_codes("'id1_int->int'(X2, RET_ID1) :- RET_ID1=X2, 'ctx_id1_int->int'(X2).", [], Codes) )).
pos_pp_cstr_ctx_id1 :-
        unit_test("Positive test pp cstr 'ctx_id1_int->int'",
                  (   SC = ('ctx_id1_int->int'('A_ID1_V') :- 'A_ID1_V'=3),
                      format_to_codes("~p", [SC], Codes),
                      format_to_codes("'ctx_id1_int->int'(A_ID1_V) :- A_ID1_V=3.", [], Codes) )).
pos_pp_cstr_max1 :-
        unit_test("Positive test pp cstr 'max1_int->int->int'",
                  (   SC = ('max1_int->int->int'('X2', 'Y3', 'RET_MAX1') :- (('X2'>'Y3', 'RET_MAX1'='X2' ; \+'X2'>'Y3', 'RET_MAX1'='Y3'), 'ctx_max1_int->int->int'('X2', 'Y3'))),
                      format_to_codes("~p", [SC], Codes),
                      format_to_codes("'max1_int->int->int'(X2, Y3, RET_MAX1) :- (X2>Y3, RET_MAX1=X2 ; \\+X2>Y3, RET_MAX1=Y3), 'ctx_max1_int->int->int'(X2, Y3).", [], Codes) )).
pos_pp_cstr_ctx_max1 :-
        unit_test("Positive test pp cstr 'ctx_max1_int->int->int'",
                  (   SC = ('ctx_max1_int->int->int'('A_MAX1_V', 'BA_MAX1_V') :- ('A_MAX1_V'=3, 'BA_MAX1_V'=1)),
                      format_to_codes("~p", [SC], Codes),
                      format_to_codes("'ctx_max1_int->int->int'(A_MAX1_V, BA_MAX1_V) :- A_MAX1_V=3, BA_MAX1_V=1.", [], Codes) )).
pos_pp_cstr_snd :-
        unit_test("Positive test pp cstr 'snd_int->int->int'",
                  (   SC = ('snd_int->int->int'('X','Y','RET_SND') :- ('RET_SND'='Y', 'ctx_snd_int->int->int'('X','Y'))),
                      format_to_codes("~p", [SC], Codes),
                      format_to_codes("'snd_int->int->int'(X, Y, RET_SND) :- RET_SND=Y, 'ctx_snd_int->int->int'(X, Y).", [], Codes) )).
pos_pp_cstr_assert :-
        unit_test("Positive test pp cstr assert",
                  (   SC = ('X2'>0 :- ('X2'>1, 'ctx_f1_int->unit'('X2'))),
                      format_to_codes("~p", [SC], Codes),
                      format_to_codes("X2>0 :- X2>1, 'ctx_f1_int->unit'(X2).", [], Codes) )).

pp_cstr :-
        pos_pp_cstr_id1,
        pos_pp_cstr_ctx_id1,
        pos_pp_cstr_max1,
        pos_pp_cstr_ctx_max1,
        pos_pp_cstr_snd,
        pos_pp_cstr_assert.


% **********************************************************************
% Run the tests

:-      wf_t_e,
        pp_typed,
        naming,
        pp_named,
        summarization,
        pp_cstr.
