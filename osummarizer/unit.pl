% Modified: Sun Feb  2 13:23:13 CET 2014

%
% There are unit tests for the following submodules of osummarizer.
%
% 1. Well-formedness of typed expressions
% 2. Pretty printing of typed expressions
% 3. Naming unit testing
% 4. Pretty printing of named expressions
% 5. Summarization unit testing
%

:- ['osummarizer.pl'].
:- use_module(library(lists)).
:- use_module(library(codesio)).


% **********************************************************************
% Unit testing framework

unit_test(TestName, TestGoal) :-
        print(TestName),
        print(' ... '),
        TestGoal,
        !,
        print('PASSED\n').
unit_test(_, _) :-
        print('FAILED\n').


% **********************************************************************
% Well-formedness of typed expressions

pos_wf_t_e_const_true :-
        unit_test('Positive test WF const true',
                  wf_typed_exp(true@loc('max.ml', 0, 0, 0, 0, 0, 0):bool)).
pos_wf_t_e_const_10 :-
        unit_test('Positive test WF const 10',
                  wf_typed_exp('10'@loc('max.ml', 0, 0, 0, 0, 0, 0):int)).
pos_wf_t_e_const_hola :-
        unit_test('Positive test WF const "hola"',
                  wf_typed_exp("hola"@loc('max.ml', 0, 0, 0, 0, 0, 0):string)).
pos_wf_t_e_const_plus :-
        unit_test('Positive test WF const +',
                  wf_typed_exp('+'@loc('max.ml', 0, 0, 0, 0, 0, 0):(int -> int -> int))).
pos_wf_t_e_const_gt :-
        unit_test('Positive test WF const >',
                  wf_typed_exp('>'@loc('max.ml', 0, 0, 0, 0, 0, 0):(int -> int -> bool))).
pos_wf_t_e_id_x :-
        unit_test('Positive test WF id x',
                  wf_typed_exp(x@loc('max.ml', 0, 0, 0, 0, 0, 0):int)).
pos_wf_t_e_id_inc :-
        unit_test('Positive test WF id inc',
                  wf_typed_exp(inc@loc('max.ml', 0, 0, 0, 0, 0, 0):(int -> int))).
pos_wf_t_e_id_max1 :-
        unit_test('Positive test WF id max1',
                  wf_typed_exp(max1@loc('max.ml', 0, 0, 0, 0, 0, 0):(int -> int -> int))).
pos_wf_t_e_app :-
        unit_test('Positive test WF app',
                  wf_typed_exp(
                     app(
                         '>'@loc('max.ml', 0, 0, 0, 0, 0, 0):(int -> int -> bool),
                         [x@loc('max.ml', 0, 0, 0, 0, 0, 0):int,
                          y@loc('max.ml', 0, 0, 0, 0, 0, 0):int]
                        )@loc('max.ml', 0, 0, 0, 0, 0, 0):bool
                    )).
pos_wf_t_e_abs_id :-
        unit_test('Positive test WF abs id',
                  wf_typed_exp(
                     abs([x@loc('id.ml', 0, 0, 0, 0, 0, 0):int],
                         x@loc('id.ml', 0, 0, 0, 0, 0, 0):int
                         )@loc('id.ml', 0, 0, 0, 0, 0, 0):(int -> int)
                    )).
pos_wf_t_e_abs_snd :-
        unit_test('Positive test WF abs snd',
                  wf_typed_exp( abs([x@loc('snd.ml', 0, 0, 0, 0, 0, 0):int, y@loc('snd.ml', 0, 0, 0, 0, 0, 0):int],
                                    x@loc('snd.ml', 0, 0, 0, 0, 0, 0):int
                                   )@loc('snd.ml', 0, 0, 0, 0, 0, 0):(int -> int -> int) )).
pos_wf_t_e_ite :-
        unit_test('Positive test WF ite',
                  wf_typed_exp( ite(true@loc('max.ml', 0, 0, 0, 0, 0, 0):bool,
                                    x@loc('max.ml', 0, 0, 0, 0, 0, 0):int,
                                    y@loc('max.ml', 0, 0, 0, 0, 0, 0):int
                                   )@loc('max.ml', 0, 0, 0, 0, 0, 0):int )).

pos_wf_t_e_let :-
        unit_test('Positive test WF let',
                  wf_typed_exp( let(x@loc('max.ml', 0, 0, 0, 0, 0, 0):int,
                                    1@loc('max.ml', 0, 0, 0, 0, 0, 0):int,
                                    x@loc('max.ml', 0, 0, 0, 0, 0, 0):int
                                   )@loc('max.ml', 0, 0, 0, 0, 0, 0):int )).

pos_wf_t_e_max :-
        unit_test('Positive test WF max',
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
        unit_test('Negative test WF app no param',
                  \+ wf_typed_exp( app(
                                       '>'@loc('max.ml', 0, 0, 0, 0, 0, 0):(int -> int -> bool),
                                       []
                                      )@loc('max.ml', 0, 0, 0, 0, 0, 0):bool )).
neg_wf_t_e_abs_no_param :-
        unit_test('Negative test WF abs no param',
                  \+ wf_typed_exp( abs([],
                                       x@loc('id.ml', 0, 0, 0, 0, 0, 0):int
                                      )@loc('id.ml', 0, 0, 0, 0, 0, 0):(int -> int) )).

wf_t_e :-
        pos_wf_t_e_const_true,
        pos_wf_t_e_const_10,
        pos_wf_t_e_const_hola,
        pos_wf_t_e_const_plus,
        pos_wf_t_e_const_gt,
        pos_wf_t_e_id_x,
        pos_wf_t_e_id_inc,
        pos_wf_t_e_id_max1,
        pos_wf_t_e_app,
        pos_wf_t_e_abs_id,
        pos_wf_t_e_abs_snd,
        pos_wf_t_e_ite,
        pos_wf_t_e_let,
        pos_wf_t_e_max,
        neg_wf_t_e_app_no_param,
        neg_wf_t_e_abs_no_param.


% % **********************************************************************
% % Pretty printing of typed expressions

pos_pp_typed_const_true :-
        unit_test('Positive test PP typed const true',
                  (   format_to_codes("~p", [true@loc('max.ml', 0, 0, 0, 0, 0, 0):bool], Codes),
                      atom_codes('true:bool', Codes) )).
pos_pp_typed_const_10 :-
        unit_test('Positive test PP typed const 10',
                  (   format_to_codes("~p", [10@loc('max.ml', 0, 0, 0, 0, 0, 0):int], Codes),
                      atom_codes('10:int', Codes) )).
pos_pp_typed_const_hola :-
        unit_test('Positive test PP typed const "hola"',
                  (   format_to_codes("~p", ["hola"@loc('max.ml', 0, 0, 0, 0, 0, 0):string], Codes),
                      atom_codes('"hola":string', Codes) )).
pos_pp_typed_const_plus :-
        unit_test('Positive test PP typed const +',
                  (   format_to_codes("~p", ['+'@loc('max.ml', 0, 0, 0, 0, 0, 0):(int -> int -> int)], Codes),
                      atom_codes('(+):(int -> int -> int)', Codes) )).
pos_pp_typed_const_gt :-
        unit_test('Positive test PP typed const >',
                  (   format_to_codes("~p", ['>'@loc('max.ml', 0, 0, 0, 0, 0, 0):(int -> int -> bool)], Codes),
                      atom_codes('(>):(int -> int -> bool)', Codes) )).
pos_pp_typed_id_x :-
        unit_test('Positive test PP typed id x',
                  (   format_to_codes("~p", [x@loc('max.ml', 0, 0, 0, 0, 0, 0):int], Codes),
                      atom_codes('x:int', Codes) )).
pos_pp_typed_id_inc :-
        unit_test('Positive test PP typed id inc',
                  (   format_to_codes("~p", [inc@loc('max.ml', 0, 0, 0, 0, 0, 0):(int -> int)], Codes),
                      atom_codes('inc:(int -> int)', Codes) )).
pos_pp_typed_id_max1 :-
        unit_test('Positive test PP typed id max1',
                  (   format_to_codes("~p", [max1@loc('max.ml', 0, 0, 0, 0, 0, 0):(int -> int -> int)], Codes),
                      atom_codes('max1:(int -> int -> int)', Codes) )).
pos_pp_typed_app_plus :-
        unit_test('Positive test PP typed app +',
                  (   Exp = app(
                                '+'@loc('max.ml', 0, 0, 0, 0, 0, 0):(int -> int -> bool),
                                [1@loc('max.ml', 0, 0, 0, 0, 0, 0):int,
                                 2@loc('max.ml', 0, 0, 0, 0, 0, 0):int]
                               )@loc('max.ml', 0, 0, 0, 0, 0, 0):bool,
                      format_to_codes("~p", [Exp], Codes),
                      atom_codes('(\n  (+):(int -> int -> bool)\n  1:int\n  2:int\n):bool', Codes) )).
pos_pp_typed_app_gt :-
        unit_test('Positive test PP typed app >',
                  (   Exp = app(
                                '>'@loc('max.ml', 0, 0, 0, 0, 0, 0):(int -> int -> bool),
                                [x@loc('max.ml', 0, 0, 0, 0, 0, 0):int,
                                 y@loc('max.ml', 0, 0, 0, 0, 0, 0):int]
                               )@loc('max.ml', 0, 0, 0, 0, 0, 0):bool,
                      format_to_codes("~p", [Exp], Codes),
                      atom_codes('(\n  (>):(int -> int -> bool)\n  x:int\n  y:int\n):bool', Codes) )).
pos_pp_typed_abs_id :-
        unit_test('Positive test PP typed abs id',
                  (   Exp = abs([x@loc('id.ml', 0, 0, 0, 0, 0, 0):int],
                                x@loc('id.ml', 0, 0, 0, 0, 0, 0):int
                               )@loc('id.ml', 0, 0, 0, 0, 0, 0):(int -> int),
                      format_to_codes("~p", [Exp], Codes),
                      atom_codes('(fun\n  x:int\n->\n  x:int\n):(int -> int)', Codes) )).
pos_pp_typed_abs_snd :-
        unit_test('Positive test PP typed abs snd',
                  (  Exp = abs([x@loc('snd.ml', 0, 0, 0, 0, 0, 0):int, y@loc('snd.ml', 0, 0, 0, 0, 0, 0):int],
                               y@loc('snd.ml', 0, 0, 0, 0, 0, 0):int
                              )@loc('snd.ml', 0, 0, 0, 0, 0, 0):(int -> int -> int),
                      format_to_codes("~p", [Exp], Codes),
                      atom_codes('(fun\n  x:int\n  y:int\n->\n  y:int\n):(int -> int -> int)', Codes) )).
pos_pp_typed_ite :-
        unit_test('Positive test PP typed ite',
                  (   Exp = ite(true@loc('max.ml', 0, 0, 0, 0, 0, 0):bool,
                                x@loc('max.ml', 0, 0, 0, 0, 0, 0):int,
                                y@loc('max.ml', 0, 0, 0, 0, 0, 0):int
                               )@loc('max.ml', 0, 0, 0, 0, 0, 0):int,
                      format_to_codes("~p", [Exp], Codes),
                      atom_codes('(if\n  true:bool\nthen\n  x:int\nelse\n  y:int\n):int', Codes) )).
pos_pp_typed_let :-
        unit_test('Positive test PP typed let',
                  (   Exp = let(x@loc('max.ml', 0, 0, 0, 0, 0, 0):int,
                                1@loc('max.ml', 0, 0, 0, 0, 0, 0):int,
                                x@loc('max.ml', 0, 0, 0, 0, 0, 0):int
                               )@loc('max.ml', 0, 0, 0, 0, 0, 0):int,
                      format_to_codes("~p", [Exp], Codes),
                      atom_codes('(let\n  x:int\n=\n  1:int\nin\n  x:int\n):int', Codes) )).
pos_pp_typed_max :-
        unit_test('Positive test PP typed max',
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
        pos_pp_typed_max.


% **********************************************************************
% Naming of typed expressions

pos_name_type_fun_params :-
        unit_test('Positive test name type with function parameters',
                  (   X = ((i -> i) -> ((i -> i) -> i)),
                      name_type(f, X, Y),
                      Y == f:(a_f:(aa_f:i -> ab_f:i) -> b_f:(ba_f:(baa_f:i -> bab_f:i) -> bb_f:i)) )).
pos_name_type_tyvar_params :-
        unit_test('Positive test name type with type variable parameters',
                  (   X = (i -> ((i -> B) -> B)),
                      name_type(f, X, Y),
                      Y == f:(a_f:i -> b_f:(ba_f:(baa_f:i -> bab_f:B) -> bb_f:B)) )).
pos_named_type_to_params_1 :-
        unit_test('Positive test named_type_to_params 1',
                  (   named_type_to_params(f:(a_f:(aa_f:i -> ab_f:i) -> b_f:(ba_f:(baa_f:i -> bab_f:i) -> bb_f:i)), R),
                      R == [a_f:(aa_f:i -> ab_f:i), ba_f:(baa_f:i -> bab_f:i), bb_f:i] )).
pos_named_type_to_params_2 :-
        unit_test('Postiive test named_type_to_params 2',
                  (   named_type_to_params(f:(g:(x:int -> y:bool) -> h:(z:int -> u:bool)), R),
                      R == [g:(x:int->y:bool), z:int, u:bool] )).
pos_rename_return_1 :-
        unit_test('Positive test rename_return 1',
                  (   rename_return(1, w, f:(g:(x:int -> y:bool) -> w:(z:int -> u:bool)), R),
                      R == f:(g:(x:int -> y:bool) -> w:(z:int -> u:bool)) )).
pos_rename_return_2 :-
        unit_test('Positive test rename_return 1',
                  (   rename_return(2, w, f:(g:(x:int -> y:bool) -> h:(z:int -> u:bool)), R),
                      R == f:(g:(x:int -> y:bool) -> h:(z:int -> w:bool)) )).
pos_rename_return_3 :-
        unit_test('Positive test rename_return 2',
                  (   rename_return(2, w, f:(a_f:(aa_f:i -> ab_f:i) -> b_f:(ba_f:(baa_f:i -> bab_f:i) -> bb_f:i)), R),
                      R == f:(a_f:(aa_f:i -> ab_f:i) -> b_f:(ba_f:(baa_f:i -> bab_f:i) -> w:i)) )).
pos_rename_return_4 :-
        unit_test('Positive test rename_return 3',
                  (   rename_return(1, w, f:(a_f:(aa_f:i -> ab_f:i) -> b_f:(ba_f:(baa_f:i -> bab_f:i) -> bb_f:i)), R),
                      R == f:(a_f:(aa_f:i -> ab_f:i) -> w:(ba_f:(baa_f:i -> bab_f:i) -> bb_f:i)) )).
pos_remove_params_1 :-
        unit_test('Positive test remove_params 1',
                  (   remove_params(1, f:(a_f:(aa_f:i -> ab_f:i) -> b_f:(ba_f:(baa_f:i -> bab_f:i) -> bb_f:i)), R),
                      R == b_f:(ba_f:(baa_f:i -> bab_f:i) -> bb_f:i) )).
pos_remove_params_2 :-
        unit_test('Positive test remove_params 2',
                  (   remove_params(2, f:(a_f:(aa_f:i -> ab_f:i) -> b_f:(ba_f:(baa_f:i -> bab_f:i) -> bb_f:i)), R),
                      R == bb_f:i )).
pos_remove_params_3 :-
        unit_test('Positive test remove_params 3',
                  (   remove_params(1, f:(g:(x:int -> y:bool) -> h:(z:int -> u:bool)), R),
                      R == h:(z:int -> u:bool) )).
pos_remove_params_4 :-
        unit_test('Positive rest remove_params 4',
                  (   remove_params(2, f:(g:(x:int -> y:bool) -> h:(z:int -> u:bool)), R),
                      R == u:bool )).
pos_naming_const_true :-
        unit_test('Positive test naming const true',
                  (   t_e_to_n_e1(true, loc('max.ml', 0, 0, 0, 0, 0, 0), bool, r, empty, R),
                      R == true@loc('max.ml', 0, 0, 0, 0, 0, 0):r:bool )).
pos_naming_const_10 :-
        unit_test('Positive test naming const 10',
                  (   t_e_to_n_e1(10, loc('max.ml', 0, 0, 0, 0, 0, 0), int, r, empty, R),
                      R == 10@loc('max.ml', 0, 0, 0, 0, 0, 0):r:int )).
pos_naming_const_hola :-
        unit_test('Positive test naming const "hola"',
                  (   t_e_to_n_e1("hola", loc('max.ml', 0, 0, 0, 0, 0, 0), string, str, empty, R),
                      R == "hola"@loc('max.ml', 0, 0, 0, 0, 0, 0):str:string )).
pos_naming_const_plus :-
        unit_test('Positive test naming const +',
                  (   t_e_to_n_e1('+', loc('max.ml', 0, 0, 0, 0, 0, 0), (int -> int -> int), r, empty, R),
                      R == '+'@loc('max.ml', 0, 0, 0, 0, 0, 0):'+_r':('a_+_r':int -> 'b_+_r':('ba_+_r':int -> 'bb_+_r':int)) )).
pos_naming_const_gt :-
        unit_test('Positive test naming const >',
                  (   t_e_to_n_e1('>', loc('max.ml', 0, 0, 0, 0, 0, 0), (int -> int -> bool), c_ret_max1, empty, R),
                      R == '>'@loc('max.ml', 0, 0, 0, 0, 0, 0):'>_c_ret_max1':('a_>_c_ret_max1':int -> 'b_>_c_ret_max1':('ba_>_c_ret_max1':int -> 'bb_>_c_ret_max1':bool)) )).
pos_naming_id_x :-
        unit_test('Positive test naming id x',
                  (   t_e_to_n_e1(x, loc('max.ml', 0, 0, 0, 0, 0, 0), int, r, empty, R),
                      R == x@loc('max.ml', 0, 0, 0, 0, 0, 0):r:int )).
pos_naming_id_inc :-
        unit_test('Positive test naming id inc',
                  (   avl_store(inc, empty, inc:(x:int -> r:int), Env),
                      t_e_to_n_e1(inc, loc('max.ml', 0, 0, 0, 0, 0, 0), (int -> int), x, Env, R),
                      R == inc@loc('max.ml', 0, 0, 0, 0, 0, 0):inc:(a_inc_x:int -> b_inc_x:int) )).
pos_naming_id_max1 :-
        unit_test('Positive test naming id max1',
                  (   avl_store(max1, empty, max1:(x2:int -> f1_max1:(y3:int -> ret_max1:int)), Env),
                      t_e_to_n_e1(max1, loc('max.ml', 0, 0, 0, 0, 0, 0), (int -> int -> int), v, Env, R),
                      R == max1@loc('max.ml', 0, 0, 0, 0, 0, 0):max1:(a_max1_v:int -> f1_max1:(ba_max1_v:int -> bb_max1_v:int)) )).
pos_naming_app_plus :-
        unit_test('Positive test naming app +',
                  (   E@L:T = app(
                                  '+'@loc('max.ml', 0, 0, 0, 0, 0, 0):(int -> int -> int),
                                  [1@loc('max.ml', 0, 0, 0, 0, 0, 0):int,
                                   2@loc('max.ml', 0, 0, 0, 0, 0, 0):int]
                                 )@loc('max.ml', 0, 0, 0, 0, 0, 0):bool,
                      t_e_to_n_e1(E, L, T, ret, empty, R),
                      R == app(
                               '+'@loc('max.ml', 0, 0, 0, 0, 0, 0):'+_ret':('a_+_ret':int -> 'b_+_ret':('ba_+_ret':int -> ret:int)),
                               [1@loc('max.ml', 0, 0, 0, 0, 0, 0):'a_+_ret':int,
                                2@loc('max.ml', 0, 0, 0, 0, 0, 0):'ba_+_ret':int]
                              )@loc('max.ml', 0, 0, 0, 0, 0, 0):ret:int )).
pos_naming_app_gt :-
        unit_test('Positive test naming app >',
                  (   E@L:T = app(
                                  '>'@loc('max.ml', 0, 0, 0, 0, 0, 0):(int -> int -> bool),
                                  [x2@loc('max.ml', 0, 0, 0, 0, 0, 0):int,
                                   y3@loc('max.ml', 0, 0, 0, 0, 0, 0):int]
                                 )@loc('max.ml', 0, 0, 0, 0, 0, 0):bool,
                      t_e_to_n_e1(E, L, T, c_ret_max1, empty, R),
                      R == app(
                               '>'@loc('max.ml', 0, 0, 0, 0, 0, 0):'>_c_ret_max1':('a_>_c_ret_max1':int -> 'b_>_c_ret_max1':('ba_>_c_ret_max1':int -> c_ret_max1:bool)),
                               [x2@loc('max.ml', 0, 0, 0, 0, 0, 0):'a_>_c_ret_max1':int,
                                y3@loc('max.ml', 0, 0, 0, 0, 0, 0):'ba_>_c_ret_max1':int]
                              )@loc('max.ml', 0, 0, 0, 0, 0, 0):c_ret_max1:bool )).
pos_naming_abs_id :-
        unit_test('Positive test naming abs id',
                  (   E@L:T = abs([x@loc('id.ml', 0, 0, 0, 0, 0, 0):int],
                                  x@loc('id.ml', 0, 0, 0, 0, 0, 0):int
                                 )@loc('id.ml', 0, 0, 0, 0, 0, 0):(int -> int),
                      t_e_to_n_e1(E, L, T, id, empty, R),
                      R == abs([x@loc('id.ml', 0, 0, 0, 0, 0, 0):x:int],
                               x@loc('id.ml', 0, 0, 0, 0, 0, 0):ret_id:int
                              )@loc('id.ml', 0, 0, 0, 0, 0, 0):id:(x:int -> ret_id:int) )).
pos_naming_abs_snd :-
        unit_test('Positive test naming abs snd',
                  (   E@L:T = abs([x@loc('snd.ml', 0, 0, 0, 0, 0, 0):int, y@loc('snd.ml', 0, 0, 0, 0, 0, 0):int],
                                  y@loc('snd.ml', 0, 0, 0, 0, 0, 0):int
                                 )@loc('snd.ml', 0, 0, 0, 0, 0, 0):(int -> int -> int),
                      t_e_to_n_e1(E, L, T, snd, empty, R),
                      R == abs([x@loc('snd.ml', 0, 0, 0, 0, 0, 0):x:int, y@loc('snd.ml', 0, 0, 0, 0, 0, 0):y:int],
                               y@loc('snd.ml', 0, 0, 0, 0, 0, 0):ret_snd:int
                              )@loc('snd.ml', 0, 0, 0, 0, 0, 0):snd:(x:int -> f1_snd:(y:int -> ret_snd:int)) )).
pos_naming_ite :-
        unit_test('Positive test naming ite',
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
        unit_test('Positive test naming let',
                  (   E@L:T = let(x@loc('max.ml', 0, 0, 0, 0, 0, 0):int,
                                  1@loc('max.ml', 0, 0, 0, 0, 0, 0):int,
                                  x@loc('max.ml', 0, 0, 0, 0, 0, 0):int
                                 )@loc('max.ml', 0, 0, 0, 0, 0, 0):int,
                      t_e_to_n_e1(E, L, T, v, empty, R),
                      R == let(x@loc('max.ml', 0, 0, 0, 0, 0, 0):x:int,
                               1@loc('max.ml', 0, 0, 0, 0, 0, 0):x:int,
                               x@loc('max.ml', 0, 0, 0, 0, 0, 0):v:int
                              )@loc('max.ml', 0, 0, 0, 0, 0, 0):v:int )).
pos_naming_max :-
        unit_test('Positive test naming max',
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
                                   ite(app('>'@loc('max.ml', 0, 0, 0, 0, 0, 0):'>_c_ret_max1':('a_>_c_ret_max1':int -> 'b_>_c_ret_max1':('ba_>_c_ret_max1':int -> c_ret_max1:bool)),
                                           ['x2'@loc('max.ml', 0, 0, 0, 0, 0, 0):'a_>_c_ret_max1':int,
                                            'y3'@loc('max.ml', 0, 0, 0, 0, 0, 0):'ba_>_c_ret_max1':int]
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
        pos_name_type_fun_params,
        pos_name_type_tyvar_params,
        pos_named_type_to_params_1,
        pos_named_type_to_params_2,
        pos_rename_return_1,
        pos_rename_return_2,
        pos_rename_return_3,
        pos_rename_return_4,
        pos_remove_params_1,
        pos_remove_params_2,
        pos_remove_params_3,
        pos_remove_params_4,
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
        pos_naming_max.


% **********************************************************************
% Pretty printing of named expressions

pos_pp_named_const_true :-
        unit_test('Positive test PP named const true',
                  (   format_to_codes("~p", [true@loc('max.ml', 0, 0, 0, 0, 0, 0):r:bool], Codes),
                      atom_codes('true:r:bool', Codes) )).
pos_pp_named_const_10 :-
        unit_test('Positive test PP named const 10',
                  (   format_to_codes("~p", [10@loc('max.ml', 0, 0, 0, 0, 0, 0):r:int], Codes),
                      atom_codes('10:r:int', Codes) )).
pos_pp_named_const_hola :-
        unit_test('Positive test PP named const "hola"',
                  (   format_to_codes("~p", ["hola"@loc('max.ml', 0, 0, 0, 0, 0, 0):str:string], Codes),
                      atom_codes('"hola":str:string', Codes) )).
pos_pp_named_const_plus :-
        unit_test('Positive test PP named const +',
                  (   format_to_codes("~p", ['+'@loc('max.ml', 0, 0, 0, 0, 0, 0):'+_r':('a_+_r':int -> 'b_+_r':('ba_+_r':int -> 'bb_+_r':int))], Codes),
                      atom_codes('(+):+_r:(a_+_r:int -> b_+_r:(ba_+_r:int -> bb_+_r:int))', Codes) )).
pos_pp_named_const_gt :-
        unit_test('Positive test PP named const >',
                  (   format_to_codes("~p", ['>'@loc('max.ml', 0, 0, 0, 0, 0, 0):'>_c_ret_max1':('a_>_c_ret_max1':int -> 'b_>_c_ret_max1':('ba_>_c_ret_max1':int -> 'bb_>_c_ret_max1':bool))], Codes),
                      atom_codes('(>):>_c_ret_max1:(a_>_c_ret_max1:int -> b_>_c_ret_max1:(ba_>_c_ret_max1:int -> bb_>_c_ret_max1:bool))', Codes) )).
pos_pp_named_id_x :-
        unit_test('Positive test PP named id x',
                  (   format_to_codes("~p", [x@loc('max.ml', 0, 0, 0, 0, 0, 0):r:int], Codes),
                      atom_codes('x:r:int', Codes) )).
pos_pp_named_id_inc :-
        unit_test('Positive test PP named id inc',
                  (   format_to_codes("~p", [inc@loc('max.ml', 0, 0, 0, 0, 0, 0):inc:(a_inc_x:int -> b_inc_x:int)], Codes),
                      atom_codes('inc:inc:(a_inc_x:int -> b_inc_x:int)', Codes) )).
pos_pp_named_id_max1 :-
        unit_test('Positive test PP named id max1',
                  (   format_to_codes("~p", [max1@loc('max.ml', 0, 0, 0, 0, 0, 0):max1:(a_max1_v:int -> f1_max1:(ba_max1_v:int -> bb_max1_v:int))], Codes),
                      atom_codes('max1:max1:(a_max1_v:int -> f1_max1:(ba_max1_v:int -> bb_max1_v:int))', Codes) )).
pos_pp_named_app_plus :-
        unit_test('Positive test PP named app +',
                  (   Exp = app(
                                '+'@loc('max.ml', 0, 0, 0, 0, 0, 0):'+_ret':('a_+_ret':int -> 'b_+_ret':('ba_+_ret':int -> ret:int)),
                                [1@loc('max.ml', 0, 0, 0, 0, 0, 0):'a_+_ret':int,
                                 2@loc('max.ml', 0, 0, 0, 0, 0, 0):'ba_+_ret':int]
                               )@loc('max.ml', 0, 0, 0, 0, 0, 0):ret:int,
                      format_to_codes("~p", [Exp], Codes),
                      atom_codes('(\n  (+):+_ret:(a_+_ret:int -> b_+_ret:(ba_+_ret:int -> ret:int))\n  1:a_+_ret:int\n  2:ba_+_ret:int\n):ret:int', Codes) )).
pos_pp_named_app_gt :-
        unit_test('Positive test PP named app >',
                  (   Exp = app(
                               '>'@loc('max.ml', 0, 0, 0, 0, 0, 0):'>_c_ret_max1':('a_>_c_ret_max1':int -> 'b_>_c_ret_max1':('ba_>_c_ret_max1':int -> c_ret_max1:bool)),
                               [x2@loc('max.ml', 0, 0, 0, 0, 0, 0):'a_>_c_ret_max1':int,
                                y3@loc('max.ml', 0, 0, 0, 0, 0, 0):'ba_>_c_ret_max1':int]
                              )@loc('max.ml', 0, 0, 0, 0, 0, 0):c_ret_max1:bool,
                      format_to_codes("~p", [Exp], Codes),
                      atom_codes('(\n  (>):>_c_ret_max1:(a_>_c_ret_max1:int -> b_>_c_ret_max1:(ba_>_c_ret_max1:int -> c_ret_max1:bool))\n  x2:a_>_c_ret_max1:int\n  y3:ba_>_c_ret_max1:int\n):c_ret_max1:bool', Codes) )).
pos_pp_named_abs_id :-
        unit_test('Positive test PP named abs id',
                  (   Exp = abs([x@loc('id.ml', 0, 0, 0, 0, 0, 0):x:int],
                               x@loc('id.ml', 0, 0, 0, 0, 0, 0):ret_id:int
                              )@loc('id.ml', 0, 0, 0, 0, 0, 0):id:(x:int -> ret_id:int),
                      format_to_codes("~p", [Exp], Codes),
                      atom_codes('(fun\n  x:x:int\n->\n  x:ret_id:int\n):id:(x:int -> ret_id:int)', Codes) )).
pos_pp_named_abs_snd :-
        unit_test('Positive test PP named abs snd',
                  (   Exp = abs([x@loc('snd.ml', 0, 0, 0, 0, 0, 0):x:int, y@loc('snd.ml', 0, 0, 0, 0, 0, 0):y:int],
                                y@loc('snd.ml', 0, 0, 0, 0, 0, 0):ret_snd:int
                               )@loc('snd.ml', 0, 0, 0, 0, 0, 0):snd:(x:int -> f1_snd:(y:int -> ret_snd:int)),
                      format_to_codes("~p", [Exp], Codes),
                      atom_codes('(fun\n  x:x:int\n  y:y:int\n->\n  y:ret_snd:int\n):snd:(x:int -> f1_snd:(y:int -> ret_snd:int))', Codes) )).
pos_pp_named_ite :-
        unit_test('Positive test PP named ite',
                  (   Exp = ite(true@loc('max.ml', 0, 0, 0, 0, 0, 0):c_ret:bool,
                               x@loc('max.ml', 0, 0, 0, 0, 0, 0):ret:int,
                               y@loc('max.ml', 0, 0, 0, 0, 0, 0):ret:int
                              )@loc('max.ml', 0, 0, 0, 0, 0, 0):ret:int,
                      format_to_codes("~p", [Exp], Codes),
                      atom_codes('(if\n  true:c_ret:bool\nthen\n  x:ret:int\nelse\n  y:ret:int\n):ret:int', Codes)
                  )).
pos_pp_named_let :-
        unit_test('Positive test PP named let',
                  (   Exp = let(x@loc('max.ml', 0, 0, 0, 0, 0, 0):x:int,
                                1@loc('max.ml', 0, 0, 0, 0, 0, 0):x:int,
                                x@loc('max.ml', 0, 0, 0, 0, 0, 0):v:int
                               )@loc('max.ml', 0, 0, 0, 0, 0, 0):v:int,
                      format_to_codes("~p", [Exp], Codes),
                      atom_codes('(let\n  x:x:int\n=\n  1:x:int\nin\n  x:v:int\n):v:int', Codes) )).
pos_pp_named_max :-
        unit_test('Positive test PP named max',
                  (   Exp = let('max1'@loc('max.ml', 0, 0, 0, 0, 0, 0):max1:(x2:int -> f1_max1:(y3:int -> ret_max1:int)),
                                abs(['x2'@loc('max.ml', 0, 0, 0, 0, 0, 0):x2:int,
                                     'y3'@loc('max.ml', 0, 0, 0, 0, 0, 0):y3:int],
                                    ite(app('>'@loc('max.ml', 0, 0, 0, 0, 0, 0):'>_c_ret_max1':('a_>_c_ret_max1':int -> 'b_>_c_ret_max1':('ba_>_c_ret_max1':int -> c_ret_max1:bool)),
                                            ['x2'@loc('max.ml', 0, 0, 0, 0, 0, 0):'a_>_c_ret_max1':int,
                                             'y3'@loc('max.ml', 0, 0, 0, 0, 0, 0):'ba_>_c_ret_max1':int]
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
                      atom_codes('(let\n  max1:max1:(x2:int -> f1_max1:(y3:int -> ret_max1:int))\n=\n  (fun\n    x2:x2:int\n    y3:y3:int\n  ->\n    (if\n      (\n        (>):>_c_ret_max1:(a_>_c_ret_max1:int -> b_>_c_ret_max1:(ba_>_c_ret_max1:int -> c_ret_max1:bool))\n        x2:a_>_c_ret_max1:int\n        y3:ba_>_c_ret_max1:int\n      ):c_ret_max1:bool\n    then\n      x2:ret_max1:int\n    else\n      y3:ret_max1:int\n    ):ret_max1:int\n  ):max1:(x2:int -> f1_max1:(y3:int -> ret_max1:int))\nin\n  (\n    max1:max1:(a_max1_v:int -> f1_max1:(ba_max1_v:int -> v:int))\n    3:a_max1_v:int\n    1:ba_max1_v:int\n  ):v:int\n):v:int', Codes)
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
        pos_pp_named_max.



% **********************************************************************
% Testing controls

:-      wf_t_e,
        pp_typed,
        naming,
        pp_named.
