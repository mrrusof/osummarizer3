% Modified: Wed Jan 29 03:13:01 CET 2014

%
% There are unit tests for the following submodules of osummarizer.
%
% 1. Well-formedness of typed expressions
% 2. Pretty printing of typed expressions
% 3. Naming unit testing
% 4. Summarization unit testing
%

:- ['osummarizer.pl'].
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
        unit_test('Positive test WF const hola',
                  wf_typed_exp("hola"@loc('max.ml', 0, 0, 0, 0, 0, 0):string)).
pos_wf_t_e_const_plus :-
        unit_test('Positive test WF const plus',
                  wf_typed_exp('+'@loc('max.ml', 0, 0, 0, 0, 0, 0):(int -> int -> int))).

pos_wf_t_e_id_x :-
        unit_test('Positive test WF id x',
                  wf_typed_exp(x@loc('max.ml', 0, 0, 0, 0, 0, 0):int)).
pos_wf_t_e_id_inc :-
        unit_test('Positive test WF id inc',
                  wf_typed_exp(inc@loc('max.ml', 0, 0, 0, 0, 0, 0):(int -> int))).

pos_wf_t_e_app :-
        unit_test('Positive test WF app',
                  wf_typed_exp(
                     app(
                         (>)@loc('max.ml', 0, 0, 0, 0, 0, 0):(int -> int -> bool),
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
                  wf_typed_exp(
                     abs([x@loc('snd.ml', 0, 0, 0, 0, 0, 0):int, y@loc('snd.ml', 0, 0, 0, 0, 0, 0):int],
                         x@loc('snd.ml', 0, 0, 0, 0, 0, 0):int
                         )@loc('snd.ml', 0, 0, 0, 0, 0, 0):(int -> int -> int)
                    )).

pos_wf_t_e_ite :-
        unit_test('Positive test WF ite',
                  wf_typed_exp(
                               ite(true@loc('max.ml', 0, 0, 0, 0, 0, 0):bool,
                                   x@loc('max.ml', 0, 0, 0, 0, 0, 0):int,
                                   y@loc('max.ml', 0, 0, 0, 0, 0, 0):int
                                  )@loc('max.ml', 0, 0, 0, 0, 0, 0):int
                 )).

pos_wf_t_e_let :-
        unit_test('Positive test WF let',
                  wf_typed_exp(let(x@loc('max.ml', 0, 0, 0, 0, 0, 0):int,
                                   1@loc('max.ml', 0, 0, 0, 0, 0, 0):int,
                                   x@loc('max.ml', 0, 0, 0, 0, 0, 0):int
                                  )@loc('max.ml', 0, 0, 0, 0, 0, 0):int
                              )).

pos_wf_t_e_max :-
        unit_test('Positive test WF max',
                  wf_typed_exp(let('max1'@loc('max.ml', 0, 0, 0, 0, 0, 0):(int -> int -> int),
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
                                  )@loc('max.ml', 0, 0, 0, 0, 0, 0):int
                              )).

:-      pos_wf_t_e_const_true,
        pos_wf_t_e_const_10,
        pos_wf_t_e_const_hola,
        pos_wf_t_e_const_plus,
        pos_wf_t_e_id_x,
        pos_wf_t_e_id_inc,
        pos_wf_t_e_app,
        pos_wf_t_e_abs_id,
        pos_wf_t_e_abs_snd,
        pos_wf_t_e_ite,
        pos_wf_t_e_let,
        pos_wf_t_e_max.

neg_wf_t_e_app_no_param :-
        unit_test('Negative test WF app no param',
                  wf_typed_exp(
                     app(
                         (>)@loc('max.ml', 0, 0, 0, 0, 0, 0):(int -> int -> bool),
                         []
                        )@loc('max.ml', 0, 0, 0, 0, 0, 0):bool
                    )).

neg_wf_t_e_abs_no_param :-
        unit_test('Negative test WF abs no param',
                  wf_typed_exp(
                     abs([],
                         x@loc('id.ml', 0, 0, 0, 0, 0, 0):int
                         )@loc('id.ml', 0, 0, 0, 0, 0, 0):(int -> int)
                    )).

:-      neg_wf_t_e_app_no_param,
        neg_wf_t_e_abs_no_param.


% % **********************************************************************
% % Pretty printing of typed expressions

pos_pp_typed_const_gt :-
        unit_test('Positive test PP const gt',
                  (
                    format_to_codes("~p", ['>'@loc('max.ml', 0, 0, 0, 0, 0, 0):(int -> int -> bool)], Codes),
                    atom_codes('(>):(int -> int -> bool)', Codes)
                  )).
pos_pp_typed_const_true :-
        unit_test('Positive test PP const true',
                  (
                    format_to_codes("~p", [true@loc('max.ml', 0, 0, 0, 0, 0, 0):bool], Codes),
                    atom_codes('true:bool', Codes)
                  )).
pos_pp_typed_const_10 :-
        unit_test('Positive test PP const 10',
                  (
                    format_to_codes("~p", [10@loc('max.ml', 0, 0, 0, 0, 0, 0):int], Codes),
                    atom_codes('10:int', Codes)
                  )).
pos_pp_typed_const_hola :-
        unit_test('Positive test PP const hola',
                  (
                    format_to_codes("~p", ["hola"@loc('max.ml', 0, 0, 0, 0, 0, 0):string], Codes),
                    atom_codes('"hola":string', Codes)
                  )).
pos_pp_typed_const_plus :-
        unit_test('Positive test PP const plus',
                  (
                    format_to_codes("~p", ['+'@loc('max.ml', 0, 0, 0, 0, 0, 0):(int -> int -> int)], Codes),
                    atom_codes('(+):(int -> int -> int)', Codes)
                  )).

pos_pp_typed_id_x :-
        unit_test('Positive test PP id x',
                  (
                    format_to_codes("~p", [x@loc('max.ml', 0, 0, 0, 0, 0, 0):int], Codes),
                    atom_codes('x:int', Codes)
                  )).
pos_pp_typed_id_inc :-
        unit_test('Positive test PP id inc',
                  (
                    format_to_codes("~p", [inc@loc('max.ml', 0, 0, 0, 0, 0, 0):(int -> int)], Codes),
                    atom_codes('inc:(int -> int)', Codes)
                  )).

pos_pp_typed_app_plus :-
        unit_test('Positive test PP app plus',
                  (
                    Exp = app(
                              (+)@loc('max.ml', 0, 0, 0, 0, 0, 0):(int -> int -> bool),
                              [1@loc('max.ml', 0, 0, 0, 0, 0, 0):int,
                               2@loc('max.ml', 0, 0, 0, 0, 0, 0):int]
                              )@loc('max.ml', 0, 0, 0, 0, 0, 0):bool,
                    format_to_codes("~p", [Exp], Codes),
                    atom_codes('(\n  (+):(int -> int -> bool)\n  1:int\n  2:int\n):bool', Codes)
                  )).
pos_pp_typed_app_gt :-
        unit_test('Positive test PP app gt',
                  (
                    Exp = app(
                              (>)@loc('max.ml', 0, 0, 0, 0, 0, 0):(int -> int -> bool),
                              [x@loc('max.ml', 0, 0, 0, 0, 0, 0):int,
                               y@loc('max.ml', 0, 0, 0, 0, 0, 0):int]
                              )@loc('max.ml', 0, 0, 0, 0, 0, 0):bool,
                    format_to_codes("~p", [Exp], Codes),
                    atom_codes('(\n  (>):(int -> int -> bool)\n  x:int\n  y:int\n):bool', Codes)
                  )).


pos_pp_typed_abs_id :-
        unit_test('Positive test PP abs id',
                  (
                    Exp = abs([x@loc('id.ml', 0, 0, 0, 0, 0, 0):int],
                              x@loc('id.ml', 0, 0, 0, 0, 0, 0):int
                             )@loc('id.ml', 0, 0, 0, 0, 0, 0):(int -> int),
                    format_to_codes("~p", [Exp], Codes),
                    atom_codes('(fun\n  x:int\n->\n  x:int\n):(int -> int)', Codes)
                  )).
pos_pp_typed_abs_snd :-
        unit_test('Positive test PP abs snd',
                  (
                    Exp = abs([x@loc('snd.ml', 0, 0, 0, 0, 0, 0):int, y@loc('snd.ml', 0, 0, 0, 0, 0, 0):int],
                              y@loc('snd.ml', 0, 0, 0, 0, 0, 0):int
                             )@loc('snd.ml', 0, 0, 0, 0, 0, 0):(int -> int -> int),
                    format_to_codes("~p", [Exp], Codes),
                    atom_codes('(fun\n  x:int\n  y:int\n->\n  y:int\n):(int -> int -> int)', Codes)
                  )).

pos_pp_typed_ite :-
        unit_test('Positive test PP ite',
                  (
                    Exp = ite(true@loc('max.ml', 0, 0, 0, 0, 0, 0):bool,
                              x@loc('max.ml', 0, 0, 0, 0, 0, 0):int,
                              y@loc('max.ml', 0, 0, 0, 0, 0, 0):int
                             )@loc('max.ml', 0, 0, 0, 0, 0, 0):int,
                    format_to_codes("~p", [Exp], Codes),
                    atom_codes('(if\n  true:bool\nthen\n  x:int\nelse\n  y:int\n):int', Codes)
                  )).

pos_pp_typed_let :-
        unit_test('Positive test PP let',
                  (
                    Exp = let(x@loc('max.ml', 0, 0, 0, 0, 0, 0):int,
                                   1@loc('max.ml', 0, 0, 0, 0, 0, 0):int,
                                   x@loc('max.ml', 0, 0, 0, 0, 0, 0):int
                                  )@loc('max.ml', 0, 0, 0, 0, 0, 0):int,
                    format_to_codes("~p", [Exp], Codes),
                    atom_codes('(let\n  x:int\n=\n  1:int\nin\n  x:int\n):int', Codes)
                  )).

pos_pp_typed_max :-
        unit_test('Positive test PP max',
                  (
                    Exp = let('max1'@loc('max.ml', 0, 0, 0, 0, 0, 0):(int -> int -> int),
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
                    atom_codes('(let\n  max1:(int -> int -> int)\n=\n  (fun\n    x2:int\n    y3:int\n  ->\n    (if\n      (\n        (>):(int -> int -> bool)\n        x2:int\n        y3:int\n      ):bool\n    then\n      x2:int\n    else\n      y3:int\n    ):int\n  ):(int -> int -> int)\nin\n  (\n    max1:(int -> int -> int)\n    3:int\n    1:int\n  ):int\n):int', Codes)
                  )).

:-      pos_pp_typed_const_gt,
        pos_pp_typed_const_true,
        pos_pp_typed_const_10,
        pos_pp_typed_const_hola,
        pos_pp_typed_const_plus,
        pos_pp_typed_id_x,
        pos_pp_typed_id_inc,
        pos_pp_typed_app_plus,
        pos_pp_typed_app_gt,
        pos_pp_typed_abs_id,
        pos_pp_typed_abs_snd,
        pos_pp_typed_ite,
        pos_pp_typed_let,
        pos_pp_typed_max.
