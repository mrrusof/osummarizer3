% Modified: Wed Jan 29 14:45:19 CET 2014

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
        pos_wf_t_e_const_gt,
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
                         '>'@loc('max.ml', 0, 0, 0, 0, 0, 0):(int -> int -> bool),
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

pos_pp_typed_const_true :-
        unit_test('Positive test PP typed const true',
                  (
                    format_to_codes("~p", [true@loc('max.ml', 0, 0, 0, 0, 0, 0):bool], Codes),
                    atom_codes('true:bool', Codes)
                  )).
pos_pp_typed_const_10 :-
        unit_test('Positive test PP typed const 10',
                  (
                    format_to_codes("~p", [10@loc('max.ml', 0, 0, 0, 0, 0, 0):int], Codes),
                    atom_codes('10:int', Codes)
                  )).
pos_pp_typed_const_hola :-
        unit_test('Positive test PP typed const "hola"',
                  (
                    format_to_codes("~p", ["hola"@loc('max.ml', 0, 0, 0, 0, 0, 0):string], Codes),
                    atom_codes('"hola":string', Codes)
                  )).
pos_pp_typed_const_plus :-
        unit_test('Positive test PP typed const +',
                  (
                    format_to_codes("~p", ['+'@loc('max.ml', 0, 0, 0, 0, 0, 0):(int -> int -> int)], Codes),
                    atom_codes('(+):(int -> int -> int)', Codes)
                  )).
pos_pp_typed_const_gt :-
        unit_test('Positive test PP typed const >',
                  (
                    format_to_codes("~p", ['>'@loc('max.ml', 0, 0, 0, 0, 0, 0):(int -> int -> bool)], Codes),
                    atom_codes('(>):(int -> int -> bool)', Codes)
                  )).

pos_pp_typed_id_x :-
        unit_test('Positive test PP typed id x',
                  (
                    format_to_codes("~p", [x@loc('max.ml', 0, 0, 0, 0, 0, 0):int], Codes),
                    atom_codes('x:int', Codes)
                  )).
pos_pp_typed_id_inc :-
        unit_test('Positive test PP typed id inc',
                  (
                    format_to_codes("~p", [inc@loc('max.ml', 0, 0, 0, 0, 0, 0):(int -> int)], Codes),
                    atom_codes('inc:(int -> int)', Codes)
                  )).

pos_pp_typed_app_plus :-
        unit_test('Positive test PP typed app +',
                  (
                    Exp = app(
                              '+'@loc('max.ml', 0, 0, 0, 0, 0, 0):(int -> int -> bool),
                              [1@loc('max.ml', 0, 0, 0, 0, 0, 0):int,
                               2@loc('max.ml', 0, 0, 0, 0, 0, 0):int]
                              )@loc('max.ml', 0, 0, 0, 0, 0, 0):bool,
                    format_to_codes("~p", [Exp], Codes),
                    atom_codes('(\n  (+):(int -> int -> bool)\n  1:int\n  2:int\n):bool', Codes)
                  )).
pos_pp_typed_app_gt :-
        unit_test('Positive test PP typed app >',
                  (
                    Exp = app(
                              '>'@loc('max.ml', 0, 0, 0, 0, 0, 0):(int -> int -> bool),
                              [x@loc('max.ml', 0, 0, 0, 0, 0, 0):int,
                               y@loc('max.ml', 0, 0, 0, 0, 0, 0):int]
                              )@loc('max.ml', 0, 0, 0, 0, 0, 0):bool,
                    format_to_codes("~p", [Exp], Codes),
                    atom_codes('(\n  (>):(int -> int -> bool)\n  x:int\n  y:int\n):bool', Codes)
                  )).


pos_pp_typed_abs_id :-
        unit_test('Positive test PP typed abs id',
                  (
                    Exp = abs([x@loc('id.ml', 0, 0, 0, 0, 0, 0):int],
                              x@loc('id.ml', 0, 0, 0, 0, 0, 0):int
                             )@loc('id.ml', 0, 0, 0, 0, 0, 0):(int -> int),
                    format_to_codes("~p", [Exp], Codes),
                    atom_codes('(fun\n  x:int\n->\n  x:int\n):(int -> int)', Codes)
                  )).
pos_pp_typed_abs_snd :-
        unit_test('Positive test PP typed abs snd',
                  (
                    Exp = abs([x@loc('snd.ml', 0, 0, 0, 0, 0, 0):int, y@loc('snd.ml', 0, 0, 0, 0, 0, 0):int],
                              y@loc('snd.ml', 0, 0, 0, 0, 0, 0):int
                             )@loc('snd.ml', 0, 0, 0, 0, 0, 0):(int -> int -> int),
                    format_to_codes("~p", [Exp], Codes),
                    atom_codes('(fun\n  x:int\n  y:int\n->\n  y:int\n):(int -> int -> int)', Codes)
                  )).

pos_pp_typed_ite :-
        unit_test('Positive test PP typed ite',
                  (
                    Exp = ite(true@loc('max.ml', 0, 0, 0, 0, 0, 0):bool,
                              x@loc('max.ml', 0, 0, 0, 0, 0, 0):int,
                              y@loc('max.ml', 0, 0, 0, 0, 0, 0):int
                             )@loc('max.ml', 0, 0, 0, 0, 0, 0):int,
                    format_to_codes("~p", [Exp], Codes),
                    atom_codes('(if\n  true:bool\nthen\n  x:int\nelse\n  y:int\n):int', Codes)
                  )).

pos_pp_typed_let :-
        unit_test('Positive test PP typed let',
                  (
                    Exp = let(x@loc('max.ml', 0, 0, 0, 0, 0, 0):int,
                                   1@loc('max.ml', 0, 0, 0, 0, 0, 0):int,
                                   x@loc('max.ml', 0, 0, 0, 0, 0, 0):int
                                  )@loc('max.ml', 0, 0, 0, 0, 0, 0):int,
                    format_to_codes("~p", [Exp], Codes),
                    atom_codes('(let\n  x:int\n=\n  1:int\nin\n  x:int\n):int', Codes)
                  )).

pos_pp_typed_max :-
        unit_test('Positive test PP typed max',
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

:-      pos_pp_typed_const_true,
        pos_pp_typed_const_10,
        pos_pp_typed_const_hola,
        pos_pp_typed_const_plus,
        pos_pp_typed_const_gt,
        pos_pp_typed_id_x,
        pos_pp_typed_id_inc,
        pos_pp_typed_app_plus,
        pos_pp_typed_app_gt,
        pos_pp_typed_abs_id,
        pos_pp_typed_abs_snd,
        pos_pp_typed_ite,
        pos_pp_typed_let,
        pos_pp_typed_max.


% % **********************************************************************
% % Pretty printing of named expressions

pos_pp_named_const_true :-
        unit_test('Positive test PP named const true',
                  (
                    format_to_codes("~p", [true@loc('max.ml', 0, 0, 0, 0, 0, 0):r:bool], Codes),
                    atom_codes('true:r:bool', Codes)
                  )).
pos_pp_named_const_10 :-
        unit_test('Positive test PP named const 10',
                  (
                    format_to_codes("~p", [10@loc('max.ml', 0, 0, 0, 0, 0, 0):r:int], Codes),
                    atom_codes('10:r:int', Codes)
                  )).
pos_pp_named_const_hola :-
        unit_test('Positive test PP named const "hola"',
                  (
                    format_to_codes("~p", ["hola"@loc('max.ml', 0, 0, 0, 0, 0, 0):str:string], Codes),
                    atom_codes('"hola":str:string', Codes)
                  )).
pos_pp_named_const_plus :-
        unit_test('Positive test PP named const +',
                  (
                    format_to_codes("~p", ['+'@loc('max.ml', 0, 0, 0, 0, 0, 0):plus:(plusa:int -> plusb:(plusba:int -> plusbb:int))], Codes),
                    atom_codes('(+):plus:(plusa:int -> plusb:(plusba:int -> plusbb:int))', Codes)
                  )).
pos_pp_named_const_gt :-
        unit_test('Positive test PP named const >',
                  (
                    format_to_codes("~p", ['>'@loc('max.ml', 0, 0, 0, 0, 0, 0):'>_c_ret':('>_c_reta':int -> '>_c_retb':('>_c_retba':int -> '>_c_retbb':bool))], Codes),
                    atom_codes('(>):>_c_ret:(>_c_reta:int -> >_c_retb:(>_c_retba:int -> >_c_retbb:bool))', Codes)
                  )).

pos_pp_named_id_x :-
        unit_test('Positive test PP named id x',
                  (
                    format_to_codes("~p", [x@loc('max.ml', 0, 0, 0, 0, 0, 0):r:int], Codes),
                    atom_codes('x:r:int', Codes)
                  )).
pos_pp_named_id_inc :-
        unit_test('Positive test PP named id inc',
                  (
                    format_to_codes("~p", [inc@loc('max.ml', 0, 0, 0, 0, 0, 0):inc:(inc_fresha:int -> inc_freshb:int)], Codes),
                    atom_codes('inc:inc:(inc_fresha:int -> inc_freshb:int)', Codes)
                  )).

pos_pp_named_app_plus :-
        unit_test('Positive test PP named app +',
                  (
                    Exp = app(
                              '+'@loc('max.ml', 0, 0, 0, 0, 0, 0):'+_x':('+_xa':int -> '+_xb':('+_xba':int -> '+_xbb':int)),
                              [1@loc('max.ml', 0, 0, 0, 0, 0, 0):'+_xa':int,
                               2@loc('max.ml', 0, 0, 0, 0, 0, 0):'+_xba':int]
                              )@loc('max.ml', 0, 0, 0, 0, 0, 0):'+_xbb':bool,
                    format_to_codes("~p", [Exp], Codes),
                    atom_codes('(\n  (+):+_x:(+_xa:int -> +_xb:(+_xba:int -> +_xbb:int))\n  1:+_xa:int\n  2:+_xba:int\n):+_xbb:bool', Codes)
                  )).
pos_pp_named_app_gt :-
        unit_test('Positive test PP named app >',
                  (
                    Exp = app(
                              '>'@loc('max.ml', 0, 0, 0, 0, 0, 0):'>_c_ret':('>_c_reta':int -> '>_c_retb':('>_c_retba':int -> 'c_ret':bool)),
                              [x2@loc('max.ml', 0, 0, 0, 0, 0, 0):'>_c_reta':int,
                               y3@loc('max.ml', 0, 0, 0, 0, 0, 0):'>_c_retba':int]
                              )@loc('max.ml', 0, 0, 0, 0, 0, 0):'c_ret':bool,
                    format_to_codes("~p", [Exp], Codes),
                    atom_codes('(\n  (>):>_c_ret:(>_c_reta:int -> >_c_retb:(>_c_retba:int -> c_ret:bool))\n  x2:>_c_reta:int\n  y3:>_c_retba:int\n):c_ret:bool', Codes)
                  )).

pos_pp_named_abs_id :-
        unit_test('Positive test PP named abs id',
                  (
                    Exp = abs([x@loc('id.ml', 0, 0, 0, 0, 0, 0):x:int],
                              x@loc('id.ml', 0, 0, 0, 0, 0, 0):r:int
                             )@loc('id.ml', 0, 0, 0, 0, 0, 0):id:(x:int -> r:int),
                    format_to_codes("~p", [Exp], Codes),
                    atom_codes('(fun\n  x:x:int\n->\n  x:r:int\n):id:(x:int -> r:int)', Codes)
                  )).
pos_pp_named_abs_snd :-
        unit_test('Positive test PP named abs snd',
                  (
                    Exp = abs([x@loc('snd.ml', 0, 0, 0, 0, 0, 0):x:int, y@loc('snd.ml', 0, 0, 0, 0, 0, 0):y:int],
                              y@loc('snd.ml', 0, 0, 0, 0, 0, 0):fresh2:int
                             )@loc('snd.ml', 0, 0, 0, 0, 0, 0):snd:(x:int -> fresh1:(y:int -> fresh2:int)),
                    format_to_codes("~p", [Exp], Codes),
                    atom_codes('(fun\n  x:x:int\n  y:y:int\n->\n  y:fresh2:int\n):snd:(x:int -> fresh1:(y:int -> fresh2:int))', Codes)
                  )).

pos_pp_named_ite :-
        unit_test('Positive test PP named ite',
                  (
                    Exp = ite(true@loc('max.ml', 0, 0, 0, 0, 0, 0):c_ret:bool,
                              x@loc('max.ml', 0, 0, 0, 0, 0, 0):ret:int,
                              y@loc('max.ml', 0, 0, 0, 0, 0, 0):ret:int
                             )@loc('max.ml', 0, 0, 0, 0, 0, 0):ret:int,
                    format_to_codes("~p", [Exp], Codes),
                    atom_codes('(if\n  true:c_ret:bool\nthen\n  x:ret:int\nelse\n  y:ret:int\n):ret:int', Codes)
                  )).

pos_pp_named_let :-
        unit_test('Positive test PP named let',
                  (
                    Exp = let(x@loc('max.ml', 0, 0, 0, 0, 0, 0):x:int,
                                   1@loc('max.ml', 0, 0, 0, 0, 0, 0):x:int,
                                   x@loc('max.ml', 0, 0, 0, 0, 0, 0):vlet:int
                                  )@loc('max.ml', 0, 0, 0, 0, 0, 0):vlet:int,
                    format_to_codes("~p", [Exp], Codes),
                    atom_codes('(let\n  x:x:int\n=\n  1:x:int\nin\n  x:vlet:int\n):vlet:int', Codes)
                  )).

pos_pp_named_max :-
        unit_test('Positive test PP named max',
                  (
                    Exp = let('max1'@loc('max.ml', 0, 0, 0, 0, 0, 0):max1:(x2:int -> max1_f4:(y3:int -> ret:int)),
                              abs(['x2'@loc('max.ml', 0, 0, 0, 0, 0, 0):x2:int,
                                   'y3'@loc('max.ml', 0, 0, 0, 0, 0, 0):y3:int
                                   ],
                                  ite(app('>'@loc('max.ml', 0, 0, 0, 0, 0, 0):'>_c_ret':('>_c_reta':int -> '>_c_retb':('>_c_retba':int -> c_ret:bool)),
                                          ['x2'@loc('max.ml', 0, 0, 0, 0, 0, 0):'>_c_reta':int,
                                           'y3'@loc('max.ml', 0, 0, 0, 0, 0, 0):'>_c_retba':int
                                           ]
                                         )@loc('max.ml', 0, 0, 0, 0, 0, 0):c_ret:bool,
                                      'x2'@loc('max.ml', 0, 0, 0, 0, 0, 0):ret:int,
                                      'y3'@loc('max.ml', 0, 0, 0, 0, 0, 0):ret:int
                                      )@loc('max.ml', 0, 0, 0, 0, 0, 0):ret:int
                                 )@loc('max.ml', 0, 0, 0, 0, 0, 0):max1:(x2:int -> max1_f4:(y3:int -> ret:int)),
                              app('max1'@loc('max.ml', 0, 0, 0, 0, 0, 0):max1:(max1_vleta:int -> max1_f4:(max1_vletba:int -> vlet:int)),
                                  [3@loc('max.ml', 0, 0, 0, 0, 0, 0):max1_vleta:int,
                                   1@loc('max.ml', 0, 0, 0, 0, 0, 0):max1_vletba:int
                                  ]
                                 )@loc('max.ml', 0, 0, 0, 0, 0, 0):vlet:int
                             )@loc('max.ml', 0, 0, 0, 0, 0, 0):vlet:int,
                    format_to_codes("~p", [Exp], Codes),
                    atom_codes('(let\n  max1:max1:(x2:int -> max1_f4:(y3:int -> ret:int))\n=\n  (fun\n    x2:x2:int\n    y3:y3:int\n  ->\n    (if\n      (\n        (>):>_c_ret:(>_c_reta:int -> >_c_retb:(>_c_retba:int -> c_ret:bool))\n        x2:>_c_reta:int\n        y3:>_c_retba:int\n      ):c_ret:bool\n    then\n      x2:ret:int\n    else\n      y3:ret:int\n    ):ret:int\n  ):max1:(x2:int -> max1_f4:(y3:int -> ret:int))\nin\n  (\n    max1:max1:(max1_vleta:int -> max1_f4:(max1_vletba:int -> vlet:int))\n    3:max1_vleta:int\n    1:max1_vletba:int\n  ):vlet:int\n):vlet:int', Codes)
                  )).

:-      pos_pp_named_const_true,
        pos_pp_named_const_10,
        pos_pp_named_const_hola,
        pos_pp_named_const_plus,
        pos_pp_named_const_gt,
        pos_pp_named_id_x,
        pos_pp_named_id_inc,
        pos_pp_named_app_plus,
        pos_pp_named_app_gt,
        pos_pp_named_abs_id,
        pos_pp_named_abs_snd,
        pos_pp_named_ite,
        pos_pp_named_let,
        pos_pp_named_max.
