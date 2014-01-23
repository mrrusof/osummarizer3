% Modified: Thu Jan 23 17:45:28 CET 2014

%
% There are unit tests for the following submodules of osummarizer.
%
% 1. Well-formedness of typed expressions
% 2. Naming unit testing
% 3. Summarization unit testing
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
        print('FAILED\n'),
        false.


% **********************************************************************
% Well-formedness of typed expressions

pos_wf_t_e_const_plus :-
        unit_test('Positive test WF const plus',
                  wf_typed_exp('+'@loc('max.ml', 0, 0, 0, 0, 0, 0):(int -> int -> int))).
pos_wf_t_e_const_true :-
        unit_test('Positive test WF const true',
                  wf_typed_exp(true@loc('max.ml', 0, 0, 0, 0, 0, 0):bool)).
pos_wf_t_e_const_10 :-
        unit_test('Positive test WF const 10',
                  wf_typed_exp('10'@loc('max.ml', 0, 0, 0, 0, 0, 0):int)).
pos_wf_t_e_const_hola :-
        unit_test('Positive test WF const hola',
                  wf_typed_exp("hola"@loc('max.ml', 0, 0, 0, 0, 0, 0):string)).

pos_wf_t_e_nullary_id :-
        unit_test('Positive test WF nullary id',
                  wf_typed_exp(x@loc('max.ml', 0, 0, 0, 0, 0, 0):int)).
pos_wf_t_e_function_id :-
        unit_test('Positive test WF function id',
                  wf_typed_exp(inc@loc('max.ml', 0, 0, 0, 0, 0, 0):(int -> int -> int))).

pos_wf_t_e_abs :-
        unit_test('Positive test WF abs',
                  wf_typed_exp(
                     abs([x@loc('max.ml', 0, 0, 0, 0, 0, 0):int],
                         x@loc('max.ml', 0, 0, 0, 0, 0, 0):int
                         )@loc('max.ml', 0, 0, 0, 0, 0, 0):(int -> int)
                    )).

pos_wf_t_e_app :-
        unit_test('Positive test WF app',
                  wf_typed_exp(
                     app('>'@loc('max.ml', 0, 0, 0, 0, 0, 0):(int -> int -> bool),
                         [x@loc('max.ml', 0, 0, 0, 0, 0, 0):int,
                          y@loc('max.ml', 0, 0, 0, 0, 0, 0):int
                          ]
                        )@loc('max.ml', 0, 0, 0, 0, 0, 0):bool
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

:-      pos_wf_t_e_const_plus,
        pos_wf_t_e_const_true,
        pos_wf_t_e_const_10,
        pos_wf_t_e_const_hola,
        pos_wf_t_e_nullary_id,
        pos_wf_t_e_function_id,
        pos_wf_t_e_abs,
        pos_wf_t_e_app,
        pos_wf_t_e_ite,
        pos_wf_t_e_let,
        pos_wf_t_e_max.
