% Wed Jan 22 00:51:46 CET 2014

%
% There are three kinds of unit tests.
%
% 1. WF unit testing
% 2. Naming unit testing
% 3. Summarization unit testing
%

:- ['osummarizer.pl'].


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

unit_test_wf_t_e_const_true :-
        unit_test('Test WF const true',
                  wf_typed_exp(true@loc('max.ml', 0, 0, 0, 0, 0, 0):bool)).
unit_test_wf_t_e_const_10 :-
        unit_test('Test WF const 10',
                  wf_typed_exp('10'@loc('max.ml', 0, 0, 0, 0, 0, 0):int)).
unit_test_wf_t_e_const_hola :-
        unit_test('Test WF const hola',
                  wf_typed_exp("hola"@loc('max.ml', 0, 0, 0, 0, 0, 0):string)).

unit_test_wf_t_e_id :-
        unit_test('Test WF id',
                  wf_typed_exp('x2'@loc('max.ml', 0, 0, 0, 0, 0, 0):int)).

unit_test_wf_t_e_abs :-
        unit_test('Test WF abs',
                  wf_typed_exp(
                     abs(['x2'@loc('max.ml', 0, 0, 0, 0, 0, 0):int],
                         'x2'@loc('max.ml', 0, 0, 0, 0, 0, 0):int
                         )@loc('max.ml', 0, 0, 0, 0, 0, 0):(int -> int -> int)
                    )).

unit_test_wf_t_e_app :-
        unit_test('Test WF app',
              wf_typed_exp(
                     app('>'@loc('max.ml', 0, 0, 0, 0, 0, 0):(int -> int -> bool),
                         ['x2'@loc('max.ml', 0, 0, 0, 0, 0, 0):int,
                          'y3'@loc('max.ml', 0, 0, 0, 0, 0, 0):int
                          ]
                        )@loc('max.ml', 0, 0, 0, 0, 0, 0):bool
                    )).

unit_test_wf_t_e_ite :-
        unit_test('Test WF ite',
                  wf_typed_exp(
                               ite(true@loc('max.ml', 0, 0, 0, 0, 0, 0):bool,
                                   'x2'@loc('max.ml', 0, 0, 0, 0, 0, 0):int,
                                   'y3'@loc('max.ml', 0, 0, 0, 0, 0, 0):int
                                   )@loc('max.ml', 0, 0, 0, 0, 0, 0):int
                 )).

unit_test_wf_t_e_let :-
        unit_test('Test WF let',
                  wf_typed_exp(let(x@loc('max.ml', 0, 0, 0, 0, 0, 0):int,
                                   1@loc('max.ml', 0, 0, 0, 0, 0, 0):int,
                                   x@loc('max.ml', 0, 0, 0, 0, 0, 0):int
                                  )@loc('max.ml', 0, 0, 0, 0, 0, 0):int
                              )).

unit_test_wf_t_e_max :-
        unit_test('Test WF max',
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

:-      unit_test_wf_t_e_const_true,
        unit_test_wf_t_e_const_10,
        unit_test_wf_t_e_const_hola,
        unit_test_wf_t_e_id,
        unit_test_wf_t_e_abs,
        unit_test_wf_t_e_app,
        unit_test_wf_t_e_ite,
        unit_test_wf_t_e_let,
        unit_test_wf_t_e_max.
