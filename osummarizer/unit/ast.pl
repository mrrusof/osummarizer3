:- ['../ast.pl'].
:- multifile ut/2.

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
