% Created  : Dec 2012
% Modified : Wed Jan 15 01:25:41 GMT 2014

:- use_module('../../utils/cmd_line_parse.pl').

user:runtime_entry(start) :-
	print('OSUMMARIZER: The summarizer for OCaml\n'),
	prolog_flag(argv, Argv),
	print(Argv), nl.
