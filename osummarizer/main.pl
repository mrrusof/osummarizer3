% Modified: Wed Jan 29 15:48:54 CET 2014

:- use_module('ext/utils/cmd_line_parse.pl', [cmd_line_parse/3,
                                              print_cmd_option_list/0]).
:- use_module('ext/utils/misc.pl',           [all/1]).
:- ['osummarizer.pl'].

user:runtime_entry(start) :-
	print('OSUMMARIZER: The summarizer for OCaml\n'),
	prolog_flag(argv, Argv),
	print(Argv), nl,
        osummarizer_start(Argv).

osummarizer_start(Argv) :-
	CmdShape =
	[
         ( '-nowf',     [], bb_put(nowf, 1), 'Do not perform WF check.'),
         ( '-v',        [], start_log,       'Show intermediate results.'),
         ( '-d',        [], start_debug,     'Show intermediate computations.'),
         ( '-h',        [], print_help_halt, 'Print this help.')
	],
	cmd_line_parse(Argv, CmdShape, Files),
        all((
             member(F, Files),
             atom_codes(F, [C|_]), [C] == "-",
             format('ERROR: unknown option ~p\n', [F]),
             print_help_halt
            )),
	(   Files = [FileIn] ->
	    summarize(FileIn, no_file)
        ;   Files = [FileOut, FileIn] ->
            summarize(FileIn, FileOut)
	;   Files = [_|[_|_]] ->
            format('ERROR: there are too many file parameters ~p\n', [Files]),
            print_help_halt
        ;   print('ERROR: the input file is missing\n'),
	    print_help_halt
	).

print_help_halt :-
        print('\nUsage: osummarizer [ option params ] input_file [ output_file ]\n'),
        print_cmd_option_list,
        print('\n'),
	halt.