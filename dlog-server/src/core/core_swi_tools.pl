:- module(core_swi_tools, [abs_file_name/3, format_to_atom/3]).

abs_file_name(Rel, Options, Abs) :- catch(absolute_file_name(Rel, Options, Abs),_, (absolute_file_name(Rel, [file_errors(fail)|Options], Abs))).

format_to_atom(Atom, Format, Params) :- format(atom(Atom), Format, Params).

% :-set_prolog_flag(verbose_file_search,true).
