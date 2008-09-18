:- module(core_swi_tools, [abs_file_name/3, format_to_atom/3]).

abs_file_name(Rel, Options, Abs) :- absolute_file_name(Rel, Options, Abs).

format_to_atom(Atom, Format, Params) :- format(atom(Atom), Format, Params).
