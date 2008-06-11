:- use_module(library(terms), [term_variables_bag/2]).
:- use_module(library(lists), [prefix/2]).
:- use_module(library(system)).

term_variables_prefix(X, Vars) :-
	term_variables_bag(X, Vs),
	prefix(Vs, Vars).

transpose_ugraph(G, TG) :-
	transpose(G, TG).

% needed only for unfold_test.pl
%target(sp3).

current_directory_files(Fs) :-
	directory_files(., Fs).

file_mod_time(File, MT) :-
	file_property(File, mod_time(MT)).

:- ensure_loaded(unfold_test).
