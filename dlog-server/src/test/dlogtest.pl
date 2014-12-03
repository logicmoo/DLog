% 2007. 09. 01.
% BME
:- module(dlogtest,[test/4]).

:- use_module(dlog, [load_KB0/6]).
:- use_module(prolog_translator, [dl2prolog/3]).
:- use_module(library(lists)).
:- use_module(library(system), [delete_file/2, file_property/2]).
:- use_module(library(timeout)).

temp_file_name('dlogtest_temp').

% test(+SuiteFile, +TexFile)
% the execution statistics of the DLog
% test suite specified in file SuiteFile
% is presented in TexFile
%
% Example:
% test('../test/standard.suite', '../test/dlog_stat.tex', '../test/dlog_runtime.tex', '../test/aggregate_table.tex').

test(SuiteFile, TexFile0, TexFile1, TexFile2) :-
	statistics(runtime, [T0,_]),
	init, 
	read_suite(SuiteFile), % read configuration
	execute_suite, % executing tests
	generate_totals, % generate aggregated results
	export_results(TexFile0, TexFile1, TexFile2), % exporting results as latex tables
	clean,
	statistics(runtime, [T1,_]),
	Time is (T1-T0)/1000,
	format('Total execution time of the DLog testing framework: ~d~n',[Time]).

init :-
	abolish(env:_),
	retractall(res:query_result(_,_,_,_,_)),
	retractall(res:load_time(_,_,_)),
	retractall(res:dlog_compilation_time(_,_,_)),
	retractall(res:prolog_compilation_time(_,_,_)),
	retractall(res:total_design_time(_,_,_)),
	retractall(res:file_size(_,_,_)),
	retractall(res:axiom_number(_,_,_)),
	retractall(res:instance_number(_,_,_)).

clean :-
	temp_file_name(FileName),
	atom_concat(FileName,'_tbox.pl', FileName1),
	delete_file(FileName1,[ignore]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Reading the configuration
%%%%%%%%%%%%%%%%%%%%%%%%%%%

read_suite(File) :-
	open(File, read, Stream),
	call_cleanup(read_suite0(Stream), close(Stream)).

read_suite0(Stream) :-
	repeat,
	read_term(Stream, Term, []),
	(
	  Term == end_of_file -> !
	;
	  assert(env:Term),
	  fail
	).

%%%%%%%%%%%%%%%%%%%%%%%%
% Executing a test suite
%%%%%%%%%%%%%%%%%%%%%%%%

execute_suite :-
	env:testcase(ID, Query, Dir, Files), % iterate through testcases
	process_testcase(ID, Query, Dir, Files),
	fail.
execute_suite.

process_testcase(ID, Query, Dir, Files) :-
	format('Executing testcase: ~p~n',[ID]),
	member(File, Files), % iterate through files
	load_dig(File, Dir, TBox, ABox, IBox, HBox), % load file
	execute_prolog(ID, File, Query, TBox, ABox, IBox, HBox),
	fail.
process_testcase(_, _, _, _).

filename(File, Dir, FileName) :-
	atom_concat(Dir, File, FileName0),
	atom_concat(FileName0, '.dig', FileName).

load_dig(File, Dir, TBox, ABox, IBox, HBox) :-
	filename(File, Dir, FileName), 
	format('Processing file: ~p~n',[FileName]),
	file_property(FileName, size(Size0)),
	Size is Size0/(1024*1024),
	safe_assert(res:file_size(File, [], Size)),
	statistics(runtime, [T0,_]),
	load_KB0(FileName, Statistics, TBox, ABox, IBox, HBox), !, % csúnya, de sajnos kell
	statistics(runtime, [T1,_]),
	T is (T1-T0)/1000,
	safe_assert(res:load_time(File, [], T)),
	memberchk(impliesc-IC, Statistics),
	memberchk(equalc-EC, Statistics),
	memberchk(related-RC, Statistics),
	memberchk(instanceof-OC, Statistics),
	TBox_axioms is IC+EC,
	ABox_axioms is RC+OC,
	safe_assert(res:axiom_number(File, [], TBox_axioms)),    
	safe_assert(res:instance_number(File, [], ABox_axioms)).

execute_prolog(ID, File, Query, TBox, ABox, IBox, HBox) :-
	get_compilation_parameters(Param), % iterate through execution parameters
	compile_and_load_kb(Param, File, TBox, ABox, IBox, HBox), 
	execute_compiled_query(ID, File, Param, Query),
	fail.
execute_prolog(_, _, _).

compile_and_load_kb(Param0, File, TBox, ABox, IBox, HBox) :-
	format('Compiling Knowledge Base with parameters: ~p~n',[Param0]),
	Param = [allinone(yes),statistics(yes)|Param0],
	temp_file_name(TFile),
	abolish(abox:_),
	statistics(runtime, [T0,_]),
	dl2prolog(TFile, kb(TBox, ABox, IBox, HBox), Param), % invoke the DL->Prolog translator
	statistics(runtime, [T1,_]),
	T is (T1-T0)/1000,
	safe_assert(res:dlog_compilation_time(File, Param0, T)),
	statistics(runtime, [T2,_]),
	atom_concat(TFile, '_tbox.pl', TFile1),
	compile(dlogtesting:TFile1), % compile the resulting Prolog code
	statistics(runtime, [T3,_]),
	T4 is (T3-T2)/1000,
	safe_assert(res:prolog_compilation_time(File, Param0, T4)).

get_compilation_parameters(Param) :-
	env:parameters(Params),
	member(Param-_, Params).

execute_compiled_query(ID, File, Param, Query) :-
	format('Executing Prolog code with query: ~p~n',[Query]),
%	QueryTerm =.. [Query, _Var],
	env:timeout(Time),
	time_out(dlog:statistics_call(Query, Result), Time, TResult),
	(
	  TResult == success->
	  once(select(runtime=OrigTime, Result, Rs)),
	  (
	    OrigTime == 0 ->
	    repeat_test(Query, NewTime) % in case of very quick test cases we repeat 1000
	  ;
	    average_test(Query, OrigTime, NewTime) % in case of normal cases we repeat 5 times
	  ),
	  project_result([runtime=NewTime|Rs] , FResult) % project results to specified statistics
	;
	  FResult = timeout
	),
	format('Execution results: ~p~n',[FResult]),
	assert(res:query_result(ID, File, Query, Param, FResult)). % asserting results

repeat_test(QueryTerm, Time) :-
	format('Repeating 0sec execution 1000 times...~n',[]),
	Goal = dlog:statistics_call(QueryTerm, _Result),
	statistics(runtime,[T0|_]),
	(
	  between(1,1000,_),
	  call(Goal),
	  fail
	;
	  true
	),
	statistics(runtime,[T1|_]),
	Time is (T1-T0)/(1000*1000).

average_test(QueryTerm, Orig, New) :-
	format('Repeating normal execution 4 more times...~n',[]),
	dlog:statistics_call(QueryTerm, Result1),
	memberchk(runtime=R1, Result1),
	dlog:statistics_call(QueryTerm, Result2),
	memberchk(runtime=R2, Result2),
	dlog:statistics_call(QueryTerm, Result3),
	memberchk(runtime=R3, Result3),
	dlog:statistics_call(QueryTerm, Result4),
	memberchk(runtime=R4, Result4),
	average([Orig, R1, R2, R3, R4], New0),
	New is New0/1000.

project_result(L, L1) :-
	env:statistics(Stat),
	project_result0(L, L1, Stat).

project_result0([], [], _).
project_result0([K=V|Ks], TS, Stat) :-
	(
	  (K == runtime; K == solnum)->
	  TS = [K=V|TS0]
	;
	  memberchk(K, Stat)->
	  TS = [K=V|TS0]
	;
	  TS0 = TS
	),
	project_result0(Ks, TS0, Stat).


%%%%%%%%%%%%%%%%%%%
% Exporting results
%%%%%%%%%%%%%%%%%%%
export_results(TexFile0, TexFile1, TexFile2) :-
	export_results0(dlog_file_stat_results('Properties of the test files','tesfilesprop'), TexFile0),
	export_results0(dlog_runtime_results('Results of the performance analysis','result'), TexFile1),
	export_results0(aggregate_results('Aggregate results','aggregate'), TexFile2).

export_results0(Goal, TexFile) :-
	open(TexFile, write, Stream),
	current_output(Out),
	set_output(Stream),
	write('%%\\ Automatically generated by the DLog system tester.\n'),	
	call_cleanup(call(Goal), (set_output(Out), close(Stream))).

table_head :-
	write('{\n\\renewcommand{\\arraystretch}{1.2}\n'),
	write('\\begin{table}[htb]\n'),
%	write('\\renewcommand{\\tabcolsep}{0.6em}\n'),
	write('\\begin{center}').

table_bottom(Caption, Label) :-
	write('\\end{tabular}\n'),
	write('\\end{center}\n'),
	format('\\caption{~p}~n',[Caption]),
	format('\\label{table:~p}~n',[Label]),
	write('\\end{table}\n}').

% query_file(-QFlat, -QNonFlat, -NumberOfColumns)
query_files(QFlat, QNonFlat, N) :-
	findall(Query-Files, env:testcase(_, Query, _, Files), QNonFlat),
	flatten(QNonFlat, [], QFlat),
	length(QFlat, N).

% result_line(+QFlat, +NameOfRow, +Param)
result_line(QF, Name, Param) :-
	(
	  Name == runtime->
	  format('& \\textbf{~p} ',[Name])
	;
	  format('& ~p ',[Name])
	),
	result_line0(QF, Name, Param),
	write('\\\\\n').

result_line0([], _Name, _Param).
result_line0([Q-F|Qs], Name, Param) :-
	once(res:query_result(_ID, F, Q, Param, Res)),
	(
	  memberchk(Name=S,Res)->
	  (
	    Name == runtime ->
	    format('&\\textbf{~2f}',[S])
	  ;
	    format('&~d',[S])
	  )
	;
	  Res == timeout,
	  format('&~p',[-])
	),
	result_line0(Qs, Name, Param).

%%%%%
% Runtime table
%%%%%
dlog_runtime_results(Caption, Label) :-
	table_head,
	runtime_table_body,
	table_bottom(Caption, Label).

runtime_table_body :-
	query_files(QFlat, QNonFlat, N),
	tabular_head(QNonFlat), 
	query_line(QNonFlat), % writes the first line in the table
	cline(N),
	testfile_line(QFlat), % writes the second line in the table
	result_line(QFlat, solnum, []), % writes the third line in the table
	write('\\hline\\hline\n'),
	table_body(QFlat).

% ----
% Head
% ----
tabular_head(List) :-
	write('\\begin{tabular}{|l|l|'),
	column_aligments(List),
	write('}\n'),
	write('\\hline\n').

column_aligments([]).
column_aligments([_Q-FS|Qs]) :-
	length(FS, L),
	write('|'),
	query_aligments(L),
	column_aligments(Qs).

query_aligments(0) :- !.
query_aligments(N) :-
	N > 0, 
	write('r|'),
	N1 is N-1,
	query_aligments(N1).

% ----------
% Query line
% ----------

query_line(List) :-
	write('& Query '),
	multicolumns(List),
	write('\\\\\n').

multicolumns([]).
multicolumns([Q-FS|Qs]) :-
	length(FS, Length),
	(
	  Qs == [] ->
	  multicolumn(last, Q, Length)
	;
	  multicolumn(nonlast, Q, Length)
	),
	multicolumns(Qs).

multicolumn(last, Q, L) :-
	!,
	format('& \\multicolumn{~p}{c|}{~p}',[L, Q]).
multicolumn(_, Q, L) :-
	format('& \\multicolumn{~p}{c||}{~p}',[L, Q]).


% -----
% cline
% -----
cline(N0) :-
	N is N0+2,
	format('\\cline{2-~p}',[N]),
	nl.

% -------------
% Testfile line
% -------------

testfile_line(QF) :-
	write('& Testfile '),
	testfile_line0(QF),
	write('\\\\\n'),
	write('\\hline\\hline\n').

testfile_line0([]).
testfile_line0([_Q-F|Fs]) :-
	underline_freed(F, F0), % for filenames containing underline(s)
	format('&~p',[F0]),
	testfile_line0(Fs).

underline_freed(Atom, Atom0) :-
	atom_codes(Atom, Cs),
	free_underline0(Cs, Cs1),
	atom_codes(Atom0, Cs1).

free_underline0([], []).
free_underline0([0'_|Cs], [0'\\,0'_|Cs0]) :-
	!, 
	free_underline0(Cs, Cs0).
free_underline0([C|Cs], [C|Cs0]):-
	free_underline0(Cs, Cs0).

% ----
% Body
% ----
table_body(QF) :-
	env:statistics(Stat),
	env:parameters(L),
	member(Param-Nick, L), % iterate through parameters
	write_block(Param, Nick, Stat, QF),
	fail.
table_body(_).

write_block(Param, Nick, Stat, QF) :-
	length(Stat, L),
	Stat = [S|Ss],
	multirow_line(L, Param, Nick, QF, S),
	block_stats(Param, Ss, QF),
	result_line(QF, runtime, Param),
	write('\\hline\n').

block_stats(Param, Stats, QF) :-
	member(Stat, Stats), % iterate through stats (loop, ancres, etc.)
	result_line(QF, Stat, Param), % writes a whole row
	fail.
block_stats(_, _, _).

multirow_line(N0, Param, Nick, QF, Stat) :-
	N is N0+1,
	format('\\multirow{~p}{*}{\\begin{sideways}~p\\end{sideways}} ',[N, Nick]),
	result_line(QF, Stat, Param).


%%%%%
% Files statistics table
%%%%%
dlog_file_stat_results(Caption, Label) :-
	table_head,
	file_stat_table_body,
	table_bottom(Caption, Label).

file_stat_table_body :-
	test_files(FList),
	tabular_head([dummy-FList]),
	zip(FList, ZFlist),
	testfile_line(ZFlist), % writes the first line in the table
	result_file_line(FList, file_size, size, []),
	result_file_line(FList, axiom_number, 'axioms (T)', []),
	result_file_line(FList, instance_number, 'axioms (A)', []),
	write('\\hline\\hline\n'),
	result_file_line(FList, load_time, load, []),
	write('\\hline\\hline\n'),
	file_stat_table_body(FList).	
	
test_files(FList) :-
	query_files(QFlat, _QNonFlat, _N),
	findall(F, member(_Q-F, QFlat), FList0),
	remove_duplicates(FList0, FList).

% ----
% Body
% ----
file_stat_table_body(FList) :-
	env:parameters(L),
	member(Param-Nick, L), % iterate through parameters
	write_file_stat_block(Param, Nick, FList),
	fail.
file_stat_table_body(_).

write_file_stat_block(Param, Nick, FList) :-
	multirow_file_stat_first_line(3, Param, Nick, FList),
	result_file_line(FList, prolog_compilation_time, compile, Param),
	result_file_line(FList, total_design_time, total, Param),
	write('\\hline\n').

multirow_file_stat_first_line(N, Param, Nick, FList) :-
	format('\\multirow{~p}{*}{\\begin{sideways}~p\\end{sideways}} ',[N, Nick]),
	result_file_line(FList, dlog_compilation_time, translate, Param).

result_file_line(Flist, Name, Nick, Param) :-
	(
	  Name == total_design_time ->
	  format('& \\textbf{~p} ',[Nick])
	;
	  format('& ~p ',[Nick])
	),
	result_file_line0(Flist, Name, Param),
	write('\\\\\n').

result_file_line0([], _Name, _Param).
result_file_line0([F|Fs], Name, Param) :-
	Term =.. [Name, F, Param, Res],
	once(call(res:Term)),
	(
	  Name == total_design_time->
	  format('&\\textbf{~2f}',[Res])
	;
	  float(Res) ->
	  format('&~2f',[Res])
	;
	  format('&~d',[Res])
	),
	result_file_line0(Fs, Name, Param).

%%%%%
% Agregate table
%%%%%
aggregate_results(Caption, Label) :-
	table_head,
	aggregate_table_body,
	table_bottom(Caption, Label).

aggregate_table_body :-
	query_files(QFlat, QNonFlat, N),
	tabular_head(QNonFlat), 
	query_line(QNonFlat), % writes the first line in the table
	cline(N),
	testfile_line(QFlat), % writes the second line in the table
	aggregate_table_body(QFlat).

aggregate_table_body(QFlat) :-
	env:competition(L0),
	member(System, ['DLog'|L0]), % iterate through reasoning systems
	write_aggregate_block(System, QFlat),
	fail.
aggregate_table_body(_).

write_aggregate_block(System, QFlat) :-
	multirow_line(3, System, QFlat),
	result_aggregate_line(QFlat, runtime, System),
	result_aggregate_line(QFlat, total, System),
	write('\\hline\n').

multirow_line(N, System, QFlat) :-
	format('\\multirow{~p}{*}{\\begin{sideways}~p\\end{sideways}} ',[N, System]),
	result_aggregate_line(QFlat, load, System).

result_aggregate_line(QFlat, Name, System) :-
	(
	  Name == total, System == 'DLog'->
	  format('& \\textbf{~p} ',[Name])
	;
	  format('& ~p ',[Name])
	),
	result_aggregate_line0(QFlat, Name, System),
	write('\\\\\n').

result_aggregate_line0([], _Name, _System).
result_aggregate_line0([Q-F|Qs], Name, System) :-
	(
	  System == 'DLog' ->
	  (
	    Name == load ->
	    once(res:total_design_time(F, [], Res))
	  ;
	    Name == runtime ->
	    get_best_runtime(F, Q, Res)
	  ;
	    % total
	    once(res:total_design_time(F, [], Res0)),
	    get_best_runtime(F, Q, Res1),
	    safe_add(Res1, Res0, Res)
	  )
	;
	  (
	    (Name == load ; Name == runtime) ->
	    once(foreign_result(System, Name, F, Q, Res))
	  ;
	    once(foreign_result(System, load, F, Q, Res0)),
	    once(foreign_result(System, runtime, F, Q, Res1)),
	    safe_add(Res1, Res0, Res)
	  )
	),
	(
	  number(Res), Name==total, System=='DLog' ->
	  format('&\\textbf{~2f}',[Res])
	;
	  number(Res) ->
	  format('&~2f',[Res])
	;
	  format('&~p',[-])
	),
	result_aggregate_line0(Qs, Name, System).


%%%%%%%%%%%%%%%%%
% auxiliary preds
%%%%%%%%%%%%%%%%%

safe_assert(G) :-
	(
	  call(G)->
	  true
	;
	  assert(G)
	).

between(A,B,A) :-
	A =< B.
between(A,B,I):-
	A < B,
	A1 is A+1,
	between(A1,B,I).

% average(+L, -Avg),
average([], 0) :- !.
average([E], E) :- !.
average([E1, E2], Avg) :- !,
	Avg is (E1+E2)/2.
average(L, Avg) :- % L must have at-least 3 elements
	min_list(L, Min),
	max_list(L, Max),
	select(Min, L, L0),
	select(Max, L0, L1),
	sum_list(L1, S),
	length(L1, Length),
	Avg is S / Length, !.

flatten([], Gy, Gy).
flatten([Q-FS|Qs], Gy, QF) :-
	findall(Q-M, member(M, FS), L),
	append(Gy, L, Gy1),
	flatten(Qs, Gy1, QF).

zip([], []).
zip([F|Fs], [z-F|Fs0]) :-
	zip(Fs, Fs0).

generate_totals :-
	test_files(FList),
	generate_totals0(FList).

generate_totals0([]).
generate_totals0([F|Fs]) :-
	generate_total(F),
	generate_totals0(Fs).

generate_total(File) :-
	env:parameters(L),
	member(Param-_Nick, L), % iterate through parameters
	generate_total0(File, Param),
	fail.
generate_total(_).

generate_total0(File, Param) :-
	res:load_time(File, [], LT),
	res:dlog_compilation_time(File, Param, DCT),
	res:prolog_compilation_time(File, Param, PCT),
	Res is LT+DCT+PCT,
	safe_assert(res:total_design_time(File, Param, Res)).

safe_add(timeout, _B, _C) :-
	!.
safe_add(A, B, C) :-
	C is A+B.

get_best_runtime(F, Q, Runtime) :-
	once(res:query_result(_ID, F, Q, [], Runtime0)),
	(
	  Runtime0 == timeout ->
	  Runtime = timeout
	;
	  memberchk(runtime=Runtime, Runtime0)
	).

%%%%%%%%%%%%%%%%%%
% Kaon2
%%%%%%%%%%%%%%%%%%
foreign_result('KAON2', load, c10, 'Good'(_), 0.445559).
foreign_result('KAON2', runtime, c10, 'Good'(_), 0.722314).
foreign_result('KAON2', load, c20, 'Good'(_), timeout).
foreign_result('KAON2', runtime, c20, 'Good'(_), timeout).
foreign_result('KAON2', load, c100, 'Good'(_), timeout).
foreign_result('KAON2', runtime, c100, 'Good'(_), timeout).
foreign_result('KAON2', load, c1000, 'Good'(_), timeout).
foreign_result('KAON2', runtime, c1000, 'Good'(_), timeout).
foreign_result('KAON2', load, c10000, 'Good'(_), timeout).
foreign_result('KAON2', runtime, c10000, 'Good'(_), timeout).

foreign_result('KAON2', load, n12, 'Good'(_), 0.47).
foreign_result('KAON2', runtime, n12, 'Good'(_), 0.49).
foreign_result('KAON2', load, n200, 'Good'(_), 0.63).
foreign_result('KAON2', runtime, n200, 'Good'(_), 2.13).
foreign_result('KAON2', load, n1200, 'Good'(_), 1.05).
foreign_result('KAON2', runtime, n1200, 'Good'(_), 67.51).
foreign_result('KAON2', load, n8000, 'Good'(_), 2.44).
foreign_result('KAON2', runtime, n8000, 'Good'(_), 425.1735).

%%%%%%%%%%%%%%%%%%
% RacerPro
%%%%%%%%%%%%%%%%%%
foreign_result('RacerPro', load, c10, 'Good'(_), 0.01).
foreign_result('RacerPro', setup, c10, 'Good'(_), 0.00).
foreign_result('RacerPro', runtime, c10, 'Good'(_), 0.07).

foreign_result('RacerPro', load, c20, 'Good'(_), 0.02).
foreign_result('RacerPro', setup, c20, 'Good'(_), 0.01).
foreign_result('RacerPro', runtime, c20, 'Good'(_), 0.06).

foreign_result('RacerPro', load, c100, 'Good'(_), 0.05).
foreign_result('RacerPro', setup, c100, 'Good'(_), 0.01).
foreign_result('RacerPro', runtime, c100, 'Good'(_), 0.14).

foreign_result('RacerPro', load, c1000, 'Good'(_), 0.54).
foreign_result('RacerPro', setup, c1000, 'Good'(_), 0.11).
foreign_result('RacerPro', runtime, c1000, 'Good'(_), 1.55).

foreign_result('RacerPro', load, c10000, 'Good'(_), 4.68).
foreign_result('RacerPro', setup, c10000, 'Good'(_), 2.02).
foreign_result('RacerPro', runtime, c10000, 'Good'(_), 76.77).

foreign_result('RacerPro', load, n12, 'Good'(_), 0.03).
foreign_result('RacerPro', setup, n12, 'Good'(_), 0.00).
foreign_result('RacerPro', runtime, n12, 'Good'(_), 0.10).

foreign_result('RacerPro', load, n200, 'Good'(_), 0.08).
foreign_result('RacerPro', setup, n200, 'Good'(_), 0.04).
foreign_result('RacerPro', runtime, n200, 'Good'(_), 0.43).

foreign_result('RacerPro', load, n1200, 'Good'(_), 0.67).
foreign_result('RacerPro', setup, n1200, 'Good'(_), 0.51).
foreign_result('RacerPro', runtime, n1200, 'Good'(_), 1.52).

foreign_result('RacerPro', load, n8000, 'Good'(_), 6.10).
foreign_result('RacerPro', setup, n8000, 'Good'(_), 2.75).
foreign_result('RacerPro', runtime, n8000, 'Good'(_), 21.44).


%%%%%%%%%%%%%%%%%%
% Pellet
%%%%%%%%%%%%%%%%%%

foreign_result('Pellet', load, c10, 'Good'(_), 1.26).
foreign_result('Pellet', setup, c10, 'Good'(_), 0.02).
foreign_result('Pellet', runtime, c10, 'Good'(_), 0.18).

foreign_result('Pellet', load, c20, 'Good'(_), 1.29).
foreign_result('Pellet', setup, c20, 'Good'(_), 0.02).
foreign_result('Pellet', runtime, c20, 'Good'(_), 0.30).

foreign_result('Pellet', load, c100, 'Good'(_), 1.37).
foreign_result('Pellet', setup, c100, 'Good'(_), 0.02).
foreign_result('Pellet', runtime, c100, 'Good'(_), 1.03).

foreign_result('Pellet', load, c1000, 'Good'(_), 1.90).
foreign_result('Pellet', setup, c1000, 'Good'(_), 0.95).
foreign_result('Pellet', runtime, c1000, 'Good'(_), 391.41).

foreign_result('Pellet', load, c10000, 'Good'(_), timeout).
foreign_result('Pellet', setup, c10000, 'Good'(_), timeout).
foreign_result('Pellet', runtime, c10000, 'Good'(_), timeout).

foreign_result('Pellet', load, n12, 'Good'(_), 1.31).
foreign_result('Pellet', setup, n12, 'Good'(_), 0.02).
foreign_result('Pellet', runtime, n12, 'Good'(_), 0.31).

foreign_result('Pellet', load, n200, 'Good'(_), 1.53).
foreign_result('Pellet', setup, n200, 'Good'(_), 0.04).
foreign_result('Pellet', runtime, n200, 'Good'(_), 0.75).

foreign_result('Pellet', load, n1200, 'Good'(_), 2.07).
foreign_result('Pellet', setup, n1200, 'Good'(_), 0.11).
foreign_result('Pellet', runtime, n1200, 'Good'(_), 2.39).

foreign_result('Pellet', load, n8000, 'Good'(_), 5.89).
foreign_result('Pellet', setup, n8000, 'Good'(_), 0.67).
foreign_result('Pellet', runtime, n8000, 'Good'(_), 23.29).


%foreign_result('KAON2', load, c11, 'Good', 0.034831).
%foreign_result('KAON2', run, c11, 'Good', 0.680399).
%foreign_result('KAON2', load, c12, 'Good', 0.035801).
%foreign_result('KAON2', run, c12, 'Good', 3.514495).
%foreign_result('KAON2', load, c13, 'Good', 0.035843).
%foreign_result('KAON2', run, c13, 'Good', 16.186322).
%foreign_result('KAON2', load, c14, 'Good', 0.034865).
%foreign_result('KAON2', run, c14, 'Good', 17.038848).
%foreign_result('KAON2', load, c15, 'Good', 0.038102).
%foreign_result('KAON2', run, c15, 'Good', 309.916518).
%foreign_result('KAON2', load, _, 'Good', timeout).
%foreign_result('KAON2', run, _, 'Good', timeout).
