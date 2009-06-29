:- module(dlog_test_outputs, 
			[pretty_print/1, html_print/1, xml_print/1, latex_print/1,	
			 pretty_print/2, html_print/2, xml_print/2, latex_print/2]).

:- use_module('../core/config', [target/1]).

:- target(sicstus) -> 
		use_module(dlog_test_sicstus_tools, [setup_and_call_cleanup/3, message_to_string/2]) 
		; true.

% :- type results == list(results1).
% :- type results1 ---> results(fileName, readTime, result, fTotalTime).
% :- type result ---> 
%        success(list(result1)) | 
%        fail | exception(univ) | time_out. % read failure/error
% :- type result1 ---> result(options, compileTime, query, totalTime).
% :- type query ---> 
%        success(list(query1)) | 
%        fail | exception(univ) | time_out. % compile failure/error
% :- type query1 ---> query(queryType, queryTime, queryResult, checkTime).
% :- type queryType ---> 
%        instances(conceptTerm) | instance(iName, conceptTerm) | 
%        roleFillers(iName, roleTerm) | 
%        relatedIndividuals(roleTerm). 
%       %| allConceptNames | allRoleNames | allIndividuals.
% :- type queryResult ---> 
%        pass | 
%        fail(answer, desired) | % wrong answer
%        fail | exception(univ) | time_out. % query failure/error
% :- type answer ---> 
%        true | false | 
%        individualSet(list(atom)) |
%        individualPairSet(list(pair(atom, atom))). 
%      %| conceptSet(list(list(atom))) | roleSet(list(list(atom))).
% :- type desired.
% %		---> 
% %       true | false | 
% %       list(atom) | 
% %       list(pair(atom, atom)).
% %      %| list(list(atom)) | list(list(atom)).
% :- type fileName == atom.
% :- type readTime == integer. %milliseconds
% :- type fTotalTime == integer. %milliseconds
% :- type compileTime == integer. %milliseconds
% :- type iName == atom.
% :- type conceptTerm.
% :- type roleTerm.
% :- type queryTime == integer. %milliseconds
% :- type checkTime == integer. %milliseconds
% :- type totalTime == integer. %milliseconds
% :- type options == list(option).
% :- type option. %name(value)

pretty_print(Results) :-
	current_output(Out),
	pretty_print(Results, stream(Out)).

pretty_print(Results, Out) :-
	(	Out = stream(S)
	->	pretty_print0(Results, S)
	;	setup_and_call_cleanup(
			open(Out, write, S),
			pretty_print0(Results, S),
			close(S)
		)
	).
	
pretty_print0(Results, Out) :-
	write(Out, '\n                Test results\n'),
	write(Out, '        (all times are in milliseconds)\n\n'),	
	pretty_print1(Results, Out),
	nl(Out).
pretty_print1([], _Out).
pretty_print1([results(FileName, ReadTime, Result, FTotalTime)| Results], Out) :- 
	format(Out, '~nResults for file \'~a\':~n', [FileName]),
	format(Out, 'Read time: ~D~n', [ReadTime]),
	pp_result(Result, Out),
	format(Out, 'Total processing time for \'~a\': ~D~n~n', [FileName, FTotalTime]),
	pretty_print1(Results, Out).

pp_result(fail, Out) :-
	write(Out, '! Reading file failed.\n').
pp_result(exception(E), Out) :-
	message_to_string(E, S), 
	format(Out, '! Exception while reading file: ~s~nexception details: ~q~n', [S, E]).
pp_result(time_out, Out) :-
	write(Out, '! Time-out while reading file.\n').
pp_result(success(Result), Out) :- pp_result1(Result, Out).

pp_result1([], _Out).
pp_result1([result(Options, CompileTime, Queries, TotalTime) | Result], Out) :-
	format(Out, '~nOptions: ~q~n', [Options]),
	format(Out, '\tCompile time: ~D~n', [CompileTime]),
	pp_queries(Queries, Out),
	format(Out, '\tTotal processing time: ~D~n', [TotalTime]),
	pp_result1(Result, Out).

pp_queries(fail, Out) :-
	write(Out, '!\tCompile failed.\n').
pp_queries(exception(E), Out) :-
	message_to_string(E, S), 
	format(Out, '!\tException while compiling: ~s~n\texception details: ~q~n', [S, E]).
pp_queries(time_out, Out) :-
	write(Out, '!\tTime-out while compiling.\n').
pp_queries(success(Queries), Out) :- pp_queries1(Queries, Out).

pp_queries1([], _Out).
pp_queries1([query(Query, QueryTime, QueryResult, CheckTime)| Queries], Out) :-
	format(Out, '\tQuery: ~q~n', [Query]),
	format(Out, '\t\tExecution time: ~D~n', [QueryTime]),
	pp_query(QueryResult, Out),
	format(Out, '\t\tTime to check answer correctness: ~D~n', [CheckTime]),
	pp_queries1(Queries, Out).


pp_query(pass, Out) :-
	write(Out, '\t\tQuery passed.\n').
pp_query(fail, Out) :-
	write(Out, '!\t\tQuery execution failed.\n').
pp_query(exception(E), Out) :-
	message_to_string(E, S), 
	format(Out, '!\t\tException while executing query: ~s~n\texception details: ~q~n', [S, E]).
pp_query(time_out, Out) :-
	write(Out, '!\t\tTime-out while executing query.\n').
pp_query(fail(Answer, Desired), Out) :-
	write(Out, '-\t\tQuery failed.\n'),
	format(Out, '\t\tAnswer: ~q~n', [Answer]),
	format(Out, '\t\tDesired answer: ~q~n', [Desired]).






html_print(Results) :-
	current_output(Out),
	html_print(Results, stream(Out)).

html_print(Results, Out) :-
	(	Out = stream(S)
	->	html_print0(Results, S)
	;	setup_and_call_cleanup(
			open(Out, write, S),
			html_print0(Results, S),
			close(S)
		)
	).

html_print0(_Results, _Out). %TODO



xml_print(Results) :-
	current_output(Out),
	xml_print(Results, stream(Out)). %TODO

xml_print(_Results, _Out) :-
	(	Out = stream(S)
	->	xml_print0(Results, S)
	;	setup_and_call_cleanup(
			open(Out, write, S),
			xml_print0(Results, S),
			close(S)
		)
	).

xml_print0(_Results, _Out).



latex_print(Results) :-
	current_output(Out),
	latex_print(Results, stream(Out)).

latex_print(Results, Out) :-
	(	Out = stream(S)
	->	latex_print0(Results, S)
	;	setup_and_call_cleanup(
			open(Out, write, S),
			latex_print0(Results, S),
			close(S)
		)
	).


latex_print0(_Results, _Out). %TODO

