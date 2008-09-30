:- module(dlog_test_outputs, 
			[pretty_print/1, html_print/1, xml_print/1, latex_print/1,	
			 pretty_print/2, html_print/2, xml_print/2, latex_print/2]).

:- use_module('../core/config', [target/1]).

:- target(sicstus) -> 
		use_module(dlog_test_sicstus_tools, [setup_and_call_cleanup/3, message_to_string/2]) 
		; true.

% :- type results --> list(results(fileName, readTime, result, fTotalTime)).
% :- type result --> list(result(options, compileTime, query, totalTime)) | fail | exception(E) | time_out. %read failure/error
% :- type query --> list(query(queryType, queryTime, queryResult, checkTime)) | fail | exception(E) | time_out. %compile failure/error
% :- type queryType --> instances(ConceptTerm) | instance(Name, ConceptTerm) | roleFillers(Name, RoleTerm) | relatedIndividuals(RoleTerm). 
			%| allConceptNames | allRoleNames | allIndividuals
% :- type queryResult --> pass | fail(answer, desired) | fail | exception(E) | time_out. %query failure/error	
% :- type answer --> true | false | individualSet(list(atom)) | individualPairSet(list(atom-atom)) 
%				%| conceptSet(list(list(atom))) | roleSet(list(list(atom)))
% :- type desired --> true | false | list(atom) | list(atom-atom)
%				%| list(list(atom)) | list(list(atom))
% :- type fileName --> atom.
% :- type readTime --> integer. %milliseconds
% :- type fTotalTime --> integer. %milliseconds
% :- type options --> list(name(value)).
% :- type compileTime --> integer. %milliseconds
% :- type totalTime --> integer. %milliseconds

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
pp_result([], _Out).
pp_result([result(Options, CompileTime, Queries, TotalTime) | Result], Out) :-
	format(Out, '~nOptions: ~q~n', [Options]),
	format(Out, '\tCompile time: ~D~n', [CompileTime]),
	pp_queries(Queries, Out),
	format(Out, '\tTotal processing time: ~D~n', [TotalTime]),
	pp_result(Result, Out).

pp_queries(fail, Out) :-
	write(Out, '!\tCompile failed.\n').
pp_queries(exception(E), Out) :-
	message_to_string(E, S), 
	format(Out, '!\tException while compiling: ~s~n\texception details: ~q~n', [S, E]).
pp_queries(time_out, Out) :-
	write(Out, '!\tTime-out while compiling.\n').
pp_queries([], _Out).
pp_queries([query(Query, QueryTime, QueryResult, CheckTime)| Queries], Out) :-
	format(Out, '\tQuery: ~q~n', [Query]),
	format(Out, '\t\tExecution time: ~D~n', [QueryTime]),
	pp_query(QueryResult, Out),
	format(Out, '\t\tTime to check answer correctness: ~D~n', [CheckTime]),
	pp_queries(Queries, Out).

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

