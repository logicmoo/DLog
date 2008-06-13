:- op(900, fx, time).
:- op(900, xfx, time).

:- use_module(library(lists)).

:- ensure_loaded(unfold).
:- ensure_loaded(ainterp).
:- use_module(tbox_annotator).

:- op(1200, xfx, :--).

% Testing

test_base :-
	working_directory(D0, tests),
	call_cleanup(current_directory_files(Fs), working_directory(_, D0)),
	runtimes_file_name('', F),
	tell(F), 
	call_cleanup(test_all1(Fs, true, base), told).

test_nonbase :-
	working_directory(D0, tests),
	call_cleanup(current_directory_files(Fs), working_directory(_, D0)),
	test_all1(Fs, false, nobs).

test_compat :-
	working_directory(D0, tests),
	call_cleanup(current_directory_files(Fs), working_directory(_, D0)),
	runtimes_file_name('cmpt_', F),
	tell(F), 
	call_cleanup(test_all1(Fs, true, cmpt), told).

runtimes_file_name(Type, F) :-
	target(T),
	atom_concat(runtimes_, Type, F0),
	atom_concat(F0, T, F1),
	atom_concat(F1, '.txt', F).		 

test_all1(Fs, Save, OptSet) :-
	sort(Fs, SFs),
	init_cumulate_time,
	statistics(runtime,[T0|_]),
	(   member(File, SFs),
	    atom_concat(File0, '.pl', File),
	    test0(File0, Save, OptSet),
	    fail
	;   statistics(runtime, [T1|_]),
	    T is T1-T0,
	    inform_cumulated_time(T)
	).

init_cumulate_time :-
	bb_put(cumulated, 0).

cumulate_time(Msec) :-
	bb_get(cumulated, T0),
	T1 is T0+Msec,
	bb_put(cumulated, T1).

inform_cumulated_time(T) :-
	bb_get(cumulated, Msec),
	Other is T-Msec,
	Format = 'Total unfold time: ~3d sec, total other runtime: ~3d sec\n',
	Args = [Msec,Other],
	nl(user_error),
	write(user_error, '--------------------------------------------------------------------------\n'),
	format(user_error, Format, Args),
	write(user_error, '--------------------------------------------------------------------------\n'),
	format(Format, Args).

% option(ShortName, Base, OptsList)
option(dfl, base, []).
option(non, nobs, [primary(none)]).
option(min, base, [primary(min)]).
option(mid, nobs, [primary(mid)]).
option(max, base, [max_clauses_unfolded(30)]).
option(din, base, [unfold(inner)]).
option(den, base, [unfold(entry)]).
% option(dnr, nobs, [recdet(off)]).
option(cl9, nobs, [max_clauses_unfolded(9)]).
option(nnn, base, [primary(none),unfold(inner)]).
option(nrc, nobs, [remove_clauses_ai(off)]).
option(cpd, cmpt, [compat(on)]).
option(opt(Opts), nobs, Opts).
option(mcg(N),    nobs, [max_clauses_generated(N)]).

test(File) :-
	init_cumulate_time,
	test0(File, false, base).

testsave(File) :-
	init_cumulate_time,
	test0(File, true, base).

testsave(File, OptName) :-
	init_cumulate_time,
	test(File, OptName, true).

test(File, OptName) :-
	init_cumulate_time,
	test(File, OptName, false).

test0(File, Save, OptSet) :-
	option(Opt, OptSet, _), ground(Opt),
	test(File, Opt, Save), fail.
test0(_, _, _) :-
	nl.

input_file(user, IF) :-
	!, IF = user.
input_file(F, IF) :-
	atom_concat('tests/', F, IF0),
	atom_concat(IF0, '.pl', IF).

output_files(user, _OptName, _Opts, OFile, QRFile) :- !,
	OFile = user, QRFile = user.
output_files(File, OptName, Opts, OFile, QRFile) :-
	(   memberchk(compat(on), Opts) -> OutDir = 'output_cmpt/'
	;   OutDir = 'output/'
	),
	functor(OptName, OptName1, _),
	atom_concat(OutDir, File, OFile0),
	atom_concat(OFile0, '.', OFile1),
	atom_concat(OFile1, OptName1, OFile),
	atom_concat(OFile1, 'qres', QRFile).

:- dynamic baseres/3.

make_clean :-
	retractall(baseres(_,_,_)).

test(File, OptName, Save) :-
	option(OptName, _, Opts),
	opt_member(Opts, all, entryspec(EntrySpec)),
	input_file(File, IFile), 
	read_kb(IFile, Prog, ABoxSignature, ABox, Queries),
	format('% File ~w, options ~w', [File,OptName]),
%	ttyflush,
	bb_put(context, File-OptName),
	statistics(runtime, [T0|_]),
	(   unfold_predicates(prog(Prog,ABoxSignature,EntrySpec), Opts, uprog(UProgram0,PredInfo0)) ->
	    statistics(runtime, [T1|_]),
	    Msec is T1-T0,
	    cumulate_time(Msec),
	    format(', run time ~3d sec~n', [Msec])
	;   format(' ==> !! FAILED~n', []),
	    my_error('FAILED', []),
	    fail
	),	
	(   memberchk(compat(on), Opts) ->
	    annotated_preds(UProgram0, UProgram),
	    predinfo_with_no_query_preds(PredInfo0, PredInfo),
	    Rec = rec
	;   PredInfo = PredInfo0,
	    UProgram = UProgram0,
	    Rec = nonrec
	),
	PredInfo = info(QPs, AtomicPs, ANRPs, RecPs, _EntryPs, EntryOnlyPs),
	InfoGoal1 = format('% File ~w, options ~w~n', [File,OptName]),
	InfoGoal21 = (write('% Query preds:      '), print_arg(QPs), nl),
	InfoGoal22 = (write('% Atomic preds:     '), print_arg(AtomicPs), nl),
	InfoGoal23 = (write('% ANR preds:        '), print_arg(ANRPs), nl),
	InfoGoal24 = (write('% Recursive preds:  '), print_arg(RecPs), nl),
%	InfoGoal25 = (write('% Entry preds:      '), print_arg(EntryPs), nl),
	InfoGoal26 = (write('% Entry only preds: '), print_arg(EntryOnlyPs), nl),
%	InfoGoal2 = (InfoGoal21,InfoGoal22,InfoGoal23,InfoGoal24,InfoGoal25,InfoGoal26),
	InfoGoal2 = (InfoGoal21,InfoGoal22,InfoGoal23,InfoGoal24,InfoGoal26),
%	format('% File ~w, options ~w, run time ~3d sec~n',
%	       [File,OptName,Msec]),
%	InfoGoal2,
	output_files(File, OptName, Opts, OFile, QRFile),
	print_kb(Save, OFile, (InfoGoal1,InfoGoal2), UProgram, ABox, PredInfo),
	base_test_results(File, Prog, ABox, Queries, QRsBase),
	print_query_results(Save, QRFile, QRsBase),
	prog_to_iprog(UProgram, Rec, IUProgram),
	bb_put(undefined, warning),
	findall(QR, run_query(Queries, IUProgram, ABox, QR), QRs),
	(   QRsBase == QRs -> true
	;   member(Q-BR, QRsBase), memberchk(Q-CR, QRs), BR \== CR,
	    (   EntrySpec = pos -> \+ atom_concat(not_, _, Q)
	    ;   true
	    ),
	    my_error('Error: query: ~w, base result = ~w, current result = ~w', [Q, BR,CR]),
	    fail
	;   true
	).

atomic_preds_of_predinfo(info(_QPs, AtomicPs, _ANRPs, _RecPs, _EntryPs, _EntryOnlyPs), AtomicPs).

query_preds_of_predinfo(info(QPs, _AtomicPs, _ANRPs, _RecPs, _EntryPs, _EntryOnlyPs), QPs).

predinfo_with_no_query_preds(info(_QPs, AtomicPs, ANRPs, RecPs, EntryPs, EntryOnlyPs),
			     info([], AtomicPs, ANRPs, RecPs, EntryPs, EntryOnlyPs)).

base_test_results(Prog, ABox, Queries, QRsBase) :-
	prog_to_iprog(Prog, rec, IProg),
	bb_put(undefined, fail),
	findall(QR, run_query(Queries, IProg, ABox, QR), QRsBase).

base_test_results(user, Prog, ABox, Queries, QRsBase) :- !,
	base_test_results(Prog, ABox, Queries, QRsBase).
base_test_results(File, Prog, ABox, Queries, QRsBase) :-
	input_file(File, IFile),
	file_mod_time(IFile, MT),
	(   baseres(File, MT, QRsBase) -> true
	;   base_test_results(Prog, ABox, Queries, QRsBase),
	    asserta(baseres(File, MT, QRsBase))
	).

prog_to_iprog(Prog, DefaultRec, IProg) :-
	findall(Func-ICls,
		(   member(Func-Cls, Prog),
		    findall(ICl,
			    (   member(Cl, Cls),
				clause_to_iclause(Cl, DefaultRec, ICl)
			    ),
			    ICls)
		),
		IProg
	       ).

clause_to_iclause(cl(Head,Body,AMap), DefaultRec0, icl(Head,AL,IBody)) :-
	process_amap(AMap, AL),
	(   DefaultRec0 == nonrec -> DefaultRec = nonrec
	;   DefaultRec = rec(Head)
	),
	body_to_ibody(Body, DefaultRec, IBody, RBody, RBody).


cond_to_goals([], _, _) --> [].
cond_to_goals([Ancs|AncsL], Rec, G) -->
	{   Rec = rec(PHead) -> Ancs1 = rec(PHead,Ancs)
	;   Ancs1 = Ancs
	},
	[goal(G,Ancs1)],
	cond_to_goals(AncsL, Rec, G).	

body_to_ibody([], _DefaultRec, BGoals0, BGoals0, []).
body_to_ibody([Goal|Goals0], DefaultRec, BGoals, BGoals0, OGoals) :-
	(   binary_goal(Goal,_) -> BGoals = [Goal|BGoals1],
	    body_to_ibody(Goals0, DefaultRec, BGoals1, BGoals0, OGoals)
	;   Goal = goal(G,AncsL) ->
	    (   AncsL = rec(PHead,AncsL1) -> Rec = rec(PHead)
	    ;   AncsL1 = AncsL, Rec = DefaultRec
	    ),
	    (   atomic(AncsL1) ->
		OGoals = [Goal|OGoals1],
		body_to_ibody(Goals0, DefaultRec, BGoals, BGoals0, OGoals1)
	    ;   cond_to_goals(AncsL1, Rec, G, OGoals, OGoals1),
		body_to_ibody(Goals0, DefaultRec, BGoals, BGoals0, OGoals1)
	    )
	;   OGoals = [Goal|OGoals1],
	    body_to_ibody(Goals0, DefaultRec, BGoals, BGoals0, OGoals1)
	).			   

run_query(Queries, Prog, ABox, P0-QR) :-
	member(Q0, Queries),
	make_entry_goal(Q0, Q, P0, X),
	catch(
	      all_sols_of_query(Q, X, Prog, ABox, QR),
	      top_query,
	      QR = top
	     ).

make_entry_goal(Q, Q, P, X) :-
	functor(Q, P, 1),
	arg(1, Q, X).
	

% make_entry_goal(Q0, Opts, Q, P0, X) :-
% 	functor(Q0, P0, 1),
%  	opt_member(Opts, both, unfold(Mode)),
%  	(   Mode = inner -> P = P0
%  	;   atom_concat(P0, '$e', P)
%  	),
% 	functor(Q, P, 1),
% 	arg(1, Q0, X),
% 	arg(1, Q, X).

all_sols_of_query(Goal, X, Prog, ABox, QR) :-
%	Goal time
	findall(X, interp(Goal, X, Prog, ABox), QR0),
	sort(QR0, QR).

% Tbox input
read_kb(user, TBox, ABoxSignature, ABox, Queries) :- !,
	read_kb(TBox, ABoxSignature, ABox, Queries).
read_kb(File, TBox, ABoxSignature, ABox, Queries) :-
	see(File),
	call_cleanup(read_kb(TBox, ABoxSignature, ABox, Queries), seen).

read_kb(AProg, ABoxSignature, ABox, Queries) :-
	read_terms(Terms1, _),
	terms_to_aprog(Terms1, AProg),
	terms_to_abox(Terms1, ABox),
	terms_to_queries(Terms1, Queries),
	unzip1(ABox, ABoxSignature0),
	sort(ABoxSignature0, ABoxSignature).

all_functors_in_prog(AProg, SFs) :-
	findall(F, functor_in_prog(AProg, F), Fs),
	sort(Fs, SFs).

functor_in_prog(Prog, Func) :-
	member(Func-_, Prog).
functor_in_prog(Prog, Func) :-
	is_called_by(Func, _, Prog).

terms_to_aprog(Terms, AProg) :-
	bagof(Func-Cls,
	      bagof(Cl, rule_in_prog(Terms, Func, Cl), Cls),
	      Prog
	     ),
	annotated_preds(Prog, AProg).

terms_to_abox(Terms, ABox) :-
	bagof(Func-Cls,
	      bagof(Cl, fact_in_prog(Terms, Func, Cl), Cls),
	      ABox
	     ), !.
terms_to_abox(_, []).

terms_to_queries(Terms, Queries) :-
	findall(Q, query_in_prog(Terms, Q), Queries).	

query_in_prog(Terms, Goal) :-
	Term = (:- Goal),
	member(Term, Terms).

fact_in_prog(Terms, Name/Arity, Fact) :-
	member(Fact, Terms),
	Fact \= (_Head:-_),
	Fact \= (:-_),
	functor(Fact, Name, Arity).

rule_in_prog(Terms, Name/Arity, Cl) :-
	member((Head:-Body), Terms),
	functor(Head, Name, Arity),
	Cl = Body-Head.

read_terms(Terms, Vars) :-
	read(Term),
	(   Term == end_of_file -> Terms = []
	;   read_terms(Terms1, Vars),
	    add_term(Term, Vars, Terms1, Terms)
	).


add_term((Head :-- Body), Vars, Terms0, Terms) :- !,
        clause_to_litlist(Head, Body, LitList),
	findall(Cl, contrapositive_of(LitList, Cl), Terms1),
	terms_vars(Terms1, Vars, Terms0, Terms).
add_term((Head:-Body), Vars, Terms0, Terms) :- !,
	open_list_to_list(Body, BL),
	sort(BL, SBL),
	list_to_open_list(SBL, SBody),
	Term = (Head:-SBody),
	terms_vars([Term], Vars, Terms0, Terms).
add_term(Term, Vars, Terms0, Terms) :-
	terms_vars([Term], Vars, Terms0, Terms).

terms_vars([], _, Terms0, Terms0).
terms_vars([Term|Terms1], Vars, Terms0, Terms) :-
	term_variables_prefix(Term, Vars),
	(   select(Term1, Terms0, Terms2), Term == Term1 ->
	    Terms3 = [Term|Terms2]
	;   Terms3 = [Term|Terms0]
	),
	terms_vars(Terms1, Vars, Terms3, Terms).

contrapositive_of(L, (Head:-Body)) :-
	select(NH, L, BL),
	functor(NH, _, 1),
	negated_goal_pattern(NH, Head, _),
	sort(BL, SBL),
	list_to_open_list(SBL, Body).

clause_to_litlist(Head, Body, [NH|BL]) :-
	negated_goal_pattern(Head, NH, _),
	open_list_to_list(Body, BL).


list_to_open_list([], true).
list_to_open_list([G0|L], G) :-
	(   L = [] -> G = G0
	;   G = (G0,G1),
	    list_to_open_list(L, G1)
	).

open_list_to_list(true, L) :- !, L = [].
open_list_to_list((G0,G1), L) :-
	!, L = [G0|L1],
	open_list_to_list(G1, L1).
open_list_to_list(G0, [G0]).

% Interpreter

interp(Q, X, Prog, ABox) :-
	interp([goal(Q,[])], Prog, ABox, [], []),
	(   var(X) -> raise_exception(top_query)
	;   true
	).

interp([], _, _, _, _).
interp([Goal|Goals], Prog, ABox, AL, LEL) :-
	interp(Goal, Goals, Prog, ABox, AL, LEL).

interp('$checkanc'(A), Goals, Prog, ABox, AL, LEL) :- !,
	member(A, AL),
	interp(Goals, Prog, ABox, AL, LEL).
interp('$checkanc'(A, B), Goals, Prog, ABox, AL, LEL) :- !,
	(   A = B -> true
	;   memberchk(A, AL)
	),
	interp(Goals, Prog, ABox, AL, LEL).
interp(goal(G,Ancs), Goals, Prog, ABox, AL, LEL0) :-
	(   ground(G) ->
	    (   interp_goal0(G, Prog, ABox, Ancs, LEL0)
	    ->  true
	    )
	;   interp_goal0(G, Prog, ABox, Ancs, LEL0)
	),
	interp(Goals, Prog, ABox, AL, LEL0).

interp_goal0(G, Prog, ABox, Ancs, LEL0) :-
	trace_interp(call, fail, G, Ancs),
	interp_goal(G, Prog, ABox, Ancs, LEL0),
	trace_interp(exit, redo, G, '').

% interp_goal(G, _Prog, ABox, abox, _LEL0) :- !,
% 	interp_abox(G, ABox).
interp_goal(G, _Prog, _ABox, _AL, LEL0) :-
 	member(G0, LEL0), G0 == G, !, fail.
% interp_goal(G, _Prog, _ABox, AL, _LEL0) :-
%	negated_goal_pattern(G, NG, _),
%	member(NG, AL).
interp_goal(G, Prog, ABox, AL, LEL0) :-
	AL \== abox,
	(   AL = rec(PGoal,AL1) ->
	    LEL1 = [PGoal|LEL0]
	;   LEL1 = LEL0, AL1 = AL
	),
	(   negated_goal_pattern(G, NG, _),
	    member(NG, AL1)
	;   clause_in_prog(G, Prog, ABox, AL1, Body),
	    interp(Body, Prog, ABox, AL1, LEL1)
	).
interp_goal(G, _Prog, ABox, _AL, _LEL0) :-
	interp_abox(G, ABox).

trace_interp(In, Fail, Goal, Ancs) :-
	bb_get(itrace, on), !,
	(   write(user_error, In:Goal), write(user_error, ' '), write(user_error, Ancs), nl(user_error)
	;   write(user_error, Fail:Goal), nl(user_error), fail
	).
trace_interp(_In, _Fail, _Goal, _Ancs).

clause_in_prog(Head, Prog, ABox, AL, Body) :-
	functor(Head, Name, Arity),
	Func = Name/Arity,
	(   memberchk(Func-Cls, Prog) -> 
	    member(Cl0, Cls),
	    copy_term(Cl0, icl(Head, AL, Body))
	;   bb_get(undefined, warning),
	    atom_concat(_, $, Name),
	    \+ memberchk(Func-_, ABox),
	    my_error('Warning:  predicate ~w has no clauses', [Name/Arity]),
	    fail
	).
	
interp_abox(Head, ABox) :-
	functor(Head, Name, Arity),
	memberchk(Name/Arity-Cls, ABox),
	member(Head, Cls).

print_query_results(true, OFile, QRs) :- !,
	current_output(Stream),
	tell(OFile),
	call_cleanup(print_query_results(QRs),
		     (told,set_output(Stream))
		    ).
print_query_results(_, _OFile, _QRs).

print_query_results(QRs) :-
	member(P-Rs, QRs),
	format('~w~t~8| ', [P]),
	print_arg(Rs), nl,
	fail.
print_query_results(_).

% TBox deannotation

print_kb(true, OFile, InfoGoal, UProgram, ABox, PredInfo) :- !,
	current_output(Stream),
	tell(OFile),
	InfoGoal, nl,
	call_cleanup(print_kb(UProgram, ABox, PredInfo),
		     (told,set_output(Stream))
		    ).
print_kb(_Save, _OFile, _InfoGoal, _UProgram, _ABox, _PredInfo).

print_kb(Program, ABox, PredInfo) :-
	member(Func-Clauses, Program),
	print_pred(Clauses,  ABox, PredInfo, Func), fail.
print_kb(_, _, _).


print_pred([], _ABox, PredInfo, Func) :- !,
	atomic_preds_of_predinfo(PredInfo, AtomicPs),
	(   \+ memberchk(Func, AtomicPs) ->
	    Func = Name/Arity,
	    functor(Head, Name, Arity),
	    print_clause(Head, [fail]), nl
	;   true
	).
print_pred(Clauses, _ABox, PredInfo, _) :-
	query_preds_of_predinfo(PredInfo, QPs),
	member(Cl, Clauses),
	print_deannotated_clause(Cl, QPs),
	fail.
print_pred(_, _, _, _) :-
	nl.

print_deannotated_clause(cl(Head0,ABody,Als0), QPs) :-
	deannotated_head(Head0, QPs, Als0, ALVar, Als, Head),
	deannotated_bodylist(ABody, ALVar, BodyList0),
	append(Als, BodyList0, BodyList),
	print_clause(Head, BodyList).

print_clause(Head, BodyList) :-
	prettyvars(Head-BodyList),
	print_goal(Head),
	(   BodyList == [] -> write(.), nl
	;   write(' :-\n'),
	    append(_, [Goal|Rest], BodyList),
	    write('        '), print_goal(Goal),
	    (   Rest == [] -> write(.), nl
	    ;   write(',\n'), fail
	    ) -> true
	).

my_writeq(Term) :-
	write_term(Term, [numbervars(true),quoted(true)]).

print_goal(Mod:Goal) :- !,
	my_writeq(Mod), my_writeq(:),
	print_goal(Goal).
print_goal(A = B) :- !,
	print_arg(A), write(' = '),
	print_arg(B).
print_goal(Goal) :-
	print_term(Goal, ', ').

print_arg(Arg) :-
	Arg = [_|_], !,
	print_list(Arg).
print_arg(Arg) :-
	(   Arg = _/_ -> true
	;   Arg = '$VAR'(_)
	), !,
	my_writeq(Arg).
print_arg(Arg) :-
	print_term(Arg, ',').

print_term(Goal, Sep) :-
	Goal =.. [Name|Args],
	my_writeq(Name),
	(   Args == [] -> true
	;   write('('),
	    append(_, [Arg|Rest], Args),
	    print_arg(Arg),
	    (   Rest == [] -> !, write(')')
	    ;   write(Sep), fail
	    )
	).
	
print_list([]) :- !,
	write([]).
print_list(List) :-
	write('['),
	append(_, [Elem|Rest], List),
	print_arg(Elem),
	(   Rest == [] -> write(']')
	;   Rest = [_|_] ->  write(','), fail
	;   write('|'), my_writeq(Rest), write(']')
	),
	!.


% user:portray(List) :-
% 	is_list(List),
% 	List = [_|_], !,
% 	print_list(List).
% user:portray(rec(A,B)) :-
% 	write('rec('),
% 	my_print(A),
% 	write(','),
% 	my_print(B),
% 	write(')').

deannotated_head(Head0, QPs, Als0, ALVar, Als, Head) :-
	functor(Head0, Name, Arity),
	(   memberchk(Name/Arity, QPs) ->
	    Head = Head0,
	    Als = Als0,
	    (   Als \== [] ->
		my_error('Internal error: query pred ~w contains ancs: ~w', [Name/Arity,Als])
	    ;   true
	    )
	;   Head0 =.. [Name,Arg],
	    Head =.. [Name,Arg,ALVar],
	    add_ancs_args(Als0, ALVar, Als)
	).

add_ancs_args([], _, []).
add_ancs_args([na(NewAncs,Alias)|Als0], Ancs, Als) :- !,
	(   NewAncs == [] ->
	    Alias = Ancs,
	    add_ancs_args(Als0, Ancs, Als)
	;   expand_newanc(na(Ancs,NewAncs,Alias), Al),
	    Als = [Al|Als1],
	    add_ancs_args(Als0, Ancs, Als1)
	).
add_ancs_args([Al0|Als0], Ancs, [Al|Als]) :-
	expand_newanc(Al0, Al),
	add_ancs_args(Als0, Ancs, Als).

expand_newanc(NA, NA).
% expand_newanc(na(Ancs,NewAncs,Alias), Alias=AllAncs) :-
%	     append(NewAncs, Ancs, AllAncs).

deannotated_bodylist([], _ALVar, []).
deannotated_bodylist([G|Gs], ALVar, DGs) :-
	deannotated_goal(G, ALVar, DGs, DGs0),
	deannotated_bodylist(Gs, ALVar, DGs0).

deannotated_goal(goal(G0,A0), _, DGs, DGs0) :- !,
	(   A0 == abox -> DGs = [abox:G0|DGs0]
	;   functor(G0, _, 2) -> DGs = [G0|DGs0]
	;   A0 == entry -> DGs = [G0|DGs0]
	;   A0 == query -> DGs = [G0|DGs0]
	;   G0 =.. [Pred,Arg],
	    append_ancs_arg(A0, Pred, Arg, DGs, DGs0)
	).
deannotated_goal(G0, ALVar, [G|DGs0], DGs0) :-
	% Adding AncList variable to '$checkanc' goals:
	G0 =.. [Pred|Args],
	G  =.. [Pred,ALVar|Args].

append_ancs_arg(rec(PG,Als), Pred, Arg, DGs, DGs0) :-
	!, append_ancs_arg(Als, rec(PG), Pred, Arg, DGs, DGs0).
append_ancs_arg(Als, Pred, Arg, DGs, DGs0) :-
	append_ancs_arg(Als, nonrec, Pred, Arg, DGs, DGs0).

append_ancs_arg([], _,_, _, DGs, DGs).
append_ancs_arg([Al0|Als], Rec, Pred, Arg, [G|DGs], DGs0) :-
	(   Rec == nonrec -> Al = Al0
	;   Rec = rec(PG), Al = rec(PG,Al0)
	),
	G =.. [Pred,Arg,Al],
	append_ancs_arg(Als, Pred, Arg, DGs, DGs0).
	
lpreds(Goal, Sel) :-
	lpreds(Goal, Sel, _Func).

lpreds(Goal, Sel, Func) :-		
	sel(Goal, Sel, P),
	append(_, [Pr|Rest], P),
	(   Pr = pr(Func,Cls,PData) -> true
	;   Pr = Func-Cls, PData = ''
	),
	writeq(user_error, Func-PData), nl(user_error),
	member(TCl,Cls),
	writeq(user_error, TCl), nl(user_error),
	var(Rest), !. 
lpreds(_, _, _).

lterms(Goal, Sel) :-
	sel(Goal, Sel, List),
	append(_, [E|Rest], List),
	writeq(user_error, E), nl(user_error),
	var(Rest), !.
lterms(_, _).

sel(Term, Sel, Arg) :-
	(   integer(Sel) -> arg(Sel, Term, Arg)
	;   Sel == [] -> Arg = Term
	;   Sel = [N|Ns], arg(N, Term, Term1),
	    sel(Term1, Ns, Arg)
	).

selw(Term, Sel, Arg) :-
	sel(Term, Sel, Arg),
	writeq(user_error, Arg), nl(user_error).

time Goal :-
	functor(Goal, Predname, _),
	Predname time Goal.

Msg time Goal :-
	statistics(runtime, [T0|_]),
	Goal,
	statistics(runtime, [T1|_]),
	Msec is T1-T0,
	(   Msec >= 50 ->
	   my_error('~t~3d~10| sec for ~q', [Msec,Msg])
	;   true
	).

:- bb_put(itrace, off).

% From SICStus BIPs

prettyvars(Term) :-
	prettyvars(Term, VarsOccs0, []),
	keysort(VarsOccs0, VarsOccs),
	set_anonymous(VarsOccs),
	set_named(VarsOccs0, 0).

prettyvars(Var) --> {var(Var)}, !, [Var-[]].
prettyvars(X) -->
	{functor(X, _, A)},
	prettyvars(0, A, X).

prettyvars(A, A, _) --> !.
prettyvars(A0, A, X) -->
	{A1 is A0+1},
	{arg(A1, X, X1)},
	prettyvars(X1),
	prettyvars(A1, A, X).

set_anonymous([]).
set_anonymous([V1-_,V2-_|L1]) :- V1==V2, !,
	set_anonymous(L1, V1).
set_anonymous([V-_|L1]) :-
	V = '$VAR'('_'),
	set_anonymous(L1).

set_anonymous([V1-_|L1], V2) :- V1==V2, !,
	set_anonymous(L1, V2).
set_anonymous(L1, _) :-
	set_anonymous(L1).

set_named([], _).
set_named([V-_|L], I) :- var(V), !,
	V = '$VAR'(I),
	J is I+1,
	set_named(L, J).
set_named([_|L], I) :-
	set_named(L, I).
