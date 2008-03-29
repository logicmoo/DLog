:- module(dlog, [dlkb2prolog/3, load_KB_interpreter/1, load_KB0/6,write_tbox/2]).

%TODO

%:- use_module('struct').
:- use_module('interfaces/dig_iface').
:- use_module('dl_translator/translator').
%:- use_module('show').
:- use_module('prolog_translator/dl_interp',[assert_clauses/1]).
:- use_module(library(lists)).
:- use_module('prolog_translator/tbox_translator',[tbox2prolog/4]).
:- use_module('prolog_translator/abox_translator',[abox2prolog/3]).
:- use_module('prolog_translator/abox_signature',[abox_signature/3]).
:- use_module('prolog_translator/transforming_tools', [neg/2]).

user:runtime_entry(start) :-
	start_dlog.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Prolog fordítás
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% abolish(abox:_), dlkb2prolog('../examples/iocaste/c10.dig', '../output/c10', [allinone(yes), statistics(yes)]).
% abolish(abox:_), dlkb2prolog('../examples/happy/happy.dig', '../output/happy', [allinone(yes), statistics(yes)]).
% abolish(abox:_), dlkb2prolog('../examples/alcoholic/ca10.dig', '../output/ca10', [allinone(yes), statistics(yes)]).
% abolish(abox:_), dlkb2prolog('../examples/shi/inverse.dig', '../output/inverse', [allinone(yes), statistics(yes)]).
% abolish(abox:_), dlkb2prolog('../examples/shi_pelda.dig', '../output/shi_pelda', [allinone(yes), statistics(yes)]).
% abolish(abox:_), dlkb2prolog('../examples/LUBM/lubm1.dig', '../output/lubm1', [allinone(yes), statistics(yes)]).
dlkb2prolog(KBFile, PFile, Options) :-
	load_KB0(KBFile, _Statistics, TBox, ABox, IBox, HBox),
	statistics(runtime, [T0,_]),
	abox_signature(ABox, ABoxStr, Signature),
	abox2prolog(PFile, abox(ABoxStr), Options),
	tbox2prolog(PFile, tbox(TBox, IBox, HBox), abox(Signature), Options),

	statistics(runtime, [T1,_]),Tt is T1-T0,
	format(' Prolog clauses have been generated in ~t~20|~t~3d sec ~n', [Tt]).

load_KB0(KBFile, Statistics, TBox_Clauses, ABox, IBox, HBox):-
	statistics(runtime, [T0,_]),
	read_tell(KBFile,TBox,ABox, Statistics),
	statistics(runtime, [T1,_]),T is T1-T0,
	format(' DIG KB read in ~t~20|~t~3d sec ~n', [T]),
	axioms_to_clauses(TBox,TBox_Clauses, IBox,HBox,_),
	statistics(runtime, [T2,_]),TA is T2-T1,
	format(' Axioms translated in ~t~20|~t~3d sec ~n', [TA]),	
	( member([],TBox_Clauses) -> nl, nl, write('Inconsistent TBox'), nl, nl, fail
	; true
	).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Prolog fordítás vége
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

write_tbox(KBFile, OutFile) :-
	load_KB(KBFile),
	open(OutFile, write, Stream),
	call_cleanup(flush_tbox(Stream), close(Stream)).

flush_tbox(Stream) :-
	current_predicate(_, dl:Head),
	clause(dl:Head, Body),
	Body \== true,
	portray_clause(Stream, (Head :- Body)),
	fail.
flush_tbox(_).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Prolog interpretalas
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

load_KB_interpreter(KBFile):-
	statistics(runtime, [T0,_]),
	read_tell(KBFile,TBox,ABox,_),
	statistics(runtime, [T1,_]),T is T1-T0,
	format(' DIG KB read in ~t~20|~t~3d sec ~n', [T]),
	axioms_to_clauses(TBox,TBox_Clauses),
	statistics(runtime, [T2,_]),TA is T2-T1,
	format(' Axioms translated in ~t~20|~t~3d sec ~n', [TA]),
	( member([],TBox_Clauses) -> nl, nl, write('Inconsistent TBox'), nl, nl, fail
	; true
	),
	abolish(dl:_),
	clauses_to_prolog(TBox_Clauses,ABox),
	statistics(runtime, [T3,_]),TB is T3-T2,
	format(' Clauses asserted in ~t~20|~t~3d sec ~n', [TB]).
		    
clauses_to_prolog(TBox_Clauses,ABox):-
	findall(P, (
		     ( member(C,TBox_Clauses)
		     ; member(A,ABox), C=[A] % TODO fail-t ki kell szedni innen
		     ),
		     cls_to_neglist(C,P)
		   ), Neglist
	       ),
	assert_clauses(Neglist).


/***************************** Klozok atalakitasa *********************************/

% cls_to_neglist(+Cls,-NL): Cls klozt alakitja at a fordito modul formatumarol
% az interpreter altal ertelmezett formatumra
cls_to_neglist([not(L)|Ls],[FL|FLs]):-
	!,trans(L,FL),
	cls_to_neglist(Ls,FLs).
cls_to_neglist([L|Ls],[FL|FLs]):-
	!,trans(not(L),FL),
	cls_to_neglist(Ls,FLs).
cls_to_neglist([],[]).

% trans(+L,-H): H az L literal struktura alakban
trans(not(L),NTL):-
	!,
	trans(L,TL),
	neg(TL, NTL).
trans(eq(X,Y),eq(X,Y)):- !.
trans(L,C):-
	L=..[T,N|P],
	( T=nconcept-> atom_concat('NC_',N,NN),
	    C=..[NN|P]
	; C=..[N|P]
	).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% runtime system
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
clean_conf :-
	current_predicate(_,conf:Head),
	retractall(conf:Head),
	fail.
clean_conf.

start_dlog :-
	log('DLog reasoner 0.1 alpha started~n~n',[]),
	load_config,
	conf:input(Input),
	conf:output(Output),
	conf:options(Options),
	log('Input ontology: ~w~n', [Input]),
	log('Ouput Prolog code: ~w~n~n', [Output]),
	log('Compilation options: ~w~n~n', [Options]),
	assert(env_default:alma(1)), % hack to supress a warning flag
	dlkb2prolog(Input, Output, Options),
	log('~nProlog code has been generated, exiting...', []).

load_config :-
	clean_conf, 
	open('../dlog.conf', read, Handle),
	load_config0(Handle),
	close(Handle),
	log('Configuration file has been successfully processed.~n~n',[]).

load_config0(Handle) :-
	catch(read(Handle, Term), Error, error_handler(Error)),
	(
	  Term \= end_of_file ->
	  assert(conf:Term),
	  load_config0(Handle)
	;
	  true
	).

% log(+Message, +Arguments)
log(Message, Arguments) :-
	format(Message, Arguments).

error_handler(syntax_error(_Goal, Position, _Message, _Tokens, _AfterError)) :-
	log("Error: Cannot read, problem with line: ~w~n",[Position]),
	fail.