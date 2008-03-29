:- use_module(library(lists)).

% run_generator(+File)
% run_generator('../test/alcoholic.gen').
run_generator(File) :-
	abolish(env:_),
	read_suite(File),
	generate.

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

generate :-
	env:testcase(N, M, B, File, Formats),
	alcoholic(N, M, B, Struct),
	export_results(Struct, File, Formats),
	fail.
generate.

export_results(Struct, File0, Formats) :-
	member(Format, Formats),
	atom_concat(File0, '.', File1),
	atom_concat(File1, Format, File),
	(
	  Format == dig ->
	  struct2dig(Struct, File)
	;
	  Format == owl ->
	  struct2owl(Struct, File)
	),
	format('File ~p in ~p format has been created~n',[File, Format]),
	fail.
export_results(_, _, _).

% alcoholic(+N, +M, +B, -Struct)
% Struct contains an alcoholic structure where
% - the number of parents is N
% - the number of friends is M
% - B is the number of parents between the last parent and her friend
alcoholic(N, M, B, Struct) :-
	parents(N, B, Parents),
	friends(M, N, Friends),
	append(Parents, Friends, Struct).

parents(N, B, [el(hasFriend, csp(N), csp(BSzul))|Struct0]) :-
	N > 1, 
	parents0(N, Struct0),
	BSzul is N-B.

parents0(0, Es) :- !, Es = [].
parents0(N, [el(hasParent, csp(N1), csp(N))|Es]) :-
	N1 is N-1,
	parents0(N1, Es).

friends(0, _N, Friends) :- !, Friends = [].
friends(M, N, Friends) :-
	friends0(N, M, N, [], Friends).

friends0(0, _M, _Offset, Gy, Friends) :-
	!, Friends = Gy.
friends0(N, M, Offset, Gy, Friends) :-
	friends_chain(M, N, Offset, Chain),
	append(Chain, Gy, Gy1),
	N1 is N-1,
	friends0(N1, M, Offset, Gy1, Friends).

friends_chain(0, _, _, Chain) :- !, Chain = [].
friends_chain(M, N, Offset, [el(hasFriend,csp(N),csp(C))|Es]) :-
	C is Offset+M,
	M1 is M-1,
	friends_chain(M1, N, Offset, Es).

struct2dig(Struct, File) :-
	open(File, write, Stream),
	current_output(Output),
	set_output(Stream),
	format('<?xml version="1.0" encoding="UTF-8"?>~n',[]),
	format('<tells uri="" xmlns="http://dl.kr.org/dig/2003/02/lang">~n',[]),
	format('    <defconcept name="Alcoholic"/>~n',[]),
	format('    <defrole name="hasParent"/>~n',[]),
	format('    <defrole name="hasFriend"/>~n',[]),
	format('    <impliesc>~n',[]),
	format('        <some>~n',[]),
	format('            <ratom name="hasFriend"/>~n',[]),
	format('            <catom name="Alcoholic"/>~n',[]),
	format('        </some>~n',[]),
	format('        <not>~n',[]),
	format('            <catom name="Alcoholic"/>~n',[]),
	format('        </not>~n',[]),
	format('    </impliesc>~n~n',[]),
	format('    <impliesc>~n',[]),
	format('        <some>~n',[]),
	format('            <ratom name="hasParent"/>~n',[]),
	format('            <not>~n',[]),
	format('              <catom name="Alcoholic"/>~n',[]),
	format('            </not>~n',[]),
	format('        </some>~n',[]),
	format('        <not>~n',[]),
	format('            <catom name="Alcoholic"/>~n',[]),
	format('        </not>~n',[]),
	format('    </impliesc>~n~n',[]),

	kiir_dig(Struct),
	format('</tells>~n',[]),
	close(Stream),
	set_output(Output).

kiir_dig([]).
kiir_dig([el(Cimke,csp(A),csp(B))|Es]) :-
	format('<related>~n',[]),
	format('  <individual name="i~w"/>~n',[A]),
	format('  <ratom name="~w"/>~n',[Cimke]),
	format('  <individual name="i~w"/>~n',[B]),
	format('</related>~n',[]),
	kiir_dig(Es).


struct2owl(Struct, File) :-
	open(File, write, Stream),
	current_output(Output),
	set_output(Stream),
	format('<?xml version="1.0"?>~n',[]),
	format('<rdf:RDF~n',[]),
	format('  xml:base = "http://www.cs.bme.hu/vima9000"~n',[]),
	format('  xmlns = "http://www.cs.bme.hu/vima9000#"~n',[]),
	format('  xmlns:rdf = "http://www.w3.org/1999/02/22-rdf-syntax-ns#"~n',[]),
	format('  xmlns:rdfs = "http://www.w3.org/2000/01/rdf-schema#"~n',[]),
	format('  xmlns:owl = "http://www.w3.org/2002/07/owl#">~n~n',[]),
	format('<owl:Ontology rdf:about=""/>~n',[]),
	format('<owl:ObjectProperty rdf:ID="hasParent" />~n',[]),
	format('<owl:ObjectProperty rdf:ID="hasFriend" />~n',[]),
	format('<owl:Class rdf:ID="Alcoholic" />~n~n',[]),
	format('<owl:Class rdf:ID="notAlcoholic">~n',[]),
	format('  <owl:complementOf>~n',[]),
	format('     <owl:Class rdf:about="#Alcoholic"/>~n',[]),
	format('  </owl:complementOf>~n',[]),
	format('</owl:Class>~n~n',[]),
	format('<owl:Restriction>~n',[]),
	format('    <owl:onProperty rdf:resource="#hasFriend" />~n',[]),
	format('    <owl:someValuesFrom rdf:resource="#Alcoholic" />~n',[]),
	format('    <rdfs:subClassOf rdf:resource="#notAlcoholic" />~n',[]),
	format('</owl:Restriction>~n',[]),
	format('<owl:Restriction>~n',[]),
	format('    <owl:onProperty rdf:resource="#hasParent" />~n',[]),
	format('    <owl:someValuesFrom rdf:resource="#notAlcoholic" />~n',[]),
	format('    <rdfs:subClassOf rdf:resource="#notAlcoholic" />~n',[]),
	format('</owl:Restriction>~n',[]),
	kiir_owl(Struct),
	format('</rdf:RDF>~n',[]),
	close(Stream),
	set_output(Output).


kiir_owl([]).
kiir_owl([el(Cimke,csp(A),csp(B))|Es]) :-
	format('<owl:Thing rdf:ID="i~w">~n',[A]),
	format('  <~w rdf:resource="#i~w" />~n',[Cimke,B]),
	format('</owl:Thing>~n~n',[]),
	kiir_owl(Es).