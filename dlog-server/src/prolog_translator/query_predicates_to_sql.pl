:- module(query_predicates_to_sql, [query_predicates_to_sql/5, test_file_to_sql/2]).

:- use_module('../test/dlog_test', [read_test_file/4]).
:- use_module('../core/kb_manager', [new_kb/1, release_kb/1]).
:- use_module('../core/config', [set_dlog_option/3]).
:- use_module('../dl_translator/axioms_to_clauses', [axioms_to_clauses/4]).
:- use_module('../prolog_translator/abox_signature', [abox_signature/4]).
:- use_module('../prolog_translator/tbox_translator', [tbox2prolog/4]).


%query_predicates_to_sql(+TransformedTBox, +QueryPredicates, +DBConnections, +DBPredicates, -SQL)
%lehet, hogy majd még egy KB URI-t is kell kapnod (ha beállításokat használsz)
query_predicates_to_sql(TransformedTBox, QueryPredicates, DBConnections, DBPredicates, SQL) :-
	writeq(query_predicates_to_sql(TransformedTBox, QueryPredicates, DBConnections, DBPredicates, SQL)).
%TransformedTBox jelenlegi formátumáért lásd tbox_writer
%DBConnections, DBPredicates formátumáért lásd lent

%test_file_to_sql(+File, -SQL)
%ideiglenes belépési csonk, amíg kész nem lesz, és nem a normál bejáratot használjuk
%(emiatt kell betölteni a teljes rendszert :) %http://xkcd.com/541/
test_file_to_sql(File, SQL) :-
	read_test_file(File, axioms(ImpliesCL, ImpliesRL, TransL, ABox, 
						_Concepts, _Roles, DBConnections, DBPredicates), 
						_Queries, _Options),
	new_kb(URI),
	set_dlog_option(unfold, URI, yes),
	set_dlog_option(projection, URI, no),  
	set_dlog_option(ground_optim, URI, no), %nem tudom még mit kell átállítani
	axioms_to_clauses([ImpliesCL, ImpliesRL, TransL], _Saved, TBox_Clauses, _Save),
	abox_signature(ABox, DBPredicates, _ABoxData, Signature),
	tbox2prolog(URI, tbox(TBox_Clauses, ImpliesRL), abox(Signature), 
			tbox(TransformedTBox, _EqRoles, QueryPredicates)), 
	query_predicates_to_sql(TransformedTBox, QueryPredicates,  DBConnections, DBPredicates, SQL),
	release_kb(URI).
	

%%%%%%%%%%%%%%%%%%%%  Axioms formátum

%   axioms(ImpliesCL, ImpliesRL, TransL, ABox, Concepts, Roles, DBConnections, DBPredicates):
%   1. ImpliesCL: concept inclusion axioms:
%      implies(C1,C2), where C1 and C2 are concepts
%   2. ImpliesRL: role inclusion axioms:
%      subrole(R, S), where R and S are roles
%   3. TransL: list of transitive roles
%   4. ABox: list of ABox axioms 
%   5. Concepts: list of all concepts
%   6. Roles: list of all roles
%   7. DBConnections: list of connection(CName, DSN, User, Pass)
%      where CName is the individual representing the connection
%   8a. DBAccesses: list of access(AName, CName, Access, NegAccess)
%      where AName is the individual representing the access
%      Access and NegAccess are query(Query) OR table(Table, Col) OR table(Table, Col1-Col2)
%      Query, Table and Cols are atoms.
%   9a. DBPredicates: list of ConceptName/1-AName or RoleName/2-AName
%      where AName is the individual representing the access 
%   8-9b. DBPredicates: list of 8a and 9a paired up to the format 
%      access(Functor, Connection, Access) and
%      access(NegFunctor, Connection, NegAccess)

