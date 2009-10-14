% gtrace,test.
% listing_prefix(dl2sql,X).
% :- module(query_predicates_to_sql, [query_predicates_to_sql/5, test_file_to_sql/2]).
:- use_module('zsl_util').
:- use_module('../test/dlog_test', [read_test_file/4]).
:- use_module('../core/kb_manager', [new_kb/1, release_kb/1]).
:- use_module('../core/config', [set_dlog_option/3]).
:- use_module('../dl_translator/axioms_to_clauses', [axioms_to_clauses/4]).
:- use_module('../prolog_translator/abox_signature', [abox_signature/4]).
:- use_module('../prolog_translator/tbox_translator', [tbox2prolog/4]).
% :- use_module('pl2sql').

:- use_module('presql_struct2sql.pl').


write_rule(IOFD,Rule):-
   writec('\n----------------------\n\n'+Rule).
/*   
  
 van-e szo, vagy egy bonyolultabb szabályrõl (trivialis szerep ill. fogalom)
 The Trivial variable shows whether it's about a simple Abox ref or it's a compund concept/role
*/ 
filter_role(Rule,[trivial=Trivial],Id):-
   (
      % inverse of trivial role
      Rule = role(inv(PRN)-[], primary, [ (inv(PRN)-[_G14714, _G14717]:-abox(idx(PRN))-[_G14714, _G14717])])
      ->
         (
         T=[] ->
               Trivial = false,
               assert(dl2sql_role_abox(Id,inv(PRN)-[_V1,_V2]))
         ;
            true
            ,writec('\n  inv role \n'+
            T+'\n\n')
         )
      ;
      %trivial role
      Rule = role(PRN-[], primary, [ (PRN-[_G14784, _G14787]:-abox(PRN)-[_G14784, _G14787])|T]) 
      -> 
         (

            T=[] ->
               Trivial = false,
               assert(dl2sql_role_abox(Id,PRN-[_V1,_V2]))
            ;
            true
            ,writec('\n  role \n'+
            T+'\n\n')
         )
      ;      
      %szerep tartalmazas      
      %ezeket egyelore nem dolgozom fel      
      Rule = role(inv(_)-[],hierarchy,_)
      ->
      Trivial = true      
      ;      
      Rule = role(_-[],hierarchy,_)
      
      
   ).

filter_concept(Rule,[trivial=Trivial],Id):-
   (
      % negation of trivial concept
      Rule = concept(not(PRN)-[], general, [ (not(PRN)-[_G9947]:-init_state(_G9995), normal(not(PRN))-[_G9947])|T1])
      
      ->
         (
            (
            T1=[(_-_:-abox(_)-[_])|T],!
            ;
            % nincs abox: memoriaban tarolt predikatum eseten? 
            % (bug gyanus)            
            T=T1 
            ),
            (
               T=[] ->
                  Trivial=false
                  %, assert(dl2sql_concept_abox(Id,normal(not(PRN))-[_]))
               ;
               writec('\n  neg concept \n'+
               T+'\n')
               ,assert_concepts(T,Id)             
               %2
            )
         )
      ;
      % trivial concept
      Rule = concept(PRN-[], general, [ (PRN-[_G14284]:-init_state(_G14355), normal(PRN)-[_G14284]), (normal(PRN)-[_G14315]:-abox(PRN)-[_G14315])|T1])
      ->
         (
            (
            T1=[(_-_:-abox(_)-[_])|T],!
            ;
            T=T1
            ),
         
            (
               T=[] ->
                  Trivial=false
                  %, assert(dl2sql_concept_abox(Id,normal(PRN)-[_]))
                  % dl2sql_dbpredicate alapjan donteni el assert-e vagy sem
               ;
               true
               ,writec('\n concept \n'+
               T+'\n')
               ,assert_concepts(T,Id)
               %,assert(dl2sql_concept(Id,T))           
               %2
            )
         )
      ;
      % concept from a role: role.top (probably bug)
      Rule = concept(PRN-[], general, [ (PRN-[_G6646]:-init_state(_G6728), normal(PRN)-[_G6646, _G6728])])
      ->
         Trivial=false
   ).

assert_concepts([],_).   
assert_concepts([H|T],Id):-
   H=(HC:-HB),
   assert(dl2sql_concept(Id,HC,HB)),
   assert_concepts(T,Id).
   
   

filter_trivial(IOFD,Rule,Id):-
%   Rule  \= concept(not(_)-_,_,_),
   FilterVar=[type=Type,trivial=Trivial],
   (
      Rule = concept(_-_,_,PredList) ->
         Type=concept,
         filter_concept(Rule,[trivial=Trivial],Id)        
      ;
      Rule = role(_-_,_,PredList) -> 
         Type=rule,
         filter_role(Rule,[trivial=Trivial],Id)         
   ),
      
   member(Pred,PredList),
   (
      Trivial = true ->
         writec(IOFD,'Ok:\n'+Rule+'\n\n')
      ;
      writec(IOFD,'filtered:\n'+Rule+'\n\n')
   ).
assert_dbpredicates([],_).
assert_dbpredicates([H|T],Id):-
   assert(dl2sql_dbpredicate(Id,H)),
   assert_dbpredicates(T,Id).

   
/*
plate(Query,PlatedQuery,Id):-

         (
            dl2sql_concept_db_abox(Id,Query,_) ->
               PlatedQuery = Query
         ;
            dl2sql_role_db_abox(Id,Query,_) ->
               PlatedQuery = Query
         ;
            dl2sql_concept(Id,Query,Q2) ->
            plate(Q2,PlatedQuery,Id)
         ;
            Query = ','(HQ,TQ),
            plate(HQ,PQ1,Id),
            plate(TQ,PQ2,Id),
            PlatedQuery = ','(PQ1,PQ2)
         ).
   
*/

% az adott fogalmat megkeresi az aboxban (adatbazis ill. memoria)
% searches the concept in the Abox ( DB or memory )
make_concept_aboxref(Id,Query,RefList):-
   Query = normal(PRN)-[X],
   (
      clause(dl2sql_concept_db_abox(Id,Query,_),_)
      ->
      tmp_id(TID1),
      TMP1 = [sql_abox(TID1,[X],PRN)]
   ;
      TMP1 = []
   ),
   (
      clause(dl2sql_cassertion(Id,Query,_),_)
      ->
      tmp_id(TID2),
      bagof(Inst,dl2sql_cassertion(Id,Query,Inst),InstList),
      TMP2 = [mem_abox(TID2,[X],PRN,InstList)]
   ;
      TMP2 = []
   ),
   append(TMP1,TMP2,RefList).

make_role_aboxref(Id,Query,RefList):-
   Query = PRN-[X,Y],
   (
      clause(dl2sql_role_db_abox(Id,Query,_),_)
      ->
      tmp_id(TID1),
      TMP1 = [sql_abox(TID1,[X,Y],PRN)]
   ;
      TMP1 = []
   ),
   (
      clause(dl2sql_rassertion(Id,Query,_),_)
      ->
      tmp_id(TID2),
      bagof(I1-I2,dl2sql_rassertion(Id,Query,[I1,I2]),InstList),
      TMP2 = [mem_abox(TID2,[X,Y],PRN,InstList)]
   ;
      TMP2 = []
   ),
   append(TMP1,TMP2,RefList).   
   
   
   
commalist2list(CommaL,List):-
	(   
	CommaL = ','(H,T) ->	
	List = [H|TC],
	    commalist2list(T,TC)
	;   
	List = [CommaL|[]]
	).

   
% plates compund concepts/roles: the output is PreSQLStruct
plate(Query,PlatedQuery,Id):-
   
   (
      dl2sql_concept(Id,Query,Concept_RHS) 
      ->
      (
         Query = normal(PRN)-[X],
         tmp_id(JoinId),
         tmp_id(Unionid),

         plate(Concept_RHS,Plated_RHS,Id),
         commalist2list(Plated_RHS,Pl_RHS_List),       
         Derivative_concept = join(JoinId,[X],Pl_RHS_List),       
         make_concept_aboxref(Id,Query,Aboxref),
         append([Derivative_concept],Aboxref,UnionList),
         PlatedQuery = union(Unionid,[X],UnionList)
      )
      
   ;
      Query = PRN-[X,Y] ->
      make_role_aboxref(Id,Query,UnionList),
      (
         length(UnionList,1)
         ->
         nth1(1,UnionList,PlatedQuery)
      ;      
         tmp_id(Unionid),            
         PlatedQuery = union(Unionid,[X,Y],UnionList)
      )
   ;  
      Query = ','(HQ,TQ)
      ->
      plate(HQ,PQ1,Id),
      plate(TQ,PQ2,Id),
      PlatedQuery = ','(PQ1,PQ2)
   ;
      Query = normal(PRN)-[X],
      make_concept_aboxref(Id,Query,Aboxref),                           
      (
         length(Aboxref,1)
         ->
         nth1(1,Aboxref,PlatedQuery)
      ;
         tmp_id(Unionid),
         PlatedQuery = union(Unionid,[X],Aboxref)
      )
   ).   
   
%!!
% a tst file-ban külön kell szerepelnie egy fogalomnak és annak tagadásának,
% a szerepekeknél viszont az inverzet nem kell külön felvenni
assert_db_abox(Id,X):-
   X=access(A,B,C),
   (
      A=PRN/1
      ->
      assert(dl2sql_concept_db_abox(Id,normal(PRN)-[_],C))
   ;   
      A=PRN/2
      ->
      (
         assert(dl2sql_role_db_abox(Id,PRN-[_,_],C)),
         assert(dl2sql_role_db_abox(Id,inv(PRN)-[_,_],C))
      )
   ).

assert_mem_abox(Id,X):-
   (
      X = cassertion(aconcept(PRN),Inst)
      ->
      assert(dl2sql_cassertion(Id,normal(PRN)-[_],Inst))
   ;
      X = rassertion(arole(PRN),Inst1,Inst2)
      ->
      assert(dl2sql_rassertion(Id,PRN-[_,_],[Inst1,Inst2]))
   ).

%query_predicates_to_sql(+TransformedTBox, +QueryPredicates, +DBConnections, +DBPredicates, -SQL)
%lehet, hogy majd még egy KB URI-t is kell kapnod (ha beállításokat használsz)
query_predicates_to_sql(TransformedTBox, QueryPredicates, _DBConnections, DBPredicates,Signature,ABox,Query, SQL,IOFD):-
   	
   writec(IOFD,'Transformed Tbox: \n\n'),
   tmp_id(Id),
   assert_dbpredicates(DBPredicates,Id),   
   bagof(X,(member(X,TransformedTBox),filter_trivial(IOFD,X,Id)),_),
   bagof(X,(member(X,DBPredicates),assert_db_abox(Id,X)),_),
   (
      bagof(X,(member(X,ABox),assert_mem_abox(Id,X)),_),!
      ;
      true
   ),
	write(IOFD,'\n\n----Query predicates\n\n'),
	write(IOFD,querypred(QueryPredicates)),
	write(IOFD,'\n\n----Database predicates\n\n'),
	write(IOFD,dbpred(DBPredicates)),
   write(IOFD,'\n-----------ABOX signature\n\n'),
	write(IOFD,abox(Signature)),
   write(IOFD,'\n-----------ABOX assertions\n\n'),
	write(IOFD,abox(ABox)),
   close(IOFD),

   Query=X-QBody,
   %writec('\nplate elott:\n'+Query),
   plate(QBody,PlatedQuery,Id),   
   
   /*
   tmp_id(TID),
   PreSqlStruct = join(TID,[X],[PlatedQuery]),
   */
   PlatedQuery = PreSqlStruct,
   writec('\n\nplated query:\n'+PreSqlStruct),
   (
      struct2query(top_level,PreSqlStruct,SQLQuery,Id),!,
      writec('\nSQL query:\n\n '+SQLQuery+'\n\n')
   ;
      write('\nstruct2query failed\n')
   ).   

   
% query_predicates_to_sql helper predikatumok

dlify(In,_):-
   writec('Hello: '+In).
   

%TransformedTBox jelenlegi formátumáért lásd tbox_writer
%DBConnections, DBPredicates formátumáért lásd lent

%test_file_to_sql(+File, -SQL)
%ideiglenes belépési csonk, amíg kész nem lesz, és nem a normál bejáratot használjuk
%(emiatt kell betölteni a teljes rendszert :) %http://xkcd.com/541/

test_file_to_sql(InFile,SQL,Outstream,Query) :-
	read_test_file(InFile, axioms(ImpliesCL, ImpliesRL, TransL, ABox, 
						_Concepts, _Roles, DBConnections, DBPredicates), 
						_Queries, _Options),
/*
   write_axioms(axioms(ImpliesCL, ImpliesRL, TransL, ABox, 
						_Concepts, _Roles, DBConnections, DBPredicates)),
*/                  

	new_kb(URI),
	set_dlog_option(unfold, URI, yes),
	set_dlog_option(projection, URI, no),  
	set_dlog_option(ground_optim, URI, no), %nem tudom még mit kell átállítani
	axioms_to_clauses([ImpliesCL, ImpliesRL, TransL], _Saved, TBox_Clauses, _Save),
	abox_signature(ABox, DBPredicates, _ABoxData, Signature),
	tbox2prolog(URI, tbox(TBox_Clauses, ImpliesRL), abox(Signature), 
			tbox(TransformedTBox, _EqRoles, QueryPredicates)),
   atom_concat(InFile,'.tbx',OutFile),
	open(OutFile,write,IOFD),
   % test05: a/2
   
	query_predicates_to_sql(TransformedTBox, QueryPredicates,  DBConnections, DBPredicates,Signature,ABox,Query, SQL,IOFD),
/*   
   (
      WriteInp = true ->
         input_tester(TransformedTBox, QueryPredicates,  DBConnections, DBPredicates,Signature, SQL,IOFD)
      ;
      true
   ),     
*/   


	release_kb(URI).

test:-
%   test_file_to_sql('happy2.tst',_).
%   Query = X-(normal(not(b))-[X]),
   Query = X-(normal(c)-[X]),
   test_file_to_sql('zsl_test06.tst',_,true,Query).
    
% to see how the transformed Tbox looks like; must see, if you want to mess with this file; see the commented part in the body of "test_file_to_sql"
write_axioms(axioms(ImpliesCL, ImpliesRL, TransL, ABox, 
						_Concepts, _Roles, DBConnections, DBPredicates)):-
   writec(
      '\n\nImpliesCL:\n'+ImpliesCL+
      '\n\nImpliesRL:\n'+ImpliesRL+
      '\n\nTransL:\n'+TransL+
      '\n\nABox:\n'+ABox+
      '\n\nConcepts:\n'+_Concepts+
      '\n\nRoles:\n'+_Roles+
      '\n\nDBConnections:\n'+DBConnections+
      '\n\nDBPredicates:\n'+DBPredicates).
   
%%%%%%%%%%%%%%%%%%%%  Axioms format

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

%:- spy(query_predicates_to_sql/7).
/*
:- spy(plate/3).
:- spy(query_predicates_to_sql/9).
:- spy(assert_concepts/2).
*/
%:- spy(assert_abox/2).



/*
select * from (select 1 as a) as t1 , (select 2 as b) as t2;

select b.a from (((select 'a' as a) union (select 2+1 as a)) as b );

(select * from (select 1 as a) as t1 , (select 2 as b) as t2) union (select * from (select 5 as a) as t1 , (select 7 as b) as t2);

% a tst file-ban külön kell szerepelnie egy fogalomnak és annak tagadásának,
% a szerepekeknél viszont az inverzet nem kell külön felvenni

*/
