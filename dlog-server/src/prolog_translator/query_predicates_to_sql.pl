% gtrace,test.
% listing_prefix(dl2sql,X).
% :- module(query_predicates_to_sql, [query_predicates_to_sql/5, test_file_to_sql/2]).
:- module(query_predicates_to_sql,[]).

:- use_module('../test/dlog_test', [read_test_file/4]).
:- use_module('../core/kb_manager', [new_kb/1, release_kb/1]).
:- use_module('../core/config', [set_dlog_option/3]).
:- use_module('../dl_translator/axioms_to_clauses', [axioms_to_clauses/4]).
:- use_module('../prolog_translator/abox_signature', [abox_signature/4]).
:- use_module('../prolog_translator/tbox_translator', [tbox2prolog/4]).
% :- use_module('pl2sql').

:- use_module('presql_struct2sql.pl').
:- use_module('zsl_util').
%:- load_files('query_to_sql_helpers/mute_debug.pl').

%debugflag deb1
deb1(yes).


write_rule(IOFD,Rule):-
   writecdeb('\n----------------------\n\n'+Rule,deb1).
   
/*
The filter_role/filter_concept predicates are working on the internal 
representation of Tboxes, which isn't pretty. It isn't documented, so I could 
only rely on best guesses. That's why there are "bug-suspicious" comments.
*/

/*    
The Trivial variable shows whether it's about a simple Abox ref or it's a compund concept/role
*/ 
filter_role(Rule,[trivial=Trivial],Id):-
   (
      % inverse of trivial role
      Rule = role(inv(PRN)-[], primary, [ (inv(PRN)-[_G14714, _G14717]:-abox(idx(PRN))-[_G14714, _G14717])])
      ->
         (
         T=[] ->
               Trivial = true,
               assert(dl2sql_role_abox(Id,inv(PRN)-[_V1,_V2]))
         ;
            true
            ,writecdeb('\n  inv role \n'+T+'\n\n',deb1)
         )
      ;
      %trivial role
      Rule = role(PRN-[], primary, [ (PRN-[_G14784, _G14787]:-abox(PRN)-[_G14784, _G14787])|T]) 
      -> 
         (

            T=[] ->
               Trivial = true,
               assert(dl2sql_role_abox(Id,PRN-[_V1,_V2]))
            ;
            true
            ,writecdeb('\n  role \n'+T+'\n\n',deb1)
         )
      ;           
      Rule = role(inv(_)-[],hierarchy,_)
      ->
      Trivial = false     
      ;     
      % TODO hier filter role
      %szerep tartalmazas      
      %ezeket egyelore nem dolgozom fel            
      Rule = role(PRN-[],hierarchy,[PRN-[V1,V2]:-PRN2-[V1,V2]]),
      assert(dl2sql_role(Id,PRN-[V1,V2],PRN2-[V1,V2])),
      assert(dl2sql_role(Id,inv(PRN)-[V1,V2],inv(PRN2)-[V1,V2]))
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
                  Trivial=true
                  %, assert(dl2sql_concept_abox(Id,(not(PRN))-[_]))
               ;
               writecdeb('\n  neg concept \n'+T+'\n',deb1)
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
                  Trivial=true
                  %, assert(dl2sql_concept_abox(Id,(PRN)-[_]))
                  % dl2sql_dbpredicate alapjan donteni el assert-e vagy sem
               ;
               true
               ,writecdeb('\n concept \n'+T+'\n',deb1)
               ,assert_concepts(T,Id)
               %,assert(dl2sql_concept(Id,T))           
               %2
            )
         )
      ;
      % concept from a role: role.top (probably bug)
      Rule = concept(PRN-[], general, [ (PRN-[_G6646]:-init_state(_G6728), normal(PRN)-[_G6646, _G6728])])
      ->
         Trivial=true
   ).

assert_concepts([],_).   
assert_concepts([H|T],Id):-
   H=(HC:-HB),
   assert(dl2sql_concept(Id,HC,HB)),
   assert_concepts(T,Id).
   
/*   
 The so called Transformed TBox still has some ABOX references, so 
 I just print them and mark them as "trivial". It also aserts the "non-trivial" ones.
 */
filter_trivial(IOFD,Rule,Id):-
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
      Trivial = false ->
         writecdeb(IOFD,'Ok:\n'+Rule+'\n\n',deb1)
      ;
      writecdeb(IOFD,'filtered:\n'+Rule+'\n\n',deb1)
   ).
assert_dbpredicates([],_).
assert_dbpredicates([H|T],Id):-
   assert(dl2sql_dbpredicate(Id,H)),
   assert_dbpredicates(T,Id).


/*
make_concept_aboxref(+Id,+Query,-RefList):-
    searches the concept in the Abox ( DB or memory )

*/
make_concept_aboxref(Id,Query,RefList):-
   Query = (PRN)-[X],
   atom(PRN),
   (
      dl2sql_concept_db_abox(Id,Query,_)
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
   
   
/* commalist2list(+CommaL,-List)
    Converts from a "commalist" to normal list
        (Commalist is used e.g. as the right hand side form of predicates in TBOX inner
        representation)
    Example:
     (a,b,c,d) -> [a,b,c,d]
*/

   
commalist2list(CommaL,List):-
	(   
	CommaL = ','(H,T) ->	
	List = [H|TC],
	    commalist2list(T,TC)
	;   
	List = [CommaL|[]]
	).

% TODO compound role - yet to be tested
plate_compound_role(Query,PlatedQuery,Id):-
   bagof(PLQNested,
   (
      dl2sql_role(Id,Query,RHS),plate(RHS,PLQNested,Id)
   ),RHS_list),
   tmp_id(TID),
   make_role_aboxref(Id,Query,AboxList),
   Query = PRN-[X,Y],
   append(RHS_list,AboxList,List),
   PlatedQuery = union(TID,[X,Y],List).     
   
plate_compound_concept(Query,PlatedQuery,Id):-
   Query = (_PRN)-[X],
   bagof(Concept_RHS,
      (
         dl2sql_concept(Id,Query,Concept_RHS)
      ),List_of_RHS),
      plate_compound_concept_it(List_of_RHS,Joins,[X],Id),
   %TODO compund concept - yet to be tested
   %break1,
           
                  
   /* and we add the Abox too
        In case of a mixed Tbox/Abox predicate
    */
   (
    make_concept_aboxref(Id,Query,Aboxref) ->
       append(Joins,Aboxref,UnionList),
       tmp_id(Unionid),
       PlatedQuery = union(Unionid,[X],UnionList)    
    ;
    [PlatedQuery] = Joins
   ).
   
% plate_compound_concept_it(+[H|T],-Joins,+[X],+Id):-
/* 
It makes a list of the right-sides, and plates them too, if neccesary 

    X remains a variable all the way (placeholder for queried argument in a "relation")
    Joins : gives back the resulting presql struct
    [H|T] : list of predicate right-hand-sides (elements in the list are in disjunction with each other)
*/
plate_compound_concept_it([],[],_,Id).   
plate_compound_concept_it([H|T],Joins,[X],Id):-
         plate(H,Plated_H,Id),
         commalist2list(Plated_H,JoinBody),
         plate_compound_concept_it(T,Joins_T,[X],Id),
         tmp_id(Joinid),
         Joins= [join(Joinid,[X],JoinBody)|Joins_T].
         
         
         %append(Plated_H_wo_commas,List_of_plated_RHS_T_wo_commas,
          %  List_of_plated_RHS).
         
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

          
/*   
 plates compund concepts/roles: the output is PreSQLStruct
 i.e. resolves compund concepts/roles with abox concepts/roles
*/
plate(Query,PlatedQuery,Id):-   
   (   
   % compound concept - substitution with right hand side
   clause(dl2sql_concept(Id,Query,_),_)
      ->
   plate_compound_concept(Query,PlatedQuery,Id)      
   ;
   %TODO plate role
   clause(dl2sql_role(Id,Query,_),_)
      ->
   plate_compound_role(Query,PlatedQuery,Id)      
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
      break1,
      Query = PRN-[X],
      atom(PRN),
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
      (
        PRN=not(X) ->
        assert(dl2sql_concept_db_abox(Id,(PRN)-[_],C))
        ;
        assert(dl2sql_concept_db_abox(Id,normal(PRN)-[_],C))
      )
   ;   
      A=PRN/2
      ->
      (
         assert(dl2sql_role_db_abox(Id,PRN-[_,_],C)),
         assert(dl2sql_role_db_abox(Id,inv(PRN)-[_,_],C))
      )
   ).

assert_mem_abox(Id,X):-
    writecdeb('memabox:\n'+X,deb1),
   (
      X = cassertion(aconcept(PRN),Inst)
      ->
      (
        PRN=not(_) ->
            assert(dl2sql_cassertion(Id,(PRN)-[_],Inst))    
        ;
        assert(dl2sql_cassertion(Id,normal(PRN)-[_],Inst))
      )
      
   ;
      X = rassertion(arole(PRN),Inst1,Inst2)
      ->
      assert(dl2sql_rassertion(Id,PRN-[_,_],[Inst1,Inst2]))
   ).

%query_predicates_to_sql(+TransformedTBox, +QueryPredicates, +DBConnections, +DBPredicates, -SQL)
%lehet, hogy majd még egy KB URI-t is kell kapnod (ha beállításokat használsz)
query_predicates_to_sql(TransformedTBox, QueryPredicates, _DBConnections, DBPredicates,Signature,ABox,Query, SQL,IOFD):-
   	
   writecdeb(IOFD,'Transformed Tbox: \n\n',deb1),
   tmp_id(Id),
   assert_dbpredicates(DBPredicates,Id),
   bagof(X,(member(X,TransformedTBox),filter_trivial(IOFD,X,Id)),_),
   (
      DBPredicates = [],!
      ;
      bagof(X,(member(X,DBPredicates),assert_db_abox(Id,X)),_)
   ),
   (
      ABox = [],!
      ;
      bagof(X,(member(X,ABox),assert_mem_abox(Id,X)),_),!      
   ),
    breakc(true),
	writecdeb(IOFD,'\n\n----Query predicates\n\n',deb1),
	writecdeb(IOFD,querypred(QueryPredicates),deb1),
	writecdeb(IOFD,'\n\n----Database predicates\n\n',deb1),
	writecdeb(IOFD,dbpred(DBPredicates),deb1),
    writecdeb(IOFD,'\n-----------ABOX signature\n\n',deb1),
	writecdeb(IOFD,abox(Signature),deb1),
    writecdeb(IOFD,'\n-----------ABOX assertions\n\n',deb1),
	writecdeb(IOFD,abox(ABox),deb1),
   

   Query=QBody,
   %writec('\nplate elott:\n'+Query,deb1),
   plate(QBody,PlatedQuery,Id),   
   
   /*
   tmp_id(TID),
   PreSqlStruct = join(TID,[X],[PlatedQuery]),
   */
   PlatedQuery = PreSqlStruct,
   writecdeb('\n\nplated query:\n'+PreSqlStruct,deb1),
   (
      struct2query(top_level,PreSqlStruct,SQLQuery,Id),!,
      writecdeb('\nSQL query:\n\n '+SQLQuery+'\n\n',deb1)
   ;
      writecdeb('\nstruct2query failed\n',deb1)
   ).   

   
% query_predicates_to_sql helper predikatumok
   
clear_mem:-
    abolish(dl2sql_dbpredicate/2),
    abolish(dl2sql_concept/3),
    abolish(dl2sql_concept_abox/3),
    abolish(dl2sql_role_abox/2).
   

%TransformedTBox jelenlegi formátumáért lásd tbox_writer
%DBConnections, DBPredicates formátumáért lásd lent

%test_file_to_sql(+File, -SQL,+Outstream,+Query)
%ideiglenes belépési csonk, amíg kész nem lesz, és nem a normál bejáratot használjuk
%(emiatt kell betölteni a teljes rendszert :) %http://xkcd.com/541/

test_file_to_sql(InFile,SQL,Outstream,Query) :-
	read_test_file(InFile, axioms(ImpliesCL, ImpliesRL, TransL, ABox, 
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

   atom_concat(InFile,'.tbx',OutFileName),
   working_directory(Wdir,_),
   writec('infile: '+InFile+'\n'+'outfile: '+OutFileName+'\n'+'working directory: '+Wdir+'\n'),
   open(OutFileName,write,IOFD),
   % test05: a/2
%/*
   write_axioms(axioms(ImpliesCL, ImpliesRL, TransL, ABox, 
						_Concepts, _Roles, DBConnections, DBPredicates,QueryPredicates)),
%*/                  
    
	query_predicates_to_sql(TransformedTBox, QueryPredicates,  DBConnections, DBPredicates,Signature,ABox,Query, SQL,IOFD),
	close(IOFD),
	clear_mem,
	writecdeb('\nhello mutedubug!\n',deb1),
	release_kb(URI).

test_subrole(1):-
%   test_file_to_sql('happy2.tst',_).
%   Query = X-((not(b))-[X]),
   Query = ((c)-[X]),
   test_file_to_sql('zsl_test_subrole01.tst',_,true,Query).
   
test_subrole(2):-   
   Query = (normal(d)-[X]),
   test_file_to_sql('zsl_test_subrole02.tst',_,true,Query).


test(db6):-
   Query = (normal(c)-[X]),
   test_file_to_sql('zsl_test06.tst',_,true,Query).
   
test(db7):-
   Query = (normal(c)-[X]),
   test_file_to_sql('zsl_test07.tst',_,true,Query).
   
test(db8):-
   Query = (normal(c)-[X]),
   test_file_to_sql('zsl_test08.tst',_,true,Query).
   
   
   
    
% to see how the transformed Tbox looks like; must see, if you want to mess with this file; see the commented part in the body of "test_file_to_sql"
write_axioms(axioms(ImpliesCL, ImpliesRL, TransL, ABox, 
						_Concepts, _Roles, DBConnections, DBPredicates,QueryPredicates)):-
   writecdeb(
      '\n\nImpliesCL:\n'+ImpliesCL+
      '\n\nImpliesRL:\n'+ImpliesRL+
      '\n\nTransL:\n'+TransL+
      '\n\nABox:\n'+ABox+
      '\n\nConcepts:\n'+_Concepts+
      '\n\nRoles:\n'+_Roles+
      '\n\nDBConnections:\n'+DBConnections+
      '\n\nDBPredicates:\n'+DBPredicates+
      '\n\nQueryPredicates:\n'+QueryPredicates+
      '--------',deb1).
   
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
/*
:- spy(plate/3).
:- spy(query_predicates_to_sql/9).
:- spy(assert_db_abox/2).
*/
%:- spy(filter_concept/3).

:- spy(plate/3).
%:- spy(plate_compound_concept/3).
:- spy(make_concept_aboxref/3).
:- dynamic(dl2sql_concept_db_abox/3).
%:- spy(assert_concepts/2).

% ends term_expansion
%:- end_debug.

