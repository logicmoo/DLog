% gtrace,test.
:- module(presql_struct2sql,[struct2query/4]).
:- use_module('zsl_util').
:- use_module('assoc_array').
:- use_module(library(lists)).


struct2query(union_body,[],'',Id).
   

   
struct2query(union_body,[H|T],SQLQuery,Id):-
   (
      T=[]
      ->
      struct2query(top_level,H,SQLQNested,Id),
      atom_append(['('+SQLQNested+')'],SQLQuery)
   ;
      struct2query(top_level,H,QH,Id),
      struct2query(union_body,T,QT,Id),
      atom_append(['('+QH+')'+' UNION '+QT],SQLQuery)
   ).
   

struct2query(mem_top,Struct,SQLQuery,Id):-
   (
      Struct = mem_abox(TID,[X,Y],PRN,Inst)
      ->
      struct2query(mem_list_role,Inst,NestedQuery,first=true,Id),
      atom_append(['SELECT * FROM ( '+NestedQuery+' ) as t_'+TID],SQLQuery)
   ;
      Struct = mem_abox(TID,[X],PRN,Inst)
      ->
      struct2query(mem_list_concept,Inst,NestedQuery,first=true,Id),
      atom_append(['SELECT * FROM ( '+NestedQuery+' ) as t_'+TID],SQLQuery)   
   ).

struct2query(mem_list_concept,[H|T],SQLQuery,first=true,Id):-
   (
      T = []
      ->
      atom_append(['SELECT \''+H+'\' AS subject'],SQLQuery)
   ;   
      struct2query(mem_list_concept,T,SQLQuery2,first=false,Id),
      atom_append(['SELECT \''+H+'\' AS subject UNION '+SQLQuery2],SQLQuery)
   ).

struct2query(mem_list_concept,[H|T],SQLQuery,first=false,Id):-
   (
      T = []
      ->
      atom_append(['SELECT \''+H+'\' '],SQLQuery)
   ;   
      struct2query(mem_list_concept,T,SQLQuery2,first=false,Id),
      atom_append(['SELECT \''+H+'\' UNION '+SQLQuery2],SQLQuery)
   ).
   

struct2query(mem_list_role,[HX-HY|T],SQLQuery,first=true,Id):-
   (
      T = []
      ->
      atom_append(['SELECT \''+HX+'\' AS subject, \''+HY+'\' AS object '],SQLQuery)
   ;   
      struct2query(mem_list_role,T,SQLQuery2,first=false,Id),
      atom_append(['SELECT \''+HX+'\' AS subject, \''+HY+'\' AS object UNION '+SQLQuery2],SQLQuery)
   ).   

struct2query(mem_list_role,[HX-HY|T],SQLQuery,first=false,Id):-
   (
      T = []
      ->
      atom_append(['SELECT \''+HX+'\', \''+HY+'\' '],SQLQuery)
   ;   
      struct2query(mem_list_role,T,SQLQuery2,first=false,Id),
      atom_append(['SELECT \''+HX+'\', \''+HY+'\' UNION '+SQLQuery2],SQLQuery)
   ).   
   

struct2query(join_body,PreSqlStruct,SQLQuery,Id):-
   PreSqlStruct = join(_,[X],LBody),
   % legfelso szint fejreszeinek valtozoit gyujti ossze
   collect_variables(LBody,InvertedStruct),
   invertedS2sql(LBody,InvertedStruct,FromTablesSQL,WhereSQL,Id),
   pop_multi_assoc(X,InvertedStruct,TID/ParamNum,_),
   (
      ParamNum = 1
      ->
      Column = 'subject '
   ;
      ParamNum = 2
      ->
      Column = 'object '
   ),
   atom_append(['SELECT t_'+TID+'.'+Column+' FROM  '+FromTablesSQL+'  '+WhereSQL],SQLQuery).
   
invertedS2sql(LBody,InvertedStruct,FromTablesSQL,WhereSQL,Id):-
   invertedS2sql_fromtable_iterator(LBody,FromTablesSQL,Id),
   assoc_to_keys(InvertedStruct,Vars),
   invertedS2sql_where_iterator(Vars,InvertedStruct,'',WhereSQL2,first=true),
   atom_append([' WHERE true '+WhereSQL2],WhereSQL).
   
             
invertedS2sql_fromtable_iterator(LBody,SQLQuery,Id):-
   (
      LBody = [H|T],T\=[]
      ->
      (
      arg(1,H,TID),
      struct2query(top_level,H,NestedQuerySQL2,Id),
      invertedS2sql_fromtable_iterator(T,NestedQuerySQL3,Id),
      atom_append([' ( '+NestedQuerySQL2+' ) as t_'+TID+' ,'+NestedQuerySQL3],SQLQuery)
      )
   ;
      LBody = [LB],
      arg(1,LB,TID),
      struct2query(top_level,LB,NestedQuerySQL2,Id),
%      NestedQuerySQL2 = SQLQuery
      atom_append([' ( '+NestedQuerySQL2+' ) as t_'+TID+' '],SQLQuery)
   ).   

% [H|T] is the list of all the Vars that occur in "clause" heads   
invertedS2sql_where_iterator([H|T],InvertedStruct,WhereSQLIn,WhereSQLOut,first=First):-
   get_assoc(H,InvertedStruct,TIDS),
   (
      T=[]
      ->
      invertedS2sql_where_iterator_iterator(TIDS,CondPartSQL,first=First),
      atom_append([WhereSQLIn+CondPartSQL],WhereSQLOut)
   ;
         
      invertedS2sql_where_iterator_iterator(TIDS,CondPartSQL,first=First),
      atom_append([WhereSQLIn+CondPartSQL],WhereSQL2),
      invertedS2sql_where_iterator(T,InvertedStruct,WhereSQL2,WhereSQLOut,first=false)
   ).
      

% the "first" arg isn't used for now;
% it would indicate whether an "and" word is needed in the "WHERE" part
invertedS2sql_where_iterator_iterator(TIDS,CondPartSQL,first=First):-
   (
      % legalabb 2 valtozo kell a joinhoz
      TIDS=[_]      
      ->
      CondPartSQL=''
   ;
      (
         (
            First = true -> And = ' and '
         ;
            And = ' and '
         ),
         TIDS = [H1|[H2|T]],
         H1 = TID1/ParamNumber1,
         (
            ParamNumber1 = 1
            ->
            atom_append([And+' t_'+TID1+'.subject '],CondPart2)
         ;
            ParamNumber1 = 2
            ->
            atom_append([And+' t_'+TID1+'.object '],CondPart2)
         ),
         H2 = TID2/ParamNumber2,
         (
            ParamNumber2 = 1
            ->
            atom_append([CondPart2+'= t_'+TID2+'.subject '],CondPart3)
         ;
            ParamNumber2 = 2
            ->
            atom_append([CondPart2+'= t_'+TID2+'.object '],CondPart3)
         ),
         (
            T=[]
            ->
            CondPartSQL = CondPart3
         ;
            invertedS2sql_where_iterator_iterator([H2|T],CondPart4,first=false),
            atom_append([CondPart3+CondPart4],CondPartSQL)
         )
      )
   ).
  

collect_variables(LBody,InvertedStruct):-
   empty_assoc(AssocIn),
   collect_variables_iterator(LBody,AssocIn,InvertedStruct).

collect_variables_iterator(LBody,AssocIn,AssocOut):-
   (
      LBody = [H|T],T\=[]
      ->
      (
      collect_variables_helper(H,AssocIn,Assoc2),
      collect_variables_iterator(T,Assoc2,AssocOut)
      )
   ;
      LBody = [LB],
      collect_variables_helper(LB,AssocIn,AssocOut)
   ).


collect_variables_helper(Elem,AssocIn,AssocOut):-   
   Elem=..HList,
   nth0(1,HList,TID),
   nth0(2,HList,Vars),
   
   Vars = [VH|VB],
   put_multi_assoc(VH,AssocIn,TID/1,Assoc2),
   (
      VB = []
      ->
      AssocOut = Assoc2
      
   ;
      VB = [VH2|[]],
      put_multi_assoc(VH2,Assoc2,TID/2,AssocOut)
   ).
   
   
   
   

struct2query(top_level,PreSqlStruct,SQLQuery,Id):-
   breakc(arg(1,PreSqlStruct,5)),
   (
      %TID:Table ID
      PreSqlStruct = union(TID,_,UBody)
      ->
      struct2query(union_body,UBody,SQLQNested,Id),
      atom_append(['SELECT * FROM ( '+SQLQNested+' ) AS t_'+TID],SQLQuery)            
   ;
      PreSqlStruct = join(TID,[X],LBody)
      ->
      struct2query(join_body,PreSqlStruct,SQLQNested,Id),
      atom_append(['SELECT * FROM ( '+SQLQNested+' ) AS t_'+TID],SQLQuery)      
   ;
      PreSqlStruct = sql_abox(TID,[X,Y],PRN)
      ->
      dl2sql_role_db_abox(Id,PRN-[_,_],query(Qtmp)),
      atom_append(['SELECT * FROM ('+Qtmp+')'+' AS '+'t_'+TID],SQLQuery)
   ;
      PreSqlStruct = sql_abox(TID,[X],PRN)
      ->
      dl2sql_concept_db_abox(Id,normal(PRN)-[_],query(Qtmp)),
      atom_append(['SELECT * FROM ('+Qtmp+')'+' AS '+'t_'+TID],SQLQuery)      
   ;   
   
      PreSqlStruct = mem_abox(TID,_,_,_)
      ->
      struct2query(mem_top,PreSqlStruct,SQLQuery,Id)
   ).
   

%:- spy(struct2query/4).    
:- spy(invertedS2sql_fromtable_iterator/3).
