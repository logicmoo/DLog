

:- module(zsl_util,[atom_append/2,writec/1,writec/2,writecdeb/2,writecdeb/3,
   foldl/4,foldr/4,map/3,filter/3,
   filter_distinct/2,
   tmp_id/1,
   breakc/1,break1/0,
   listing_prefix/2,
   append_all/2,
   tr/0
%TODO ez miert kerult be?
%   ,term_expansion/2   
   ]).
:- meta_predicate
        writecdeb(+,1).
:- meta_predicate
        writecdeb(+,+,1).

      
    

:- use_module(library(lists)).




% usage: atom_append([a+b+c],abc).   
% atom_append([+Atoms],?Out):-
atom_append([Atoms],Out):-
   atom_append2(Atoms,Out). 
   
atom_append2(H+T,Out):-
   !,atom_append2(H,Out2),
   atom_concat(Out2,T,Out).

atom_append2(H,H).   
   
:- abolish(tmp_id_counter/1).
:- dynamic(tmp_id_counter/1).

% counter for temporary tables and other stuff that might need a unique ID
tmp_id_counter(1).
tmp_id(X):- 
   tmp_id_counter(X),
   X1 is X+1,
   abolish(tmp_id_counter/1),
   assert(tmp_id_counter(X1)).
   

% writecdeb: write-compound for debugging
writecdeb(STR,M:DEBUGFLAG):-!,

    Goal =.. [DEBUGFLAG,X],
    (
        call(M:Goal),
        (
            X=yes,writec(STR),!;
            X=no,!;
            throw('writec exception: illegal flag; only permitted yes or no')
        )
    ;
        throw('writec exception: undefined flag:'+DEBUGFLAG+' in module '+M)
    ).
    
    
writecdeb(IOFD,STR,M:DEBUGFLAG):-!,
    Goal =.. [DEBUGFLAG,X],
    (
        call(M:Goal),
        (
            X=yes,writec(IOFD,STR),!;
            X=no,!;
            throw('writec exception: illegal flag; only permitted yes or no')
        )
    ;
        throw('writec exception: undefined flag:'+DEBUGFLAG+' in module '+M)
    ).
    
    
/* write compound
     similar to the print functions in Java/Python/Pascal:
     + operator can be used for concatenation
 */
writec(H+T):-   
   !,writec(H),
   write_term(T,[]).
   


writec(E):-
   write_term(E,[]).
   
writec(nil,_):-!.
   
writec(IOFD,H+T):-   
   !,writec(IOFD,H),
   write_term(IOFD,T,[]).

writec(IOFD,E):-
   write_term(IOFD,E,[]).

    
   
% filters out for what Goal is false
filter([],_,[]).      
filter([H|T],X-Goal,Out):-
   copy_term(X-Goal,CX-CGoal),
   CX=H,
   filter(T,X-Goal,FTail),
   (  
      call(CGoal) -> Out = [H|FTail]
   ;
      Out = FTail
   ).
   
% map(+InList,+Func,-OutList)   
% Func = (+X,-Y)-(Funcbody)
map([],_,[]).
map([H|T],(X,Y)-Goal,[H2|T2]):-
   copy_term(X-Y-Goal,X2-Y2-Goal2),
   X2=H,
   Y2=H2,
   call(Goal2),
   map(T,(X,Y)-Goal,T2).

   
/*
| ?- foldr([[1],[2,3],[4]],(X1,X2,Y)-append(X1,X2,Y),[],Out).
Out = [1,2,3,4] ? 
*/   
foldr([H|T],(X1,X2,Y)-Goal,Init,Out):-
   foldr_helper(T,H,(X1,X2,Y)-Goal,Init,Out).
   
foldr_helper([],ListElement,(X1,X2,Y)-Goal,Init,Out):-
   copy_term(X1-X2-Y-Goal,X1C-X2C-YC-GoalC),
   X1C=ListElement,
   X2C=Init,
   call(GoalC),
   Out=YC.

foldr_helper([H|T],ListElement,(X1,X2,Y)-Goal,Init,Out):-
   copy_term(X1-X2-Y-Goal,X1C-X2C-YC-GoalC),
   foldr_helper(T,H,(X1,X2,Y)-Goal,Init,Out2),
   X1C=ListElement,
   X2C=Out2,
   call(GoalC),
   Out=YC.

/*
| ?- foldl([[1],[2,3],[4]],(X1,X2,Y)-append(X1,X2,Y),[],Out).
Out = [4,2,3,1] ? 
*/
foldl([H|T],(X1,X2,Y)-Goal,Init,Out):-
   foldl_helper(T,H,(X1,X2,Y)-Goal,Init,Out).

foldl_helper([],H,(X1,X2,Y)-Goal,Init,Out):-
   copy_term(X1-X2-Y-Goal,X1C-X2C-YC-GoalC),
   X1C=H,
   X2C=Init,
   call(GoalC),
   Out=YC.   

foldl_helper(T,H,(X1,X2,Y)-Goal,Init,Out):-
   copy_term(X1-X2-Y-Goal,X1C-X2C-YC-GoalC),
   X1C=H,
   X2C=Init,
   call(GoalC),
   foldl(T,(X1,X2,Y)-Goal,YC,Out).
   
% filters out the recurrence
filter_distinct([InH|InT],Out):-
   !,filter(InT,X-(X\==InH),InTFilt),   
   filter_distinct(InTFilt,Out2),
%   writec('InFilt: '+InTFilt),
   Out = [InH|Out2].
   
filter_distinct([E],[E]).
filter_distinct([],[]).
   
% utility for listing predicates beginning with a prefix
% listing_prefix(+Pref,?Name/?Arity):-
listing_prefix(Pref,Name/Arity):-
   current_functor(Name,Arity),
   atom_concat(Pref,_,Name).

% breakpoint - it always breaks   
break1.

% conditional breakpoint
breakc(G):-
   (
      call(G) ->
      break1
   ;
      true
   ).
   
:- spy(break1/0).

append_all([[]],[]):-!.
% this only matches a list with one element
append_all([[X]],[X]):-!.
append_all([H1,H2|T],Out):-
   append(H1,H2,Res),
   ( 
      T = [] ->
      Out = Res
   ;
      append_all([Res|T],Out)
   ).
   
tr:- gtrace,trace.   
