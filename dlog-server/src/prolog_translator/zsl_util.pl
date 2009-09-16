% atom_concattal valo probalkozas

:- module(zsl_util,[atom_append/2,writec/1,writec/2,
   foldl/4,foldr/4,map/3,filter/3,
   filter_distinct/2,
   tmp_id/1,
   breakc/1,break1/0,
   listing_prefix/2   
   ]).

:- use_module(library(lists)).

   
atom_append([Atoms],Out):-
   atom_append2(Atoms,Out). 
   
atom_append2(H+T,Out):-
   !,atom_append2(H,Out2),
   atom_concat(Out2,T,Out).

atom_append2(H,H).   
   
:- abolish(tmp_id_counter/1).
:- dynamic(tmp_id_counter/1).

% szamlalo az atmeneti tablak megszamozasara
tmp_id_counter(1).
tmp_id(X):- 
   tmp_id_counter(X),
   X1 is X+1,
   abolish(tmp_id_counter/1),
   assert(tmp_id_counter(X1)),
   breakc(X=14).

   
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
   
% azt szuri ki, amire Goal nem teljesul
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
   
map([],_,[]).
% +[H|T],+Goal,
% (X,Y):A Goal fejresze
% Goal(X)=Y
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
   
% kiszuri az ismetlodeseket   
filter_distinct([InH|InT],Out):-
   !,filter(InT,X-(X\==InH),InTFilt),   
   filter_distinct(InTFilt,Out2),
%   writec('InFilt: '+InTFilt),
   Out = [InH|Out2].
   
filter_distinct([E],[E]).
filter_distinct([],[]).
   
listing_prefix(Pref,Name/Arity):-
   current_functor(Name,Arity),
   atom_concat(Pref,_,Name).

break1.

breakc(G):-
   (
      call(G) ->
      break1
   ;
      true
   ).
   
:- spy(break1/0).   
