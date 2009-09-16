:- module(assoc_array,[
	put_multi_assoc/4,
	remove_multi_assoc/4,
	pop_multi_assoc/4
	]).
:-use_module(library(assoc)).
:-use_module(library(lists)).

/* About this module:
"multi_assoc" predicates let you assign more than one Values to a Key.
Actually a List is assigned to a Key, and the values will be popped in a FIFO order.

Usage example: see predicate test/0



*/



/*
put_multi_assoc(+Key, +Assoc, +Value, ?NewAssoc)
    NewAssoc is an association list identical to Assoc except that Key is associated with Value. This can be used to insert new values. 	 
*/	 
put_multi_assoc(Key,Assoc,Value,NewAssoc):-	
	( 
		get_assoc(Key,Assoc,VList) -> append(VList,[Value],VList2) ;
		VList2 = [Value]
	),
	put_assoc(Key,Assoc,VList2,NewAssoc).

/*	
 Remove a key-value pair; as a key might have assigned to several values, and a value might be assigned to several keys, both must be given.
*/ 
% remove_multi_assoc(+Key,+Assoc,+Value,-NewAssoc)	
remove_multi_assoc(Key,Assoc,Value,NewAssoc):-
	get_assoc(Key,Assoc,VList),
	delete(VList,Value,VList2),
   % overwrite
	put_assoc(Key,Assoc,VList2,NewAssoc).

% Get and delete the first Value assigned to the Key.
% pop_multi_assoc(+Key,+Assoc,-Value,-NewAssoc)

pop_multi_assoc(Key,Assoc,Value,NewAssoc):-
	get_assoc(Key,Assoc,[H|T]),
   % overwrite 
	put_assoc(Key,Assoc,T,NewAssoc),
	Value=H.
	
	
	
		
	


test:-
	empty_assoc(Assoc),
	put_multi_assoc(1,Assoc,a,Assoc2),
	put_multi_assoc(1,Assoc2,b,Assoc3),
	nl,nl,
	pop_multi_assoc(1,Assoc3,X,Assoc4),	
	write(X),nl,
	pop_multi_assoc(1,Assoc4,Y,Assoc5),
	write(Y),nl,
	get_assoc(1,Assoc3,L),
	write(L).
/* output:
a
b
[a,b]
*/
