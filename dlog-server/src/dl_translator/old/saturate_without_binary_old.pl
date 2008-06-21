:- module(saturate_without_binary_old,[saturate_without_binary/2]).

:- use_module('../struct').
:- use_module('../saturate').
:- use_module(selectResolvable_old).
:- use_module(library(lists),[member/2, append/3]).

/***************************** Klozhalmaz telitese **********************************/

% saturate_without_binary(+W,-S):-
% W klozok listaja, melyek telitesevel kapjuk S-et
saturate_without_binary(W,S):-
	simplifyClauses(W,W2),
	selectResolvable_without_binary(W2,Selected),
	saturate_without_binary([],Selected,S).
	
% saturate_without_binary(+W1,+W2,-S): W1 es W2 "rendezett klozok" listaja, S-t W1 es W2
% telitesevel nyerjuk ugy hogy W1 es W2-beli klozokat, valamint W2-W2-beli
% klozokat rezolvalunk egymassal. A rezolucio soran figyelmen kivul hagyjuk a binaris literalokat
saturate_without_binary(W1,[],W1).
saturate_without_binary(W1,[C|W2],S):-
	redundant(C,W1), !,
	% print('---- ') ,print(C), print('---- redundans'),nl,
	saturate_without_binary(W1,W2,S).
saturate_without_binary(W1,[C|W2],S):-	
	% print(C), nl,	
	findall(R,(
		   member(R1,W1),			
		   resolve(R1,C,Res),
		   decomposeClause(Res,Decomposed),
		   % print('   + '), print(R1),nl,
		   % print('   = '), print(Decomposed),nl,
		   member(R1,Decomposed),
		   cls_to_ocls_without_binary(R1,R)
		  ), Rs),
	elim_reds(W1,C,EW1),
	append(Rs,W2,EW2),
	saturate_without_binary([C|EW1],EW2,S).


% selectResolvable_without_binary(+Cs,-OCs)
%	Cs klozhalmazban a rezolvalhato literalok
%	elolre hozasaval kapjuk OCs klozhalamzt
selectResolvable_without_binary([],[]).
selectResolvable_without_binary([C|Cs],[OC|OCs]):-	
	cls_to_ocls_without_binary(C,OC),
	selectResolvable_without_binary(Cs,OCs).

% cls_to_ocls_without_binary(+Cls,-OCls): Cls klozban egy rezolvalhato
% literal elore hozasaval kapjuk OCls-t
cls_to_ocls_without_binary([],[]) :- !.
cls_to_ocls_without_binary([Cls],[Cls]):- !.

cls_to_ocls_without_binary(Cls,[Maximal|Rest]):-
	remove_binary(Cls,Ls),
	getGreatest(Ls,Maximal), !,
	select_precisely(Maximal,Cls,Rest).
cls_to_ocls_without_binary(Cls,Cls).

% remove_binary(Cls,Ls)
% Cls listabol eltavolitva a binaris predikatumokat
% (egyenloseget is beleertve) kapjuk Ls listat
remove_binary([],[]).
remove_binary([C|Cls],Ls):-
	( contains_struct(C,arole(_,_,_))
	; contains_struct(C,eq(_,_))
	), !,
	remove_binary(Cls,Ls).
remove_binary([C|Cls],[C|Ls]):-
	remove_binary(Cls,Ls).