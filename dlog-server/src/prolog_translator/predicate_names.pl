:- module(predicate_names, [predicate_name/2]).

dlog_prefix(Name, PrefN) :-
	atom_concat('$dlog$', Name, PrefN).

concept_prefix(Pred, PrefN) :-
	Pred = not(Name) ->
		atom_concat('not$', Name, PrefN)
	;	PrefN = Pred.


%predicate_name(+Pred, -GeneratedName)
%RName-Indiv, idx(RName)-Indiv -> '$dlog$hasChild$$i1', '$dlog$idx$hasChild$$i1'
predicate_name(Pred-Indiv, GeneratedName) :- !, %A
	(	Pred = idx(IName)
	->	atom_concat('idx$', IName, Name)
	;	Name = Pred
	),		
	atom_concat(Name, $$, DName), %rname$$iname/1 vs not$cname/1 
			%would collide if rname='not' and iname=cname
	atom_concat(DName, Indiv, JName),
	dlog_prefix(JName, GeneratedName).

%idx(RName) -> '$dlog$idx$hasChild'
predicate_name(idx(Pred), GeneratedName) :- !, %A
	atom_concat('idx$', Pred, FName),
	dlog_prefix(FName, GeneratedName).

%choice(CName), choice(not(CName)) -> '$dlog$choice$Good', '$dlog$choice$not$Good'
predicate_name(choice(Name), GeneratedName) :- !, %T
	concept_prefix(Name, Name1), 
	atom_concat('choice$', Name1, CName),
	dlog_prefix(CName, GeneratedName).

%once(CName), once(not(CName))-> '$dlog$once$Good', '$dlog$once$not$Good'
predicate_name(once(Name), GeneratedName) :- !, %T
	concept_prefix(Name, Name1), 
	atom_concat('once$', Name1, CName),
	dlog_prefix(CName, GeneratedName).

%normal(CName), normal(not(CName)) -> '$dlog$normal$Good', '$dlog$normal$not$Good'
predicate_name(normal(Name), GeneratedName) :- !, %T
	concept_prefix(Name, Name1), 
	atom_concat('normal$', Name1, CName),
	dlog_prefix(CName, GeneratedName).

%inv(RName) -> '$dlog$inv$hasChild'
predicate_name(inv(Name), GeneratedName) :- !, %T
	atom_concat('inv$', Name, IName),
	dlog_prefix(IName, GeneratedName).

%RName, CName, not(CName) -> '$dlog$hasChild', '$dlog$Good',  '$dlog$not$Good' %TODO: RName vs CName collision?
predicate_name(Name, GeneratedName) :- %AT
	concept_prefix(Name, Name1), 
	dlog_prefix(Name1, GeneratedName).
