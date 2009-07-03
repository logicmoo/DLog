options([abox_target(allinonefile), tbox_target(allinonefile), indexing(yes)]).
options([abox_target(allinonefile), tbox_target(allinonefile), indexing(no)]). %All DB -> indexing doesn't help at all
options([indexing(no)]).
options([indexing(yes)]).

concept('Good').
concept('Patricide').
role(hasChild).
equiv(and([some(arole(hasChild), and([aconcept('Patricide'), some(arole(hasChild), not(aconcept('Patricide')))]))]), aconcept('Good')).

dbConnection(iodb, iocaste).
dbAccess('Patricide'/1, iodb, 
	queries('SELECT name FROM people WHERE patricide', 
			'SELECT 1 FROM people WHERE name = ? AND patricide')).
dbAccess(not('Patricide')/1, iodb, 
	queries('SELECT name FROM people WHERE NOT patricide',
			'SELECT 1 FROM people WHERE name = ? AND NOT patricide')).
dbAccess(hasChild/2, iodb, 
	queries('SELECT parent, child FROM hasChild', 
			'SELECT parent FROM hasChild WHERE child = ?',
			'SELECT child FROM hasChild WHERE parent = ?',
			'SELECT 1 FROM hasChild WHERE parent = ? AND child = ?')).


query(instances(aconcept('Good')), [i1]).
query(instance(i1, aconcept('Good'))). %default answer: true
query(instance(i2, aconcept('Good')), false).
query(roleFillers(i1, arole(hasChild)), [i2, i3]).
query(relatedIndividuals(arole(hasChild)), [i1-i2, i1-i3, i3-i4, i2-i3]).

