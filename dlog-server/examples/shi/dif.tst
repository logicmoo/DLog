options([abox_target(allinonefile), tbox_target(allinonefile)]).

query(instances(aconcept('Clever')), [b]).
query(instances(not(aconcept('Clever'))), [c, d]).

concept('Clever').
role(hasChild).

implies(top, atmost(1, arole(hasChild), aconcept('Clever'))).

rassertion(arole(hasChild), a, b).
rassertion(arole(hasChild), a, c).
rassertion(arole(hasChild), a, d).
rassertion(arole(hasChild), b, e).
rassertion(arole(hasChild), b, f).
rassertion(arole(hasChild), c, g).

assertion(aconcept('Clever'), b).
