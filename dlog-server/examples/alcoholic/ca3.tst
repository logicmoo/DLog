%Generated from '../examples/alcoholic/ca3.dig'.


query(instances(aconcept('Alcoholic')), []).
query(instances(not(aconcept('Alcoholic'))), [i0, i1, i2]).

concept('Alcoholic').

role(hasFriend).
role(hasParent).

implies(some(arole(hasParent), not(aconcept('Alcoholic'))), not(aconcept('Alcoholic'))).
implies(some(arole(hasFriend), aconcept('Alcoholic')), not(aconcept('Alcoholic'))).



rassertion(arole(hasParent), i0, i1).
rassertion(arole(hasParent), i1, i2).
rassertion(arole(hasParent), i2, i3).
rassertion(arole(hasFriend), i3, i2).



