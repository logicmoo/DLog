%Generated from '../examples/alcoholic/ca10.dig'.


query(instances(aconcept('Alcoholic')), []).
query(instances(not(aconcept('Alcoholic'))), []).

concept('Alcoholic').

role(hasFriend).
role(hasParent).

implies(some(arole(hasParent), not(aconcept('Alcoholic'))), not(aconcept('Alcoholic'))).
implies(some(arole(hasFriend), aconcept('Alcoholic')), not(aconcept('Alcoholic'))).



rassertion(arole(hasParent), i0, i1).
rassertion(arole(hasParent), i1, i2).
rassertion(arole(hasFriend), i2, i3).
rassertion(arole(hasParent), i3, i4).
rassertion(arole(hasParent), i4, i5).



