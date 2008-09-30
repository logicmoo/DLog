%Generated from '../examples/shi/inverse.dig'.

query(instances(aconcept('Okos')), [o1]).
query(instances(aconcept('Boldog')), []).


implies(some(inv(arole(gyereke)), aconcept('Okos')), aconcept('Boldog')).

subrole(arole(gyereke), arole(utodja)).
subrole(arole(hasParent), arole(szuloje)).
subrole(arole(szuloje), arole(hasParent)).
subrole(arole(szuloje), inv(arole(gyereke))).
subrole(inv(arole(gyereke)), arole(szuloje)).


cassertion(aconcept('Okos'), o1).
rassertion(inv(arole(gyereke)), e, a).
rassertion(arole(gyereke), a, d).
rassertion(arole(gyereke), a, c).
rassertion(arole(gyereke), a, b).



