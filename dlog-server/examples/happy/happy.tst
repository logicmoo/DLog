%Generated from '../examples/happy/happy.dig'.

query(instances(aconcept('Happy')), [i1]).

concept('Happy').
concept('Pretty').
concept('Clever').

role(hasChild).

implies(some(arole(hasChild), and([some(arole(hasChild), aconcept('Clever')), some(arole(hasChild), aconcept('Pretty'))])), aconcept('Happy')).



rassertion(arole(hasChild), i2, i3).
rassertion(arole(hasChild), i1, i2).
cassertion(aconcept('Clever'), i3).
cassertion(aconcept('Pretty'), i3).



