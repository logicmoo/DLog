%Generated from '../examples/iocaste/existtest.dig'.


query(instances(aconcept('C')), [miki]).

concept('C').
concept('B').
concept('A').


implies(some(arole(hasChild), aconcept('B')), aconcept('C')).
implies(aconcept('A'), some(arole(hasChild), aconcept('B'))).



cassertion(aconcept('A'), miki).



