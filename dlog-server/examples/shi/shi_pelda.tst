%Generated from '../examples/shi/shi_pelda.dig'.

query(instances(aconcept('Gazdag')), [d, e, c]).
query(instances(aconcept('Boldog')), [d, e, a]).



implies(some(inv(arole(utodja)), aconcept('Gazdag')), aconcept('Gazdag')).
implies(some(arole(gyereke), aconcept('Gazdag')), aconcept('Boldog')).

subrole(arole(gyereke), arole(utodja)).

transitive(arole(utodja)).

cassertion(aconcept('Gazdag'), d).
rassertion(arole(gyereke), e, c).
rassertion(arole(gyereke), d, e).
rassertion(arole(gyereke), a, c).
rassertion(arole(gyereke), a, b).



