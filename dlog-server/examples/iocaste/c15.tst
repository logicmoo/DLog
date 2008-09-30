%Generated from '../examples/iocaste/c15.dig'.


query(instances(aconcept('Good')), [i1]).

concept('Good').
concept('Patricide').

role(hasChild).

implies(and([some(arole(hasChild), and([aconcept('Patricide'), some(arole(hasChild), not(aconcept('Patricide')))]))]), aconcept('Good')).
implies(aconcept('Good'), and([some(arole(hasChild), and([aconcept('Patricide'), some(arole(hasChild), not(aconcept('Patricide')))]))])).



cassertion(not(aconcept('Patricide')), i17).
cassertion(aconcept('Patricide'), i2).
rassertion(arole(hasChild), i16, i17).
rassertion(arole(hasChild), i15, i16).
rassertion(arole(hasChild), i14, i15).
rassertion(arole(hasChild), i13, i14).
rassertion(arole(hasChild), i12, i13).
rassertion(arole(hasChild), i11, i12).
rassertion(arole(hasChild), i10, i11).
rassertion(arole(hasChild), i9, i10).
rassertion(arole(hasChild), i8, i9).
rassertion(arole(hasChild), i7, i8).
rassertion(arole(hasChild), i6, i7).
rassertion(arole(hasChild), i5, i6).
rassertion(arole(hasChild), i4, i5).
rassertion(arole(hasChild), i3, i4).
rassertion(arole(hasChild), i2, i3).
rassertion(arole(hasChild), i1, i16).
rassertion(arole(hasChild), i1, i15).
rassertion(arole(hasChild), i1, i14).
rassertion(arole(hasChild), i1, i13).
rassertion(arole(hasChild), i1, i12).
rassertion(arole(hasChild), i1, i11).
rassertion(arole(hasChild), i1, i10).
rassertion(arole(hasChild), i1, i9).
rassertion(arole(hasChild), i1, i8).
rassertion(arole(hasChild), i1, i7).
rassertion(arole(hasChild), i1, i6).
rassertion(arole(hasChild), i1, i5).
rassertion(arole(hasChild), i1, i4).
rassertion(arole(hasChild), i1, i3).
rassertion(arole(hasChild), i1, i2).



