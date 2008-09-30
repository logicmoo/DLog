concept('Good').
concept('Patricide').
role(hasChild).
equiv(and([some(arole(hasChild), and([aconcept('Patricide'), some(arole(hasChild), not(aconcept('Patricide')))]))]), aconcept('Good')).
cassertion(not(aconcept('Patricide')), i12).
cassertion(aconcept('Patricide'), i2).
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



query(instances(aconcept('Good')), [i1]).
query(instance(i1, aconcept('Good'))). %default answer: true
query(instance(i2, aconcept('Good')), false).
query(roleFillers(i1, arole(hasChild)), [i2, i3, i4, i5, i6, i7, i8, i9, i10, i11]).
query(relatedIndividuals(arole(hasChild)), [i1-i2, i1-i3, i1-i4, i1-i5, i1-i6, i1-i7, i1-i8, i1-i9, i1-i10, i1-i11, 
				i2-i3, i3-i4, i4-i5, i5-i6, i6-i7, i7-i8, i8-i9, i9-i10, i10-i11, i11-i12]).

