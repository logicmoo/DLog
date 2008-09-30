%Generated from '../examples/alcoholic/ca20.dig'.


query(instances(aconcept('Alcoholic')), []).
query(instances(not(aconcept('Alcoholic'))), [i0, i1, i10, i11, i12, i13, i14, i15, i16, i17, i18, i19, i2, i3, i4, i5, i6, i7, i8, i9]).

concept('Alcoholic').

role(hasFriend).
role(hasParent).

implies(some(arole(hasParent), not(aconcept('Alcoholic'))), not(aconcept('Alcoholic'))).
implies(some(arole(hasFriend), aconcept('Alcoholic')), not(aconcept('Alcoholic'))).



rassertion(arole(hasParent), i0, i1).
rassertion(arole(hasParent), i1, i2).
rassertion(arole(hasParent), i2, i3).
rassertion(arole(hasParent), i3, i4).
rassertion(arole(hasParent), i4, i5).
rassertion(arole(hasParent), i5, i6).
rassertion(arole(hasParent), i6, i7).
rassertion(arole(hasParent), i7, i8).
rassertion(arole(hasParent), i8, i9).
rassertion(arole(hasParent), i9, i10).
rassertion(arole(hasParent), i10, i11).
rassertion(arole(hasParent), i11, i12).
rassertion(arole(hasParent), i12, i13).
rassertion(arole(hasParent), i13, i14).
rassertion(arole(hasParent), i14, i15).
rassertion(arole(hasParent), i15, i16).
rassertion(arole(hasParent), i16, i17).
rassertion(arole(hasParent), i17, i18).
rassertion(arole(hasParent), i18, i19).
rassertion(arole(hasParent), i19, i20).
rassertion(arole(hasFriend), i20, i19).



