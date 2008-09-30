%Generated from '../examples/iocaste/n20.dig'.


query(instances(aconcept('Good')), [i1]).

concept(note).
concept(notd).
concept(notc).
concept(notb).
concept(nota).
concept('Good').
concept(e).
concept(d).
concept(c).
concept(b).
concept(a).

role(role_j).
role(role_i).
role(role_h).
role(role_g).
role(role_f).
role(role_e).
role(role_d).
role(role_c).
role(role_b).
role(role_a).

implies(and([not(aconcept(e))]), aconcept(note)).
implies(aconcept(note), and([not(aconcept(e))])).
implies(and([not(aconcept(d))]), aconcept(notd)).
implies(aconcept(notd), and([not(aconcept(d))])).
implies(and([not(aconcept(c))]), aconcept(notc)).
implies(aconcept(notc), and([not(aconcept(c))])).
implies(and([not(aconcept(b))]), aconcept(notb)).
implies(aconcept(notb), and([not(aconcept(b))])).
implies(and([not(aconcept(a))]), aconcept(nota)).
implies(aconcept(nota), and([not(aconcept(a))])).
implies(and([some(arole(role_a), and([aconcept(a), some(arole(role_a), aconcept(nota))]))]), aconcept('Good')).
implies(aconcept('Good'), and([some(arole(role_a), and([aconcept(a), some(arole(role_a), aconcept(nota))]))])).



rassertion(arole(role_e), i13, i20).
rassertion(arole(role_c), i13, i13).
rassertion(arole(role_b), i13, i1).
rassertion(arole(role_a), i13, i19).
cassertion(aconcept(e), i13).
cassertion(top, i13).
rassertion(arole(role_d), i3, i9).
rassertion(arole(role_a), i3, i4).
cassertion(top, i3).
rassertion(arole(role_b), i2, i10).
rassertion(arole(role_a), i2, i3).
cassertion(aconcept(a), i2).
cassertion(top, i2).
rassertion(arole(role_e), i6, i19).
rassertion(arole(role_e), i6, i17).
rassertion(arole(role_b), i6, i9).
cassertion(aconcept(notd), i6).
cassertion(top, i6).
rassertion(arole(role_e), i12, i3).
rassertion(arole(role_e), i12, i6).
cassertion(top, i12).
rassertion(arole(role_c), i11, i9).
rassertion(arole(role_b), i11, i1).
cassertion(aconcept(note), i11).
cassertion(top, i11).
cassertion(aconcept(b), i8).
rassertion(arole(role_d), i17, i2).
rassertion(arole(role_d), i17, i8).
cassertion(aconcept(d), i17).
cassertion(top, i17).
rassertion(arole(role_d), i1, i13).
rassertion(arole(role_b), i1, i4).
rassertion(arole(role_a), i1, i3).
rassertion(arole(role_a), i1, i2).
cassertion(aconcept(notd), i1).
cassertion(top, i1).
rassertion(arole(role_c), i19, i2).
cassertion(top, i19).
rassertion(arole(role_b), i16, i16).
cassertion(top, i16).
rassertion(arole(role_d), i5, i12).
rassertion(arole(role_d), i5, i11).
rassertion(arole(role_c), i5, i1).
rassertion(arole(role_b), i5, i6).
cassertion(top, i5).
rassertion(arole(role_d), i7, i16).
rassertion(arole(role_d), i7, i3).
rassertion(arole(role_c), i7, i1).
rassertion(arole(role_c), i7, i7).
cassertion(aconcept(notb), i7).
cassertion(top, i7).
rassertion(arole(role_e), i4, i20).
rassertion(arole(role_e), i4, i10).
cassertion(aconcept(nota), i4).
cassertion(top, i4).
rassertion(arole(role_d), i10, i20).
rassertion(arole(role_d), i10, i3).
rassertion(arole(role_b), i10, i15).
cassertion(top, i10).
rassertion(arole(role_c), i20, i19).
rassertion(arole(role_b), i20, i15).
cassertion(top, i20).
rassertion(arole(role_e), i15, i3).
rassertion(arole(role_d), i15, i10).
rassertion(arole(role_c), i15, i19).
rassertion(arole(role_c), i15, i7).
cassertion(aconcept(e), i15).
cassertion(top, i15).
rassertion(arole(role_e), i9, i17).
rassertion(arole(role_c), i9, i10).
rassertion(arole(role_a), i9, i20).
rassertion(arole(role_a), i9, i12).
cassertion(aconcept(b), i9).
cassertion(top, i9).



