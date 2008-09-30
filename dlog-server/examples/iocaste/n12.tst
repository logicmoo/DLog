%Generated from '../examples/iocaste/n12.dig'.


query(instances(aconcept('Good')), [i1]).

concept('Good').
concept('Patricide').

role(hasChild).

implies(and([some(arole(hasChild), and([aconcept('Patricide'), some(arole(hasChild), not(aconcept('Patricide')))]))]), aconcept('Good')).
implies(aconcept('Good'), and([some(arole(hasChild), and([aconcept('Patricide'), some(arole(hasChild), not(aconcept('Patricide')))]))])).



cassertion(aconcept('Patricide'), i36).
cassertion(not(aconcept(b)), i35).
cassertion(not(aconcept(d)), i34).
cassertion(not(aconcept(d)), i33).
cassertion(not(aconcept(c)), i32).
cassertion(aconcept(c), i31).
cassertion(aconcept(d), i29).
cassertion(not(aconcept(c)), i27).
cassertion(not(aconcept(d)), i24).
cassertion(aconcept(d), i23).
cassertion(aconcept(d), i22).
cassertion(not(aconcept(b)), i21).
cassertion(aconcept(d), i20).
cassertion(not(aconcept(e)), i19).
cassertion(not(aconcept(c)), i17).
cassertion(aconcept(b), i15).
cassertion(aconcept('Patricide'), i14).
cassertion(not(aconcept('Patricide')), i13).
cassertion(not(aconcept('Patricide')), i12).
cassertion(not(aconcept(c)), i11).
cassertion(not(aconcept(d)), i8).
cassertion(aconcept(e), i4).
cassertion(aconcept('Patricide'), i2).
rassertion(arole(role_b), i36, i25).
rassertion(arole(role_d), i36, i13).
rassertion(arole(hasChild), i34, i15).
rassertion(arole(role_d), i33, i2).
rassertion(arole(role_b), i33, i30).
rassertion(arole(hasChild), i33, i15).
rassertion(arole(role_b), i31, i18).
rassertion(arole(role_e), i31, i22).
rassertion(arole(role_d), i30, i17).
rassertion(arole(role_e), i30, i8).
rassertion(arole(role_b), i30, i24).
rassertion(arole(role_e), i29, i23).
rassertion(arole(role_d), i28, i20).
rassertion(arole(hasChild), i28, i36).
rassertion(arole(role_d), i27, i30).
rassertion(arole(role_b), i26, i31).
rassertion(arole(role_b), i26, i28).
rassertion(arole(role_d), i26, i23).
rassertion(arole(hasChild), i25, i26).
rassertion(arole(role_e), i25, i12).
rassertion(arole(role_c), i23, i20).
rassertion(arole(hasChild), i23, i36).
rassertion(arole(role_d), i23, i26).
rassertion(arole(role_e), i22, i15).
rassertion(arole(hasChild), i21, i15).
rassertion(arole(role_c), i21, i23).
rassertion(arole(role_d), i21, i16).
rassertion(arole(role_b), i20, i3).
rassertion(arole(role_e), i19, i13).
rassertion(arole(role_e), i19, i33).
rassertion(arole(role_b), i17, i4).
rassertion(arole(role_d), i16, i21).
rassertion(arole(role_d), i16, i14).
rassertion(arole(role_c), i16, i12).
rassertion(arole(role_d), i15, i8).
rassertion(arole(role_c), i14, i21).
rassertion(arole(role_c), i14, i33).
rassertion(arole(role_c), i14, i25).
rassertion(arole(hasChild), i11, i12).
rassertion(arole(role_b), i11, i31).
rassertion(arole(hasChild), i10, i11).
rassertion(arole(role_c), i10, i5).
rassertion(arole(role_c), i10, i8).
rassertion(arole(hasChild), i9, i10).
rassertion(arole(role_e), i9, i34).
rassertion(arole(hasChild), i8, i29).
rassertion(arole(hasChild), i8, i9).
rassertion(arole(hasChild), i7, i8).
rassertion(arole(role_b), i7, i30).
rassertion(arole(hasChild), i6, i7).
rassertion(arole(hasChild), i6, i34).
rassertion(arole(role_e), i6, i32).
rassertion(arole(role_e), i6, i22).
rassertion(arole(hasChild), i5, i6).
rassertion(arole(role_e), i5, i14).
rassertion(arole(role_b), i5, i35).
rassertion(arole(hasChild), i4, i5).
rassertion(arole(hasChild), i3, i4).
rassertion(arole(role_d), i3, i28).
rassertion(arole(role_c), i3, i3).
rassertion(arole(hasChild), i2, i3).
rassertion(arole(hasChild), i2, i32).
rassertion(arole(hasChild), i2, i31).
rassertion(arole(role_d), i2, i18).
rassertion(arole(role_b), i1, i17).
rassertion(arole(role_b), i1, i29).
rassertion(arole(role_e), i1, i25).
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



