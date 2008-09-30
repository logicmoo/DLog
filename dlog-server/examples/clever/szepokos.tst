%Generated from '../examples/clever/szepokos.dig'.

options([projection(no), abox_target(allinonefile), tbox_target(allinonefile)]).
options([projection(yes), abox_target(allinonefile), tbox_target(allinonefile)]).

query(instances(aconcept('Szep')), [betty]).
query(instances(aconcept('Okos')), [betty, susan]).



implies(aconcept('Szep'), all(arole(gyereke), all(arole(gyereke), aconcept('Okos')))).
implies(all(arole(gyereke), aconcept('Okos')), aconcept('Okos')).



rassertion(arole(gyereke), betty, susan).
cassertion(aconcept('Szep'), betty).



