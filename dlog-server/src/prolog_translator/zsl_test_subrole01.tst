% options([abox_target(allinonefile), tbox_target(allinonefile)]).
options([abox_target(allinonefile), tbox_target(allinonefile), projection(no), ground_optim(no)]).
options([abox_target(allinonefile), tbox_target(allinonefile), projection(no), ground_optim(no), unfold(yes)]).

concept(c).



role(a).
role(b).


subrole(arole(b),arole(a)).


implies(some(arole(a),top),aconcept(c)).


rassertion(arole(b),i1,i2).
rassertion(arole(a),d1,d2).
cassertion(aconcept(c),d5).

% expected result: aconcept(c): i1
query(instances(aconcept(c)), _).
