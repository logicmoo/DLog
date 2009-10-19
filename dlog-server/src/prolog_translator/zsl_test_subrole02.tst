% options([abox_target(allinonefile), tbox_target(allinonefile)]).
options([abox_target(allinonefile), tbox_target(allinonefile), projection(no), ground_optim(no)]).
options([abox_target(allinonefile), tbox_target(allinonefile), projection(no), ground_optim(no), unfold(yes)]).

concept(d).



role(a).
role(b).
role(c).

subrole(arole(b),arole(a)).
subrole(arole(c),arole(a)).

implies(some(arole(a),top),aconcept(d)).


rassertion(arole(b),i1,i2).
rassertion(arole(a),d1,d2).
cassertion(aconcept(d),d5).

% expected result: aconcept(c): i1
%query(instances(aconcept(c)), _).
