% options([abox_target(allinonefile), tbox_target(allinonefile)]).
options([abox_target(allinonefile), tbox_target(allinonefile), projection(no), ground_optim(no)]).
options([abox_target(allinonefile), tbox_target(allinonefile), projection(no), ground_optim(no), unfold(yes)]).

concept(a).
concept(d).


role(atmp).
role(b).
role(c).

subrole(arole(atmp),arole(b)).
subrole(arole(atmp),arole(c)).


implies(some(arole(atmp),top),aconcept(a)).
implies(aconcept(a),aconcept(d)).

rassertion(arole(b),i1,i2).
rassertion(arole(c),i1,i2).
cassertion(aconcept(a),i5).

%query(instances(aconcept(a)), [i1]).
