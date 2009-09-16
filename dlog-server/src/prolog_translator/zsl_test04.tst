% options([abox_target(allinonefile), tbox_target(allinonefile)]).
options([abox_target(allinonefile), tbox_target(allinonefile), projection(no), ground_optim(no)]).
options([abox_target(allinonefile), tbox_target(allinonefile), projection(no), ground_optim(no), unfold(yes)]).

role(a).

concept(b).
concept(c).

dbConnection(dsn1, table1).
dbAccess(not(c)/1, dsn1, query('SELECT id from items where c')).

cassertion(aconcept(b),i4).
rassertion(arole(a),i4,i5).
cassertion(aconcept(c),i6).
%cassertion(not(aconcept(c)),i7).


implies(and([some(arole(a),aconcept(b))]),aconcept(c)).

