% options([abox_target(allinonefile), tbox_target(allinonefile)]).
options([abox_target(allinonefile), tbox_target(allinonefile), projection(no), ground_optim(no)]).
options([abox_target(allinonefile), tbox_target(allinonefile), projection(no), ground_optim(no), unfold(yes)]).

% diszjunkcio fogalmakra

role(a).


concept(b).
concept(c).

concept(f).

dbConnection(dsn1, table1).
%dbAccess(c/1, dsn1, query('SELECT subject from items where c')).
%dbAccess(not(c)/1, dsn1, query('SELECT subject from items where not c')).
dbAccess(b/1, dsn1, query('SELECT subject from items where b')).
dbAccess(not(b)/1, dsn1, query('SELECT subject from items where not b')).
dbAccess(a/2, dsn1, query('SELECT subject, object from relationship_a')).


%cassertion(aconcept(b),i5).
%rassertion(arole(a),i4,i5).
cassertion(aconcept(c),i6).
%cassertion(aconcept(d),i7).
%rassertion(arole(e),i8,i7).
cassertion(aconcept(f),d9).

%cassertion(not(aconcept(c)),i7).


implies(and([some(arole(a),aconcept(b))]),aconcept(c)).
implies(aconcept(f),aconcept(c)).

