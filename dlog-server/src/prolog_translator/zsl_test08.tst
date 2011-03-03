% execute_test_files(['../prolog_translator/zsl_test08.tst'],text).
% options([abox_target(allinonefile), tbox_target(allinonefile)]).
options([abox_target(allinonefile), tbox_target(allinonefile), projection(no), ground_optim(no)]).
options([abox_target(allinonefile), tbox_target(allinonefile), projection(no), ground_optim(no), unfold(yes)]).

role(a).
role(e).

concept(b).
concept(c).
concept(d).
/*
dbConnection(dsn1, table1). 
% IIRC table1 is defined in Windows Control Panel -> Administrative tools ODBC data sources


%dbAccess(c/1, dsn1, query('SELECT subject from items where c')).
%dbAccess(not(c)/1, dsn1, query('SELECT subject from items where not c')).
dbAccess(b/1, dsn1, query('SELECT subject from items where b')).
dbAccess(not(b)/1, dsn1, query('SELECT subject from items where not b')).
dbAccess(a/2, dsn1, query('SELECT subject, object from relationship_a')).
dbAccess(d/1, dsn1, query('SELECT subject from items where d')).
dbAccess(not(d)/1, dsn1, query('SELECT subject from items where not d')).

dbAccess(e/2, dsn1, query('SELECT subject,object from relationship_e')).

*/

cassertion(aconcept(b),i5).
rassertion(arole(a),i4,i5).

cassertion(aconcept(c),i6).
cassertion(aconcept(d),i7).
rassertion(arole(e),i8,i7).
rassertion(arole(a),i9,i8).


implies(and([some(arole(a),aconcept(b))]),aconcept(c)).
implies(and([some(arole(e),aconcept(d))]),aconcept(b)).
query(instances(aconcept(c)), [i4,i9]).
query(instances(aconcept(b)), [i5,i8]).


% ./tbox_translator -> store predicate mit csinal?
% tbox2prolog végére beszúrni a saját részt: qpredet tboxból kivenni, ABoxba betenni a megfelelõ sql lekérést (dbaccess)
% core/kbmanager.pl ahol meghívódik a tbox2prolog (add_tbox, add_abox)
% lásd még add_tbox

