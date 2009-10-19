% gtrace, test(Q).
:- use_module('../presql_struct2sql.pl').
:- use_module('../zsl_util.pl').

t(1,join(1,[X],[mem_abox(2,[X],d,[i1])])).

t(2,join(1,[X],[mem_abox(2,[X],d,[i2,i3])])).

t(3,union(1,[X],[mem_abox(2,[X],d,[i2,i3]),mem_abox(3,[X],e,[i4])])).

t(4,join(1,[X],[mem_abox(2,[X],a,[1,2,4]),mem_abox(3,[X],b,[2,3])])).

t(5,join(1,[X],
   [
      mem_abox(2,[X],a,[1,2,4]),mem_abox(3,[X,Y],b,[2-5,3-4,4-5]),
      mem_abox(4,[Y],c,[4,5,6])
   ]
   )).
   
t(6,join(1,[X],[
      mem_abox(2,[X],a,[1,2,4]),mem_abox(3,[X,Y],b,[2-5,3-4,4-5]),
      mem_abox(4,[Y],c,[4,5,6]),
      mem_abox(5,[X,Y],b,[2-5])
   ])).

t(7,union(5,[X],
   [
   union(3,[X],[mem_abox(2,[X],d,[i2,i3]),mem_abox(4,[X],e,[i4])]),
   mem_abox(2,[X],d,[i5,i6])
   ]
)).

t(8,union(5,[X],
   [
   union(3,[X],[mem_abox(2,[X],d,[i2,i3])])
   ]
)).

t(9,union(5,[X],
   [
   mem_abox(2,[X],d,[i2,i3])
   ]
)).

t(10,
   
   mem_abox(2,[X],d,[i2,i3,i4,i5])
   
).

t(11,
   
   mem_abox(2,[X,Y],d,[i2-q2,i3-q3,i4-q4,i5-q5])
   
).

t(12,
   union(1,[X,Y],[
      mem_abox(2,[X,Y],d,[i2-q2,i3-q3]),
      mem_abox(3,[X,Y],e,[i4-q4,i5-q5])
   ])   
).

test(X):-
   t(X,PreSqlStruct),
   struct2query(top_level,PreSqlStruct,SQLQ,1),
   writec('\n'+SQLQ+'\n\n').
   
   
/*

SELECT t_2.subject  FROM  
	(SELECT * FROM ((SELECT 1 AS subject) UNION (SELECT 2 AS subject) UNION (SELECT 4 AS subject)) 
		as t_2) as t_2,
	(SELECT * FROM ((SELECT 2 AS subject2) UNION (SELECT 3 AS subject2)) 
		as t_3) as t_3,  
		
	WHERE  t_2.subject = t_3.subject2 
   
*/   
