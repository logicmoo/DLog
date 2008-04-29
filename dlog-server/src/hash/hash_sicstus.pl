foreign(new_loop, new_loop(+term,+term,+term)).
foreign(check_loop, check_loop(+term,+term,[-integer])).
foreign_resource(hash_sicstus, [new_loop,check_loop]).
