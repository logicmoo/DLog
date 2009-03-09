foreign(init_hash, init_hash(+term)).
foreign(new_loop, new_loop(+term, +term, +term)).
foreign(check_loop, check_loop(+term, +term)).
foreign_resource(hash_sicstus, [init(install), init_hash, new_loop, check_loop]).
