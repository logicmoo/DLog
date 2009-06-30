foreign(init_hash, init_hash(+term)).
foreign(init_hash_size, init_hash(+term, +term)).
foreign(put_to_hash, put_to_hash(+term, +term, +term)).
foreign(check_hash, check_hash(+term, +term)).
foreign_resource(hash_sicstus, [init(install), init_hash, init_hash_size, put_to_hash, check_hash]).
