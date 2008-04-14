:- module(hash_swi, [term_hash/4]).

term_hash(G, -1, M, H) :- 
				hash_term(G, H1), 
				(nonvar(H1) -> H is H1 mod M ; true).