#include <SWI-Prolog.h>

#ifdef __cplusplus
extern "C" {
#endif


//#ifdef __SWI_PROLOG__
//#endif

#define prolog(A) if(!(A)) PL_fail; //prolog-like calls
#define dlog_hash(func_atom, arg_atom) \
			(((func_atom) ^ ((arg_atom) << 16)) % 49997)

//TODO: _PL_get_arg(1, x2, loophash); //no checking


foreign_t lookup(term_t goal, term_t hashes, term_t elem);

foreign_t new_loop(term_t goal, term_t hashes, term_t hashes2) {
	term_t elem = PL_new_term_ref(), 
			list = PL_new_term_ref();
			
	prolog(
		PL_unify(hashes, hashes2) &&	
		lookup(goal, hashes, list)
	);
	
	while (! PL_is_variable(list))
	{
		prolog(PL_get_arg(2, list, list));
		//PL_get_tail(list, list);
	}

	PL_put_list(elem);
	prolog(
		PL_unify(list, elem)  &&
		PL_get_arg(1, elem, elem) &&
		//PL_get_head(elem, elem) &&
		PL_unify(elem, goal)
	);
	PL_succeed;
}

foreign_t check_loop(term_t goal, term_t hashes) {
	term_t elem = PL_new_term_ref(), 
			list = PL_new_term_ref();
	
	prolog(lookup(goal, hashes, list));
	while (! PL_is_variable(list))
	{
		prolog(PL_get_list(list, elem, list));
		//prolog(PL_get_arg(1, list, elem));
		if(PL_unify(elem, goal)) PL_succeed;
		//else PL_get_arg(2, list, list);
	}
	
	PL_fail;
}

foreign_t lookup(term_t goal, term_t hashes, term_t elem){
	term_t loophash = PL_new_term_ref(), 
			arg = PL_new_term_ref();
	atom_t func_atom, arg_atom;
	int arity, row, col;
	unsigned long hash;
	//functor_t functor;
	
	prolog(
		PL_get_arg(1, hashes, loophash) && //LoopHash-AL
		PL_get_name_arity(goal, &func_atom, &arity) &&
		//PL_get_functor(goal, &functor);
		PL_get_arg(1, goal, arg) &&
		PL_get_atom(arg, &arg_atom)
	);
	hash = dlog_hash(func_atom, arg_atom);
	row = hash / 250;
	col = hash % 250;
	prolog(
		PL_get_arg(row+1, loophash, elem) &&
		PL_get_arg(col+1, elem, elem)
	);
	
	PL_succeed;
}


install_t install() 
{ 
	PL_register_foreign_in_module("anc_loop", "new_loop", 3, new_loop, 0);
	PL_register_foreign_in_module("anc_loop", "check_loop", 2, check_loop, 0);
}

#ifdef __cplusplus
}
#endif

