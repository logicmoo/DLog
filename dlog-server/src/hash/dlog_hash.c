#include "Backtrackable_HashSet.h"

#ifdef __SWI_PROLOG__
	#include <pthread.h>
	#include <SWI-Prolog.h>
	#define MULTI_THREADED  
#else
	#include <sicstus/sicstus.h>
	#include "sicstus_compatibility.h"
#endif
#include <stdlib.h>


#ifndef MODULE_NAME
	///Name of the Prolog module.
	#define MODULE_NAME "dlog_hash"
#endif
//prolog-like calls
#define prolog(A) if(!(A)) PL_fail; 

//TODO: _PL_get_arg(1, x2, loophash); //no checking
//TODO: hash anc list, optional hash

#ifdef MULTI_THREADED
	//TODO: instead of thread specific, use KB specific?  
	pthread_key_t hash_key;
	#define get_hashref() ((Backtrackable_HashSet*) pthread_getspecific(hash_key))
	#define set_hashref(hashref) pthread_setspecific(hash_key, (void*)hashref);
#else
	Backtrackable_HashSet* _hashref = NULL;
	#define get_hashref() _hashref
	#define set_hashref(hashref) (_hashref = (hashref))
#endif

//TODO: selectable size -> KB specific hash
foreign_t init_hash(term_t state) {
	Backtrackable_HashSet* hashref = get_hashref();

	if(!hashref){
		hashref = (Backtrackable_HashSet*) malloc(sizeof(Backtrackable_HashSet));
								// default size
		if(!hashref || init_Backtrackable_HashSet(hashref, 0)){
			term_t except = PL_new_term_ref();
			PL_put_atom_chars(except, "error_creating_hash");
			return PL_raise_exception(except); //TODO: informative exception
		}

		set_hashref(hashref);
		
		if(PL_unify_integer(state, 0)) PL_succeed;
		else PL_fail;
	}else{	
		if(PL_unify_integer(state, clear_Backtrackable_HashSet(hashref))) PL_succeed;
		else PL_fail;
	}
}

foreign_t new_loop(term_t goal, term_t hashes0, term_t hashes2) {
	int arity;
	term_t loophash0 = PL_new_term_ref();
	term_t anclist = PL_new_term_ref();
	term_t arg = PL_new_term_ref();
	term_t hashes1 = PL_new_term_ref();
	term_t loophash1 = PL_new_term_ref();
	functor_t hash_func;
	long state0, state1;
	atom_t func_atom, arg_atom;
	Backtrackable_HashSet *hashref = get_hashref();
			
	prolog(
			PL_get_functor(hashes0, &hash_func) && //LoopHash-AL
			PL_get_arg(1, hashes0, loophash0) && 
			PL_get_long(loophash0, &state0) &&
			PL_get_arg(2, hashes0, anclist) &&
						
			PL_get_name_arity(goal, &func_atom, &arity) &&
			PL_get_arg(1, goal, arg) &&
			PL_get_atom(arg, &arg_atom)
	);
	
	state1 = put_to_Backtrackable_HashSet(hashref, state0, func_atom, arg_atom);
	if(state1 <= 0){
		//TODO: informative exception
		term_t except = PL_new_term_ref();
		PL_put_atom_chars(except, "put_to_Backtrackable_HashSet_error");
		return PL_raise_exception(except);
	}

	PL_put_integer(loophash1, state1);
	PL_cons_functor(hashes1, hash_func, loophash1, anclist);
	
	if(PL_unify(hashes1, hashes2)) PL_succeed;
	else PL_fail;
}

foreign_t check_loop(term_t goal, term_t hashes) {
	int arity;
	term_t loophash = PL_new_term_ref();
	term_t anclist = PL_new_term_ref();
	term_t arg = PL_new_term_ref();
	functor_t hash_func;
	long state;
	atom_t func_atom, arg_atom;
	Backtrackable_HashSet *hashref = get_hashref();
	int c;
			
	prolog(
			PL_get_functor(hashes, &hash_func) && //LoopHash-AL
			PL_get_arg(1, hashes, loophash) && 
			PL_get_long(loophash, &state) &&
			PL_get_arg(2, hashes, anclist) &&
			
			PL_get_name_arity(goal, &func_atom, &arity) &&
			PL_get_arg(1, goal, arg) &&
			PL_get_atom(arg, &arg_atom)
	);
	
	c = check_Backtrackable_HashSet(hashref, state, func_atom, arg_atom);
	if(c == 1) PL_succeed;
	else if(c == 0){
		PL_fail;
	}else{
		//error
		term_t except = PL_new_term_ref();
		term_t argt = PL_new_term_ref();
		atom_t name = PL_new_atom("check_loop_error");
		{	//block needed for SICStus compatibility
			functor_t f = PL_new_functor(name, 1);
			PL_put_functor(except, f);
		}
		PL_unregister_atom(name);
		PL_get_arg(1, except, argt);
		PL_unify_integer(argt, c);
		return PL_raise_exception(except);
	}
}

#ifdef MULTI_THREADED
void cleanup_hash(void *h){
	if(h != NULL) free(h);
}
#endif


#ifdef MULTI_THREADED
install_t install() 
{
#ifdef DEBUG_DLOG_HASH
	hash_log = fopen(HASH_LOG_NAME, "w");
#endif
	if(pthread_key_create(&hash_key, cleanup_hash)){
		//error
		term_t except = PL_new_term_ref();
		PL_put_atom_chars(except, "error_loading_hash");
		 
		//PL_raise_exception(except);
		PL_throw(except);
		return;
	};

	
	PL_register_foreign_in_module(MODULE_NAME, "init_hash", 1, (void*)init_hash, 0);
	PL_register_foreign_in_module(MODULE_NAME, "new_loop", 3, (void*)new_loop, 0);
	PL_register_foreign_in_module(MODULE_NAME, "check_loop", 2, (void*)check_loop, 0);
}

#else //MULTI_THREADED

//SICStus
install_t install(int when){
#ifdef DEBUG_DLOG_HASH
	hash_log = fopen(HASH_LOG_NAME, "w");
#endif	
}

#endif //MULTI_THREADED

