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
//#define prolog(A) if(!(A)) PL_fail; 
#define prolog(A) do{ if(!(A)) PL_fail; }while(0)

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

///Throw an informative exception to prolog.
foreign_t throw_prolog_exception(const char *name, const long code);
foreign_t init_hash_size0(const long size, term_t state);

foreign_t init_hash(term_t state) {
	return init_hash_size0(0l, state); // default size
}

foreign_t init_hash_size(term_t size, term_t state) {
	long s;
	prolog(
		PL_get_long(size, &s)
	);
	return init_hash_size0(s, state);
}

foreign_t init_hash_size0(const long size, term_t state) {
	long e;
	Backtrackable_HashSet* hashref = get_hashref();	
	
	if(!hashref){
		hashref = (Backtrackable_HashSet*) malloc(sizeof(Backtrackable_HashSet));
		if(!hashref) return throw_prolog_exception("init_hash_error", ALLOC_ERROR);
		e = init_Backtrackable_HashSet(hashref, size); 
		set_hashref(hashref);
	}else{
		e = clear_Backtrackable_HashSet(hashref, size);
	}	
	if(e) return throw_prolog_exception("init_hash_error", e);
	if(PL_unify_integer(state, 0)) PL_succeed; 
	else PL_fail;
}


foreign_t put_to_hash(term_t goal, term_t hashes0, term_t hashes2) {
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
	if(state1 <= 0)	return throw_prolog_exception("new_loop_error", state1);

	PL_put_integer(loophash1, state1);
	PL_cons_functor(hashes1, hash_func, loophash1, anclist);
	
	if(PL_unify(hashes1, hashes2)) PL_succeed;
	else PL_fail;
}

foreign_t check_hash(term_t goal, term_t hashes) {
	int arity;
	term_t loophash = PL_new_term_ref();
	term_t anclist = PL_new_term_ref();
	term_t arg = PL_new_term_ref();
	functor_t hash_func;
	long state;
	atom_t func_atom, arg_atom;
	Backtrackable_HashSet *hashref = get_hashref();
	long c;
			
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
		return throw_prolog_exception("check_loop_error", c);
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
		 
		//PL_raise_exception(except); //sets the exception to be thrown on failure by prolog
		PL_throw(except); //longjmp
		return; //not really neaded :)
	};

	
	PL_register_foreign_in_module(MODULE_NAME, "init_hash", 1, (void*)init_hash, 0);
	PL_register_foreign_in_module(MODULE_NAME, "init_hash", 2, (void*)init_hash_size, 0);
	PL_register_foreign_in_module(MODULE_NAME, "put_to_hash", 3, (void*)put_to_hash, 0);
	PL_register_foreign_in_module(MODULE_NAME, "check_hash", 2, (void*)check_hash, 0);
}

#else //MULTI_THREADED

//SICStus
install_t install(int when){
#ifdef DEBUG_DLOG_HASH
	hash_log = fopen(HASH_LOG_NAME, "w");
#endif	
}

#endif //MULTI_THREADED


/**
 * Throw an informative exception to prolog in the form of name(message) where 
 * message is an atom representing an error defined in Backtrackable_HashSet.h 
 * or an integer
 * @param name the functor name of the exception.
 * @param code the error code.
 * @return PL_raise_exception(except) (SP_error in SICStus or false in SWI)
 */
foreign_t throw_prolog_exception(const char *name, const long code){
	term_t except = PL_new_term_ref();
	term_t argt = PL_new_term_ref();
	atom_t aname = PL_new_atom(name); //increases atom ref counter
	functor_t f = PL_new_functor(aname, 1);
	
	PL_put_functor(except, f);
		
	//already have an other reference in except, let GC later destroy it
	PL_unregister_atom(aname); 
	PL_get_arg(1, except, argt);
	switch(code){
	case ALLOC_ERROR:
		PL_unify_atom_chars(argt, "alloc_error");
		break;
	case INVALID_ARGUMENT:
		PL_unify_atom_chars(argt, "invalid_argument");
		break;
	case OVERFLOW:
		PL_unify_atom_chars(argt, "overflow");
		break;
	case ALREADY_ADDED:
		PL_unify_atom_chars(argt, "already_added");
		break;	
	default: PL_unify_integer(argt, code);
	}
	return PL_raise_exception(except);
}
