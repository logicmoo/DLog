#include "Backtrackable_HashSet.h"

#ifdef __SWI_PROLOG__
	#include <pthread.h>
	#include <SWI-Prolog.h>
	#define MULTI_THREADED  
#else
	#include <sicstus/sicstus.h>
	#include "sicstus_compatibility.h"
#endif

#define MODULE_NAME "dlog_hash"
//prolog-like calls
#define prolog(A) if(!(A)) PL_fail; 

class HashElem: public Backtrackable_HashSet::Hashable{
public:
	atom_t func_atom, arg_atom;
	virtual inline long hashval() const{
		return (func_atom ^ (arg_atom << 16)) % 49997; 
	}
		
	virtual inline bool operator==(const Backtrackable_HashSet::Hashable &h) const{
		const HashElem *e = dynamic_cast<const HashElem *>(&h);
		bool r = e && func_atom == e->func_atom && arg_atom == e->arg_atom; 
		return r;
	}
};

//TODO: _PL_get_arg(1, x2, loophash); //no checking

#ifdef MULTI_THREADED
	pthread_key_t hash_key;
	#define get_hashref() ((Backtrackable_HashSet*) pthread_getspecific(hash_key))
	#define set_hashref(hashref) pthread_setspecific(hash_key, (void*)hashref);
#else
	Backtrackable_HashSet* _hashref = NULL;
	#define get_hashref() _hashref
	#define set_hashref(hashref) (_hashref = (hashref))
#endif

extern "C" foreign_t init_hash(term_t hash) {
	Backtrackable_HashSet* hashref = get_hashref();

	if(!hashref){
		hashref = new Backtrackable_HashSet();
		
		/*}catch{
			term_t except = PL_new_term_ref();
			PL_put_atom_chars(except, "error_creating_hash");
			return PL_raise_exception(except);
		}*/
		
		set_hashref(hashref);
	}
	
	if(PL_unify_pointer(hash, hashref->clear())) PL_succeed;
	else PL_fail;
}

extern "C" foreign_t new_loop(term_t goal, term_t hashes0, term_t hashes2) {
	HashElem *e = new HashElem(); 	
	int arity;
	term_t loophash0 = PL_new_term_ref();
	term_t anclist = PL_new_term_ref();
	term_t arg = PL_new_term_ref();
	functor_t hash_func;
	void *state0;
			
	prolog(
			PL_get_functor(hashes0, &hash_func) && //LoopHash-AL
			PL_get_arg(1, hashes0, loophash0) && 
			PL_get_pointer(loophash0, &state0) &&
			PL_get_arg(2, hashes0, anclist) &&
						
			PL_get_name_arity(goal, &e->func_atom, &arity) &&
			PL_get_arg(1, goal, arg) &&
			PL_get_atom(arg, &e->arg_atom)
	);
	
	Backtrackable_HashSet* hashref = get_hashref();
	
	void *state1;
	try{
		state1 = hashref->put(state0, *e);
	}catch(char *ex){
		term_t except = PL_new_term_ref();
		PL_put_atom_chars(except, ex);
		return PL_raise_exception(except);
	}
	
	
	term_t hashes1 = PL_new_term_ref();
	term_t loophash1 = PL_new_term_ref();
	PL_put_pointer(loophash1, state1);
	PL_cons_functor(hashes1, hash_func, loophash1, anclist);
	
	if(PL_unify(hashes1, hashes2)) PL_succeed;
	else PL_fail;
}

extern "C" foreign_t check_loop(term_t goal, term_t hashes) {
	HashElem e;
	int arity;
	term_t loophash = PL_new_term_ref();
	term_t anclist = PL_new_term_ref();
	term_t arg = PL_new_term_ref();
	functor_t hash_func;
	void *state;
			
	prolog(
			PL_get_functor(hashes, &hash_func) && //LoopHash-AL
			PL_get_arg(1, hashes, loophash) && 
			PL_get_pointer(loophash, &state) &&
			PL_get_arg(2, hashes, anclist) &&
			
			PL_get_name_arity(goal, &e.func_atom, &arity) &&
			PL_get_arg(1, goal, arg) &&
			PL_get_atom(arg, &e.arg_atom)
	);
	
	Backtrackable_HashSet* hashref = get_hashref();
	
	try{
		if(hashref->check(state, e)) PL_succeed;
		else PL_fail;
	}catch(char *ex){
		term_t except = PL_new_term_ref();
		PL_put_atom_chars(except, ex);
		return PL_raise_exception(except);
	}
}

#ifdef MULTI_THREADED
void cleanup_hash(void *h){
	if(h != NULL) free(h);
}
#endif


#ifdef MULTI_THREADED
extern "C" install_t install() 
{
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
#endif

