#include <pthread.h>
#include <SWI-Prolog.h>
#include "Backtrackable_HashSet.h"

//#include <fstream>
//using namespace std;
//ofstream log("log.txt");

//#ifdef __SWI_PROLOG__
//#endif

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
//		log<<"("<<func_atom<<", "<<arg_atom<<")==("<<e->func_atom<<", "<<e->arg_atom<<"): "<<r<<endl<<flush; 
		return r;
	}
};

//TODO: _PL_get_arg(1, x2, loophash); //no checking


pthread_key_t hash_key;

extern "C" foreign_t init_hash(term_t hash) {
	Backtrackable_HashSet* hashref = (Backtrackable_HashSet*) pthread_getspecific(hash_key);
	if(!hashref){
		hashref = new Backtrackable_HashSet();
		
		/*}catch{
			term_t except = PL_new_term_ref();
			PL_unify_atom_chars(except, "error_creating_hash");
			return PL_raise_exception(except);
		}*/
		
		pthread_setspecific(hash_key, (void*)hashref);
	}
	
	return PL_unify_pointer(hash, hashref->clear());
}

extern "C" foreign_t new_loop(term_t goal, term_t hashes0, term_t hashes1) {
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
	
	Backtrackable_HashSet* hashref = (Backtrackable_HashSet*) pthread_getspecific(hash_key);
	
	void *state1;
	try{
		state1 = hashref->put(state0, *e);
	}catch(char *ex){
		term_t except = PL_new_term_ref();
		PL_unify_atom_chars(except, ex);
		return PL_raise_exception(except);
	}
	term_t loophash1 = PL_new_term_ref();

	
	PL_put_pointer(loophash1, state1);
	prolog(			
			PL_unify_functor(hashes1, hash_func) &&
			PL_unify_arg(1, hashes1, loophash1) &&
			PL_unify_arg(2, hashes1, anclist)
	);
	
	PL_succeed;
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
	
	Backtrackable_HashSet* hashref = (Backtrackable_HashSet*) pthread_getspecific(hash_key);
	
	try{
		return hashref->check(state, e);
	}catch(char *ex){
		term_t except = PL_new_term_ref();
		PL_unify_atom_chars(except, ex);
		return PL_raise_exception(except);
	}
}


void cleanup_hash(void *h){
	if(h != NULL) free(h);
	//return NULL;
}


extern "C" install_t install() 
{
	if(pthread_key_create(&hash_key, cleanup_hash)){
		//error
		term_t except = PL_new_term_ref();
		PL_unify_atom_chars(except, "error_loading_hash");
		 
		PL_raise_exception(except);
		return;
	};
	
	PL_register_foreign_in_module("anc_loop", "init_hash", 1, (void*)init_hash, 0);
	PL_register_foreign_in_module("anc_loop", "new_loop", 3, (void*)new_loop, 0);
	PL_register_foreign_in_module("anc_loop", "check_loop", 2, (void*)check_loop, 0);
}


