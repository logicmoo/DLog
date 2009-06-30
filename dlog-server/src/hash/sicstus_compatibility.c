#include "sicstus_compatibility.h"

SP_term_ref _term;
//atom_t _new_atom;


/*
int PL_unify_integer(term_t t, long l){
	SP_term_ref t2 = SP_new_term_ref();
	SP_put_integer(t2, l);
	return SP_unify(t, t2);
}

int PL_unify_pointer(term_t t, void *ptr){
	SP_term_ref t2 = SP_new_term_ref();
	SP_put_address(t2, ptr);
	return SP_unify(t, t2);
}
*/
