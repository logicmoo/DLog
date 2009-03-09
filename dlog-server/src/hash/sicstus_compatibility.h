#ifndef SICSTUS_COMPATIBILITY_H_
#define SICSTUS_COMPATIBILITY_H_

#include <sicstus/sicstus.h>

#define install_t void
#define term_t SP_term_ref
#define atom_t SP_atom
typedef struct {SP_atom name; int arity;} functor_t; 
#define foreign_t int

//for PL_unify_*; not threads safe
extern SP_term_ref _term;

#define PL_succeed return SP_SUCCESS
//#define PL_fail return SP_FAILURE
#define PL_fail {SP_fail(); return SP_FAILURE;}
#define PL_new_atom(string) (SP_register_atom(SP_atom_from_string(string)), SP_atom_from_string(string))
#define PL_atom_chars(atom) SP_string_from_atom(atom)
#define PL_new_functor(name, arity) {(name), (arity)}
//atom_t PL_functor_name(functor_t f)
//int PL_functor_arity(functor_t f)
// -> int SP_atom_length(SP_atom a)
#define PL_register_atom(atom) SP_register_atom(atom)
#define PL_unregister_atom(atom) SP_unregister_atom(atom)
#define PL_new_term_ref() SP_new_term_ref()
//term t PL new term refs(int n)
//#define PL_copy_term_ref(from)
//void PL reset term refs(term t after)

#define PL_term_type(term) SP_term_type(term)
	#define PL_VARIABLE SP_TYPE_VARIABLE
	#define PL_ATOM SP_TYPE_ATOM 
	//#define PL_STRING 
	#define PL_INTEGER SP_TYPE_INTEGER
	#define PL_FLOAT SP_TYPE_FLOAT
	#define PL_TERM SP_TYPE_COMPOUND
#define PL_is_variable(term) SP_is_variable(term)
//#define PL_is_ground(term_t)
#define PL_is_atom(term) SP_is_atom(term)
//#define PL_is_string(term_t)
#define PL_is_integer(term) SP_is_integer(term)
#define PL_is_float(term) SP_is_float(term)
#define PL_is_compound(term) SP_is_compound(term)
//#define PL_is_functor(term_t, functor_t)
#define PL_is_list(term) SP_is_list(term)
#define PL_is_atomic(term) SP_is_atomic(term)
#define PL_is_number(term) SP_is_number(term)

#define PL_get_atom(term, atom) SP_get_atom(term, atom)
#define PL_get_atom_chars(term, chars) SP_get_string(term, chars)
//#define PL_get_string chars(term t +t, char **s, int *len)
// -> SP_get_list_chars(SP_term_ref t, char **s)
// -> int SP_get_list_n_chars(SP_term_ref t, SP_term_ref tail, long n, long *w, char *s)
//#define PL_get_chars(term t +t, char **s, unsigned flags)
// -> int SP_get_number_chars(SP_term_ref t, char **s)
//#define PL_get_list_chars(+term t l, char **s, unsigned flags)
//#define PL_get_integer(+term t t, int *i)
#define PL_get_long(term, long) SP_get_integer(term, long)
//#define PL_get_int64(term t +t, int64 t *i)
// -> int SP_get_integer_bytes(SP_term_ref tr, void *buf, size_t *pbuf_size, int native)
//#define PL_get_bool(term t +t, int *val)
#define PL_get_pointer(term, pointer) SP_get_address(term, pointer)
#define PL_get_float(term, double) SP_get_float(term, double)
#define PL_get_functor(term, functor) SP_get_functor(term, &(*functor.name), &(*functor.arity))
#define PL_get_name_arity(term, name, arity) SP_get_functor(term, name, arity)
//#define PL_get_module(term t +t, module t *module)
#define PL_get_arg(index, term, arg) SP_get_arg(index, term, arg)
#define _PL_get_arg(index, term, arg) SP_get_arg(index, term, arg)
//int PL get atom nchars(term t t, size t *len, char **s)
//int PL get list nchars(term t t, size t *len, char **s)
//int PL get nchars(term t t, size t *len, char **s, unsigned int flags)

//atom t PL new atom nchars(size t len, const char *s)
//const char * PL atom nchars(atom t a, size t *len)
//atom t PL new atom wchars(size t len, const pl wchar t *s)
//pl wchar t* PL atom wchars(atom t atom, int *len)
//int PL get wchars(term t t, size t *len, pl wchar t **s, unsigned flags)
//int PL unify wchars(term t t, int type, size t len, const pl wchar t *s)
//int PL unify wchars diff(term t +t, term t -tail, int type, size t len, const pl wchar t *s)

#define PL_get_list(term, head, tail) SP_get_list(term, head, tail)
//int PL get head(term t +l, term t -h)
//int PL get tail(term t +l, term t -t)
//int PL get nil(term t +l)

#define PL_put_variable(term) SP_put_variable(term)
#define PL_put_atom(term, atom) SP_put_atom(term, atom)
#define PL_put_atom_chars(term, chars) SP_put_string(term, chars)
//#define PL_put_string chars(term t -t, const char *chars)
//#define PL_put_string nchars(term t -t, size t len, const char *chars)
//#define PL_put_list chars(term t -t, const char *chars)
//int PL put atom nchars(term t t, size t len, const char *s)
//int PL put string nchars(term t t, size t len, const char *s)
//int PL put list ncodes(term t t, size t len, const char *s)
//int PL put list nchars(term t t, size t len, const char *s)
// -> int SP_put_list_chars(SP_term_ref t, SP_term_ref tail, char *s)
// -> int SP_put_list_n_chars(SP_term_ref t, SP_term_ref tail, long n, char *s)
#define PL_put_integer(term, long) SP_put_integer(term, long)
#define PL_put_int64(term, int64) SP_put_integer_bytes(term, (void*) &(int64), 8, 1) 
#define PL_put_pointer(term, pointer) SP_put_address(term, pointer)
#define PL_put_float(term, double) SP_put_float(term, double)
#define PL_put_functor(term, functor) SP_put_functor(term, functor.name, functor.arity)
#define PL_put_list(term) SP_put_list(term)
//#define PL_put_nil(term t -l)
#define PL_put_term(to, from) SP_put_term(to, from)

#if defined _MSC_VER && _MSC_VER < 1400
	//TODO: VC < 2005 -> variadic macro not supported
	#define PL_cons_functor(term, functor, t1, t2) SP_cons_functor(term, functor.name, functor.arity, t1, t2)
#else
	#define PL_cons_functor(term, functor, ...) SP_cons_functor(term, functor.name, functor.arity, __VA_ARGS__)
#endif

//void PL cons functor v(term t -h, functor t f, term t a0)
#define PL_cons_list(term, head, tail) SP_cons_list(term, head, tail)

/////////////////////////////   UNIFICATION    ////////////////////////////////////////////
#define SP_unify_type(term, type, value) (_term = SP_new_term_ref(), \
					SP_put_##type(_term, value) && SP_unify(term, _term))

#define PL_unify(term1, term2) SP_unify(term1, term2)
#define PL_unify_atom(term, atom) SP_unify_type(term, atom, atom)
//#define PL_unify_chars(term t ?t, int flags, size t len, const char *chars)
#define PL_unify_atom_chars(term, chars) SP_unify_type(term, string, chars)
//#define PL_unify_list_chars(term t ?t, const char *chars) //->  SP_put_list_chars(SP_term_ref t, SP_term_ref tail, char *s)
//#define PL_unify_string chars(term t ?t, const char *chars) //-> int SP_put_list_chars(SP_term_ref t, SP_term_ref tail, char *s)
//#define PL_unify_string nchars(term t ?t, size t len, const char *chars) //->int SP_put_list_n_chars(SP_term_ref t, SP_term_ref tail, long n, char *s)
#define PL_unify_integer(term, long) SP_unify_type(term, integer, long)
//#define PL_unify_int64(term t ?t, int64 t n) //->int SP_put_integer_bytes(SP_term_ref tr, void *buf, size_t buf_size, int native)
#define PL_unify_float(term, double) SP_unify_type(term, float, double)
#define PL_unify_pointer(term, pointer) SP_unify_type(term, address, pointer) 

//#define PL_unify_functor(term t ?t, functor t f) //-> int SP_put_functor(SP_term_ref t, SP_atom name, int arity)
//#define PL_unify_list(term t ?l, term t -h, term t -t)
//#define PL_unify_nil(term t ?l)
//#define PL_unify_arg(int index, term t ?t, term t ?a)
//#define PL_unify_term(term t ?t, ... )
	//...
//int PL unify atom nchars(term t t, size t len, const char *s)
//int PL unify string nchars(term t t, size t len, const char *s)
//int PL unify list ncodes(term t t, size t len, const char *s)
//int PL unify list nchars(term t t, size t len, const char *s)

//int PL chars to term(const char *chars, term t -t)
// -> int SP_read_from_string(SP_term_ref t, const char*string, SP_term_ref vals[])
//char * PL quote(int chr, const char *string)

// -> int SP_compare(SP_term_ref x, SP_term_ref y)

#define PL_malloc(size) SP_malloc(size)
#define PL_realloc(ptr, size) SP_realloc(ptr, size)
#define PL_free(ptr) SP_free(ptr)
// -> void * SP_calloc(size_t nmemb, size_t size)
// -> char * SP_strdup(const char *str)

#define PL_raise_exception(exception) (SP_raise_exception(exception), SP_ERROR)

//#define PL_register_foreign(const char *name, int arity, foreign t (*function)(), int flags)
//#define PL_register_foreign_in_module(module, name, arity, foreign t (*function)(), flags) SP_define_c_predicate(name, arity, module, SP_CPredFun *proc, NULL)
	// ...

#endif /*SICSTUS_COMPATIBILITY_H_*/
