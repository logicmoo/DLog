#include <sicstus/sicstus.h>

void new_loop(SP_term_ref x1, SP_term_ref x2, SP_term_ref x3) {
  SP_term_ref loophash = SP_new_term_ref(), func = SP_new_term_ref(), 
    arg = SP_new_term_ref(), tmp = SP_new_term_ref(),
    tmp1 = SP_new_term_ref();
  unsigned long func_atom, arg_atom, hash;
  int arity, row, col;
  if (SP_unify(x2,x3)==SP_FAILURE)
    SP_fail();
  SP_get_arg(1, x2, loophash);
  SP_get_functor(x1, &func_atom, &arity);
  SP_get_arg(1, x1, arg);
  SP_get_atom(arg, &arg_atom);
  hash = (((func_atom & 0xfffffff)>>2) ^
	  ((arg_atom & 0xfffffff)<<11)) % 49997;
  row = hash / 250;
  col = hash % 250;
  SP_get_arg(row+1, loophash, tmp); 
  SP_get_arg(col+1, tmp, tmp); 
  while (! SP_is_variable(tmp))
    {
	SP_get_arg(2, tmp, tmp);
    }
  SP_put_list(tmp1);
  SP_get_arg(1, tmp1, arg);
  SP_unify(arg, x1);
  SP_unify(tmp, tmp1);
   }

int check_loop(SP_term_ref x1, SP_term_ref x2) {
  SP_term_ref loophash = SP_new_term_ref(), func = SP_new_term_ref(), 
              arg = SP_new_term_ref(), tmp = SP_new_term_ref();
  unsigned long func_atom, arg_atom, hash;
  int arity, row, col;

  SP_get_arg(1, x2, loophash);
  SP_get_functor(x1, &func_atom, &arity);
  SP_get_arg(1, x1, arg);
  SP_get_atom(arg, &arg_atom);
  hash = (((func_atom & 0xfffffff)>>2) ^
	  ((arg_atom & 0xfffffff)<<11)) % 49997;
  row = hash / 250;
  col = hash % 250;
  SP_get_arg(row+1, loophash, tmp);
  SP_get_arg(col+1, tmp, tmp);
  while (! SP_is_variable(tmp))
    {
      SP_get_arg(1, tmp, arg);
      if (SP_unify(arg, x1) == SP_SUCCESS)
	return 1;
      else
	SP_get_arg(2, tmp, tmp);
    }
  return 0;
}
