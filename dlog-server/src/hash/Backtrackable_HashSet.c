#include "Backtrackable_HashSet.h"

#include <errno.h>
#include <string.h> //memset

#ifdef DEBUG_DLOG_HASH
	//WARNING: before using the library, hash_log needs to be initialized!
	FILE *hash_log = NULL;
	#if defined _MSC_VER && _MSC_VER < 1400
		// VC < 2005 -> variadic macro nor supported
		int _log(const char *format, ...){
			va_list args;
			va_start(args, format);
			vfprintf(hash_log, format, args);
			va_end(args);
			fflush(hash_log);
			return 1;
		}
	#endif
#endif

///Rollback the hash set h to the target state.
long rollback(Backtrackable_HashSet *h, const long target);
///Remove all elements added to the hash set h after the target state.
long rollback0(Backtrackable_HashSet *h, const long target);

/**
 * Constructor, (re)initializes (allocates and clears) a newly created 
 * (or destroyed) Backtrackable_HashSet.
 * @param h is the set to be initialized.
 * @param size is the number of buckets in the hash table. 
 * If size <= 0, DEFAULT_TABLE_SIZE is used.
 * @return 0 (the state reference for the empty hash set) on success, 
 * ALLOC_ERROR otherwise. May return INVALID_ARGUMENT if DEBUG_DLOG_HASH is 
 * defined.
 */
long init_Backtrackable_HashSet(Backtrackable_HashSet *h, const long size)
{
	LOG("init_Backtrackable_HashSet(%p, %ld)\n", h, size);
#ifdef DEBUG_DLOG_HASH
	//check arguments
	if(!h){
		LOG("init_Backtrackable_HashSet(%p, %ld): null pointer error.\n", h, size);
		errno = EINVAL;
		return INVALID_ARGUMENT;
	}
#endif
	
	if(size>0) h->size = size;
	else h->size = DEFAULT_TABLE_SIZE; 
	h->count = 0;
	h->alloc_flag = 0;
//	memset(&(h->stack), 0, sizeof(Stack)); //not needed, count marks valid data
	h->stack.next = NULL;
	h->stack.prev = NULL;
	h->stack_end=&(h->stack);
	h->hash = (Bucket*) calloc(h->size, sizeof(Bucket));
	if(h->hash){
		memset(h->hash, 0, (h->size) * sizeof(Bucket));
		LOG("init_Backtrackable_HashSet(%p, %ld) succeeded.\n", h, size);
		return 0;
	}else{
		LOG("init_Backtrackable_HashSet(%p, %ld): alloc error.\n", h, size);
		return ALLOC_ERROR;
	}
}



/**
 * Destructor, frees up all excess memory allocated during the use of the hash 
 * set. The destructed hash set can be freed, or reinitialized using 
 * init_Backtrackable_HashSet. Always succeeds. If DEBUG_DLOG_HASH is enabled
 * and h is NULL, logs it and sets errno to EINVAL.
 * @param h is the set to be destroyed.
 */
//runtime: iterate over hash + free overflow buckets + free stack == 
//	size + size*f(count, size*BUCKET_SIZE) + count/STACK_SIZE
void delete_Backtrackable_HashSet(Backtrackable_HashSet *h)
{
	long i;
	Bucket *b1, *b2;
	Stack *h1, *h2;
	LOG("delete_Backtrackable_HashSet(%p)\n", h);
	
#ifdef DEBUG_DLOG_HASH
	//check arguments
	if(!h){
		LOG("delete_Backtrackable_HashSet(%p): null pointer error.\n", h);
		errno = EINVAL;
		return;
	}
#endif
	
	//iterate over hash table
	for(i=0; i < h->size; i++){
		b1 = h->hash[i].next;
		while(b1){
			//free overflow buckets
			b2 = b1 ->next;
			free(b1);
			b1 = b2;
		}
	}
	free(h->hash);
	
	//free stack
	h1 = h->stack.next;
	while(h1){
		h2 = h1->next;
		free(h1);
		h1 = h2;
	}
	LOG("delete_Backtrackable_HashSet(%p) complete.\n", h);
}

/**
 * Clears the hash set. Depending on the number of elements in the set either 
 * removes them one by one, or destroys and reinitializes the set.
 * @param h is the set to be cleared.
 * @return 0 (the state reference for the empty hash set) on success, 
 * ALLOC_ERROR otherwise. If DEBUG_DLOG_HASH is defined might return 
 * INVALID_ARGUMENT or OVERFLOW.
 */
long clear_Backtrackable_HashSet(Backtrackable_HashSet *h, const long size)
{
	LOG("clear_Backtrackable_HashSet(%p, %ld)\n", h, size);
	// unchanged           remains default size
	if(size == h->size || !size && h->size == DEFAULT_TABLE_SIZE){
		return rollback(h, 0);
	}else{ //change table size
		delete_Backtrackable_HashSet(h); 
		return init_Backtrackable_HashSet(h, size); 
	}	
}

/**
 * Roll back the hash set to state and add {func_atom, arg_atom}. 
 * This invalidates all states created by this hashtable after creating state.
 * @param h is the hash set.
 * @param state is a valid state created by this hash table.
 * @param func_atom is the functor atom,
 * @param arg_atom is the argument atom of the element to add.
 * @return the new state (>0) on success or ALLOC_ERROR on error. 
 * If DEBUG_DLOG_HASH is defined might return INVALID_ARGUMENT (if h is NULL, 
 * state<0 or state>h->count), OVERFLOW (should not happen) or ALREADY_ADDED 
 * (if {func_atom, arg_atom} is already in the hash set in state; in this case
 * the hash set remains valid with the latest state state).
 */
long put_to_Backtrackable_HashSet(Backtrackable_HashSet *h, const long state, 
		const atom_t func_atom, const atom_t arg_atom)
{
	long hashval;
	long i;
	Bucket *b;
	HashElem *elem;

	LOG("put_to_Backtrackable_HashSet(%p, %ld, %ld, %ld)\n", h, state, func_atom, arg_atom);
	
	i=rollback(h, state);
	if(i<0){
		//ALLOC_ERROR(, etc.)
		LOG("put_to_Backtrackable_HashSet(%p, %ld, %ld, %ld): rollback error: %ld.\n", 
				h, state, func_atom, arg_atom, i);
		return i; //error
	}
	
	hashval = hashval(func_atom, arg_atom) % h->size;
#ifdef DEBUG_DLOG_HASH
	//check if element is already present in hash set.
	i=0;	
	b = &(h->hash[hashval]);
	while(b->elems[i]){
		if(b->elems[i]->arg_atom == arg_atom && b->elems[i]->func_atom == func_atom){
			LOG("put_to_Backtrackable_HashSet(%p, %ld, %ld, %ld) error: member already added.\n", 
					h, state, func_atom, arg_atom);
			errno = EDOM;//EUCLEAN;
			return ALREADY_ADDED; //TODO: error or already added: return state;
		}
			
		if(++i >= BUCKET_SIZE){
			if(b->next){
				b = b->next;
				i = 0;
			} else break; //not found -> OK
		}
	}
#endif
	
	//add to stack
	if(h->alloc_flag){
		//need more stack
		h->alloc_flag = 0;
		h->stack_end->next = (Stack*) malloc(sizeof(Stack));
		if(!h->stack_end->next){
			LOG("put_to_Backtrackable_HashSet(%p, %ld, %ld, %ld): alloc error.\n", 
					h, state, func_atom, arg_atom);
			return ALLOC_ERROR; //OOM
		}
		//memset(h->stack_end->next, 0, sizeof(Stack)); //not needed
		h->stack_end->next->next = NULL;
		h->stack_end->next->prev = h->stack_end;
		h->stack_end = h->stack_end->next; 
	}
	elem = &(h->stack_end->elems[h->count++ % STACK_SIZE]);
	elem->arg_atom = arg_atom;
	elem->func_atom = func_atom;
	if(h->count % STACK_SIZE == 0){
		//stack full
		if(h->stack_end->next) //already allocated
			h->stack_end = h->stack_end->next;
		else h->alloc_flag = 1; //need to allocate next time
	}
	
	//add to hash table
	i = 0;
	b = &(h->hash[hashval]);
	while(b->elems[i]){ //slot used
		if(++i >= BUCKET_SIZE){
			//bucket full
			if(! b->next){
				//need new bucket
				b->next = (Bucket *) malloc(sizeof(Bucket));
				if(! b->next){
					LOG("put_to_Backtrackable_HashSet(%p, %ld, %ld, %ld): alloc error.\n", 
							h, state, func_atom, arg_atom);
					return ALLOC_ERROR; //OOM
				}
				memset(b->next, 0, sizeof(Bucket));
			}
			b = b->next;
			i = 0;
		}
	}
	b->elems[i] = elem;

	LOG("put_to_Backtrackable_HashSet(%p, %ld, %ld, %ld): success, new count: %ld.\n", 
			h, state, func_atom, arg_atom, h->count);
	return h->count;
}

/**
 * Roll back the hash set to state and check if {func_atom, arg_atom} is present 
 * in the hash table. This invalidates all states created by this hashtable 
 * after creating state.
 * @param h is the hash set.
 * @param state is a valid state created by this hash table.
 * @param func_atom is the functor atom,
 * @param arg_atom is the argument atom of the element to look up.
 * @return 1 if elem was found in the hash table, 0 if not found or ALLOC_ERROR 
 * on error. If DEBUG_DLOG_HASH is defined might return INVALID_ARGUMENT or 
 * OVERFLOW.
 */
int check_Backtrackable_HashSet(Backtrackable_HashSet *h, const long state, 
		const atom_t func_atom, const atom_t arg_atom)
{	
	long hashval;
	long i;
	Bucket *b;
	
	LOG("check_Backtrackable_HashSet(%p, %ld, %ld, %ld)\n", h, state, func_atom, arg_atom);
	
	i = rollback(h, state);
	if(i != state){
		//ALLOC_ERROR(, etc.)
		LOG("check_Backtrackable_HashSet(%p, %ld, %ld, %ld): rollback error: %ld\n", 
				h, state, func_atom, arg_atom, i);
		return i;
	}
	
	hashval = hashval(func_atom, arg_atom) % h->size;
	i = 0;
	b = &(h->hash[hashval]);
	while(b->elems[i]){
		if(b->elems[i]->arg_atom == arg_atom && b->elems[i]->func_atom == func_atom){
			//found
			LOG("check_Backtrackable_HashSet(%p, %ld, %ld, %ld): found.\n", 
					h, state, func_atom, arg_atom);
			return 1;
		}
		if(++i >= BUCKET_SIZE){
			if(b->next){
				//next bucket
				b = b->next;
				i = 0;
			} else{
				//no more buckets -> not found
				LOG("check_Backtrackable_HashSet(%p, %ld, %ld, %ld): not found (no more buckets).\n", 
						h, state, func_atom, arg_atom);
				return 0;
			}
		}
	}
	
	//empty bucket -> not found
	LOG("check_Backtrackable_HashSet(%p, %ld, %ld, %ld): not found (empty bucket).\n", 
			h, state, func_atom, arg_atom);
	return 0;
}


/**
 * Rollback the hash set h to the target state. If the target is 0 (empty table)
 * and there are many elements in the table, deletes and reinitializes the hash 
 * set. Otherwise removes elements one by one.
 * @param h is the hash set to roll back.
 * @param target is the target state.
 * @return the new state (target) on success, or ALLOC_ERROR on error. 
 * If DEBUG_DLOG_HASH is defined might return INVALID_ARGUMENT (if h is NULL, 
 * state<0 or state>h->count) or OVERFLOW (should not happen; indicates a 
 * critical corruption of the hash set). 
 */
long rollback(Backtrackable_HashSet *h, const long target){
	LOG("rollback(%p, %ld)\n", h, target);
#ifdef DEBUG_DLOG_HASH
	//check arguments
	if(!h || target < 0 || target > h->count){
		LOG("rollback(%p, %ld) error: invalid argument.\n", h, target);
		errno = EINVAL;
		return INVALID_ARGUMENT;
	}
#endif
	if(target == h->count){
		LOG("rollback(%p, %ld): already at target.\n", h, target);
		return target;
	}
	
	if(target <= 0){ //if(target == 0){
		//empty the hash set
		long size = h->size;
		//TODO: tune heuristics
		if(h->count > (DEFAULT_TABLE_SIZE / 2) ){
			LOG("rollback(%p, %ld): delete and reinit.\n", h, target);
			delete_Backtrackable_HashSet(h); //iterates over whole hash table, frees it
			return init_Backtrackable_HashSet(h, size); // and reallocates
		}else{
			//removes all elements one by one freeing excess memory
			return rollback0(h, 0);
		}
	} else return rollback0(h, target);
}

/**
 * Remove all elements added to the hash set h after the target state one by 
 * one. Overflow buckets are freed immediately when empty, stack is freed if
 * the previous stack piece is less than FREE_STACK_RATIO full.
 * @param h is the hash set to roll back.
 * @param target is the target state.
 * @return the new state (target) on success. 
 * If DEBUG_DLOG_HASH is defined might return OVERFLOW (should not happen; 
 * indicates a critical corruption of the hash set). 
 */
long rollback0(Backtrackable_HashSet *h, const long target){
	HashElem *elem;
	long hashval;
	Bucket *b1, *b2;
	long i;
	Stack *s1, *s2;
	
	LOG("rollback0(%p, %ld)\n", h, target);
	
	while(h->count > target){
		//{stack_end, count} marks the insertion point if it already exists;
		//item to remove == insertion point after removal
		if(h->count-- % STACK_SIZE == 0){// && h->stack_end->prev){ 
				//target is already checked >= 0 --> count > 0 --> no need to check this
			if(h->alloc_flag) h->alloc_flag = 0; //stack no longer full
			else h->stack_end = h->stack_end->prev; //step to previous stack part
		}
		elem = &(h->stack_end->elems[h->count % STACK_SIZE]);
		hashval = hashval(elem->func_atom, elem->arg_atom) % h->size;
		b1=&(h->hash[hashval]);
		b2 = NULL;
		while(b1->next){ //select last overflow bucket (removing last elem first)
			b2 = b1;
			b1 = b1->next;
		}
		if(b1->elems[0]==elem){
			//removing last elem from bucket
			if(b2){
				//removed last elem from overflow bucket, free it
				free(b2->next); 
				b2->next = NULL;
			} else b1->elems[0]=NULL; //removed last member from only bucket
		}else{
			//removing other elem from bucket
			i = 1; 
#ifdef DEBUG_DLOG_HASH
			//check that elem is really in the hash table, protect against overflow
			while(i < BUCKET_SIZE && b1->elems[i]!=elem) i++;
			if(i < BUCKET_SIZE){
				b1->elems[i] = NULL;
			}
			else{
				LOG("rollback0(%p, %ld): overflow error.\n", h, target);
				errno = EDOM; //EOVERFLOW;//EUCLEAN; //not supported in VS6
				return OVERFLOW; //overflow: elem not found in the last bucket; shouldn't happen
			}
#else
			while(b1->elems[i]!=elem) i++; //find elem
			b1->elems[i] = NULL; //and remove it
#endif
		}		
	}//while(h->count > target)
	
	//remove excess stacks
	if(h->stack_end->next){
		if(h->count % STACK_SIZE < STACK_SIZE * FREE_STACK_RATIO){
			//free next stack
			s1 = h->stack_end->next;
			h->stack_end->next = NULL;
		}else{
			//keep next, free rest
			s1 = h->stack_end->next->next;
			h->stack_end->next->next = NULL;
		}
		
		while(s1){
			s2 = s1->next;
			free(s1);
			s1 = s2;		
		}
	}//else: no empty stack parts -> nothing to free.
	
	LOG("rollback0(%p, %ld): success\n", h, target);
	//success
	return target;
}

