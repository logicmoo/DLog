#ifndef BACKTRACKABLE_HASHSET_H_
#define BACKTRACKABLE_HASHSET_H_



#ifdef __SWI_PROLOG__
	#include <SWI-Prolog.h>
#else
	#include <sicstus/sicstus.h>
	#include "sicstus_compatibility.h"
#endif

///Define for debug mode including logging and error checking.
//#define DEBUG_DLOG_HASH

/**
 * The following parameters provide default value,
 * can be redefined with earlier #define or -D
 */
#ifndef DEFAULT_TABLE_SIZE
	/**
	 * Default size (number of buckets) of the hash table.
	 * This value is used if the size is not specified when creating the table.
	 */
	#define DEFAULT_TABLE_SIZE 1024
#endif
#ifndef BUCKET_SIZE
	///The number of elements stored in a bucket.
	#define BUCKET_SIZE 2
#endif
#ifndef STACK_SIZE
	///Stack is allocated in units that can store STACK_SIZE elements. 
	#define STACK_SIZE 50
#endif
#ifndef FREE_STACK_RATIO
	///Stack is freed if the previous stack piece is less than FREE_STACK_RATIO full. 
	#define FREE_STACK_RATIO 0.8
#endif


///Calculate the hash value of (func_atom, arg_atom).
#define hashval(func_atom, arg_atom) (((func_atom) ^ ((arg_atom) << 16)) % 49997)
//#define hashval(elem) (((elem)->func_atom ^ ((elem)->arg_atom << 16)) % 49997)

///An element stored in the hashset.
typedef struct {
	atom_t func_atom, arg_atom;
} HashElem;

///A bucket of the hash table.
typedef struct Bucket {
	///Free elements are NULL, used are pointers to the stack position.
	HashElem *elems[BUCKET_SIZE];
	///Pointer to the overflow bucket. NULL if non-existent.
	struct Bucket *next;
} Bucket;

///Stack used for backtracking and storing the elements.
typedef struct Stack {
	///Stores the elements of the hash set. First free member is marked by count (external).
	HashElem elems[STACK_SIZE];
	///Pointer to the next part. NULL if non-existent.
	struct Stack *next;
	///Pointer to the previous part. NULL it first.
	struct Stack *prev;
} Stack;

/**
 * The main datastructure.
 * {stack_end, count} marks the insertion point if it already exists 
 * (alloc_flag is zero). If alloc_flag is non-zero, a new stack part 
 * has to be allocated first.
 */
typedef struct
{
	///Number of buckets in hash table.
	long size;
	///Number of elements currently stored in hash table.
	long count;
	///Non-zero if stack allocation is required before adding new element.
	int alloc_flag;
	
	///Stack used for backtracking and storing the elements.
	Stack stack;
	///Pointer to the element of the stack where to add the new element.
	Stack *stack_end;
	///The hash table.
	Bucket *hash;
} Backtrackable_HashSet;

///Constructor, (re)initializes a Backtrackable_HashSet. 
long init_Backtrackable_HashSet(Backtrackable_HashSet *h, const long size);

///Destructor, frees up all excess memory allocated during the use of the hash set.
void delete_Backtrackable_HashSet(Backtrackable_HashSet *h);

///Empties the hash set and changes its size if necessary.
long clear_Backtrackable_HashSet(Backtrackable_HashSet *h, const long size);

///Roll back the hash set to state and add {func_atom, arg_atom}.
long put_to_Backtrackable_HashSet(Backtrackable_HashSet *h, const long state, 
		const atom_t func_atom, const atom_t arg_atom);

///Roll back the hash set to state and check if {func_atom, arg_atom} is present in it.
int check_Backtrackable_HashSet(Backtrackable_HashSet *h, const long state, 
		const atom_t func_atom, const atom_t arg_atom);

//Logging
#ifdef DEBUG_DLOG_HASH
	#include <stdio.h>
	#ifndef HASH_LOG_NAME
		///Name of the log file. 
		#define HASH_LOG_NAME "hash_log.txt"
	#endif
	//WARNING: before using the library, hash_log needs to be initialized!
	//hash_log = fopen(HASH_LOG_NAME, "w");
	extern FILE *hash_log;

	#if defined _MSC_VER && _MSC_VER < 1400
		// VC < 2005 -> variadic macro nor supported
		// Problem: if multi-threaded may get jumbled between the two parts
		#include <stdarg.h> 
		#define LOG fprintf(hash_log, "%s:%d ", __FILE__, __LINE__) && _log
		int _log(const char *format, ...);
	
	#else
		// TODO: , ## __VA_ARGS__
		#define LOG(format, ...) \
				do{ \
					fprintf(hash_log, "%s:%d " format, __FILE__, __LINE__, __VA_ARGS__); \
					fflush(hash_log); \
				}while(0) 
	#endif
#else
	#define LOG
#endif

		
//Error values
//ENOMEM
#define ALLOC_ERROR -1
//EINVAL
#define INVALID_ARGUMENT -2
//overflow: elem not found in the last bucket; shouldn't happen ->EOVERFLOW/EUCLEAN
#define OVERFLOW -3
#define ALREADY_ADDED -4


#endif /*BACKTRACKABLE_HASHSET_H_*/
