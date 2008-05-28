#ifndef BACKTRACKABLE_HASHSET_H_
#define BACKTRACKABLE_HASHSET_H_

#include <cstdlib>

class Backtrackable_HashSet
{
public:
	/**
	 * Abstract superclass for elements to be stored in the set.
	 */
	class Hashable{
	public:
		/**
		 * The hash value of this object.
		 */
		virtual long hashval() const = 0;
		/**
		 * Equality of two objects.
		 */		
		virtual bool operator==(const Hashable&) const = 0;
		/**
		 * The object returned by this method is stored in the hash set.
		 * It is deleted upon backtracking.
		 */
		virtual Hashable* copy() {return this;}
	};	
	
	Backtrackable_HashSet();
	Backtrackable_HashSet(const int size);
	virtual ~Backtrackable_HashSet();
	
	/**
	 * Clear the hash table 
	 * @return a state reference for the cleared state.
	 */
	void *clear();
	/**
	 * Roll back the hash table to state and add elem. 
	 * This invalidates all states created by this hashtable after creating state.
	 * @param state is a valid state created by this hash table.
	 * @param elem is the element to add.
	 * @return the new state.
	 */
	void *put(void *state, Hashable &elem);
	/**
	 * Roll back the hash table to state and check if elem is present in the hash table. 
	 * This invalidates all states created by this hashtable after creating state.
	 * @param state is a valid state created by this hash table.
	 * @param elem is the element to look up.
	 * @return whether elem was found in the hash table.
	 */
	bool check(void *state, const Hashable &elem);
	
private:
	int size;
	struct LinkedList {
		Hashable *elem;
		LinkedList *next;
		LinkedList(Hashable *e){next = NULL; elem = e;}
		LinkedList(Hashable *e, LinkedList *n){next = n; elem = e;}
	};
	LinkedList *heap;
	LinkedList *heap_end;
	LinkedList **hash;
	/**
	 * Roll back the hash table to target state (remove any added elements since target)
	 */
	void rollback(LinkedList *target);
	/**
	 * Remove elem from hash.
	 */
	void remove(Hashable *elem);
};




#endif /*BACKTRACKABLE_HASHSET_H_*/
