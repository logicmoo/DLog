#include "Backtrackable_HashSet.h"

#include <cstdlib>

//#include <fstream>
//using namespace std;
//ofstream log("log.txt");


Backtrackable_HashSet::Backtrackable_HashSet()
{	
	size = 1000;
	heap=heap_end=NULL;
	hash = new LinkedList*[size];
	for(int i=0; i<size; i++)
		hash[i] = NULL;
}

Backtrackable_HashSet::Backtrackable_HashSet(const int size)
{	
	this->size = size;
	heap=heap_end=NULL;
	hash = new LinkedList*[size];
	for(int i=0; i<size; i++)
			hash[i] = NULL;
}

Backtrackable_HashSet::~Backtrackable_HashSet()
{
	delete [] hash;
	delete heap;
}


void *Backtrackable_HashSet::clear(){
	rollback(NULL);
	return NULL;
}

void *Backtrackable_HashSet::put(void *state, Hashable &elem){
	rollback((LinkedList*)state);
	
	long hashval = elem.hashval() % size;
	Hashable *e = elem.copy();
	hash[hashval] = new LinkedList(e, hash[hashval]);
	
	LinkedList *l = new LinkedList(e);
	if(!heap) heap = l;
	if(heap_end) heap_end->next = l;
	heap_end = l;
	
	return heap_end;
}

bool Backtrackable_HashSet::check(void *state, const Hashable &elem){
	rollback((LinkedList*)state);
	
	long hashval = elem.hashval() % size;
	LinkedList *list = hash[hashval];
	
	while(list){
		if(*list->elem == elem){
			return true;
		}
		list = list->next;
	}

	return false;
}

void Backtrackable_HashSet::rollback(LinkedList *target){
	if(!target){
		target = heap;
		heap = NULL;
		heap_end = NULL;
	}
	else{
		heap_end = target;
		target = target->next;
	}

	while(target){
		remove(target->elem);
		delete (target->elem);
		LinkedList *next = target->next;
		delete target;
		target = next;
	}
}

void Backtrackable_HashSet::remove(Hashable *elem){
	long hashval = elem->hashval() % size;
	LinkedList *list = hash[hashval];
	LinkedList *prev = NULL;
	while(list){
		if(*(list->elem) == *elem){
			if(prev){
				prev->next = list->next;
			}else{
				hash[hashval] = list->next;
			}
			delete list;
			return;
		}
		prev = list;
		list = list->next;
	}

	throw("Element not found (this shouldn't happen)!");
}
