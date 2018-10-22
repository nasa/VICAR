/* ibislists.h */

#ifndef _H_ILIST
#define _H_ILIST

/* public types */

typedef int (*list_value);

struct List {
	struct List *next;
	struct List *prev;
	void (*destruct)(list_value); /* destruction method for value */
	list_value value;
};
typedef struct List List;


/* public declarations */

List* _i_new_list( void (*kill_method)(list_value));
void _i_free_list( List *list);
List* _i_find_value(List *list, list_value value);
void _i_insert_value(List *list, list_value value);
void _i_append_value(List *list, list_value value);
void _i_delete_value(List *list, list_value value);


#endif /* _H_ILIST */

