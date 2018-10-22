/*
 * Linked List Utilities for XIBIS module
 */

#include <stdlib.h>
#include <string.h>
#include "ibislists.h"

static void _free_element( List *elt )
{
	if (elt)
	{
		if (elt->destruct)
			(*elt->destruct)(elt->value);
		free (elt);
	}
}

/**
 **  Creation Method for lists
 **   A list consists of a list of values or pointers
 **   to objects. If the objects should be destroyed
 **   along with the list, the destruction method
 **   should be specified here.
 **/

List* _i_new_list(void (*kill_method)())
{
	List *newlist;
	
	newlist = (List *)calloc( 1L, sizeof(List) );
	if (newlist && kill_method)
	   newlist->destruct = kill_method;
	return ( newlist );
}

void _i_free_list(List *list )
{

	if (list)
	{
		List *ent=list->next;
		List *next;
		while (ent)
		{
			next = ent->next;
			_free_element( ent );
			ent = next;
		}

		free (list);
	}
}

/**
 **  Destruction of single list entry
 **   -- if the value has a destruction
 **      method, destroy it, too.
 **/



List* _i_find_value(List *list, list_value value)
{
	while(list && list->value!=value)
		list=list->next;

	return (list);
}

/**
 ** count the number of values in list
 **/
 
int _i_count_list(List *list)
{
	int count=0;
	
	if (list)
		for(count=0,list=list->next; list; list=list->next)
			count++;

	return (count);
}


/**
 **  Install a new value into the list.
 **  if there is a destruction method,
 **  install that, too.
 **/


void _i_insert_value(List* list, list_value value)
{
	register List *head, *ent;

	/* make sure its not already in list */
	
	if (!_i_find_value(list, value))
	{	
		/* initialize list element */
		ent = (List *)malloc(sizeof(List));
		ent->value=value;
		ent->destruct = list->destruct;  /* destruction method */
		ent->next=list->next;
		ent->prev=(List *)0;
		
		if ((head=list->next)) head->prev=ent;
		
		list->next=ent;
	}
}

void _i_append_value(List* list, list_value value)
{
	register List *ent;

	/* make sure its not already in list */
	
	if (!_i_find_value(list, value))
	{	
		/* initialize list element */
		ent = (List *)malloc(sizeof(List));
		ent->value=value;
		ent->destruct = list->destruct;  /* destruction method */
		
		/* find end of list */
		while (list->next) list=list->next;
		
		ent->prev=list;
		ent->next=(List *)0;		
		list->next=ent;
	}
}


void _i_delete_value(List* list, list_value value)
{
	register List *elt,*next,*prev;

	/* make sure its in list */
	if ((elt=(List*)_i_find_value(list,value)))
	{	
		/* unlink from chain */
		
		/* Handle special case if first element */
		if (elt==list->next)
		{
			list->next=elt->next;
			elt->prev=(List *)0;
		}
		else
		{
			next=elt->next;
			prev=elt->prev;
			if (next) next->prev=prev;
			if (prev) prev->next=next;
		}
		
		_free_element(elt);	
	}
}

