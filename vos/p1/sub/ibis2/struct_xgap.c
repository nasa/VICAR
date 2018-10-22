/**
 **  XGAP -- File Gap Manager:
 **   keeps track of available free space in IBIS file.
 **
 **  The publicly visible routines are _i_allocate_space(),
 **   _i_deallocate_space(), and _i_find_space().
 **/


#include "ibis.h"

void _i_init_space(XIBIS* ibis)
{
	XCOL *col;
	int i;
	int *size=ibis->format_size;
	
	_i_deallocate_space( ibis, 0, ibis->extent);

	for (i=0;i<ibis->nc;i++)
	{
		col = ibis->column[i];
		_i_allocate_space( ibis, col->offset, size[col->format]);
	}
}

static XGAP *find_offset( XIBIS *ibis, int offset )
{
	List *elt;
	XGAP *gap;

	for (elt=ibis->gaps->next; elt; elt=elt->next)
	{
		gap = (XGAP *)elt->value;
		if (offset >= gap->offset && offset < gap->end)
			return (gap);
	}
	
	return (XGAP *) 0;
}

static XGAP *find_size( XIBIS *ibis, int size )
{
	List *elt=ibis->gaps;
	XGAP *gap=(XGAP *) 0;
	
	if (!ibis->gaps) _i_init_space(ibis);

	for (elt=ibis->gaps->next; elt; elt=elt->next)
	{
		gap = (XGAP *)elt->value;
		if (gap->end - gap->offset >= size )
			return (gap);
	}
	
	return (XGAP *) 0;
}



/**
 **  Removes a chunk of free space <offset, size> from gap list
 **/

void _i_allocate_space(XIBIS *ibis, int offset, int size )
{
	XGAP *pos;
	XGAP *new;

	if (!ibis->gaps) _i_init_space(ibis);

	pos = find_offset( ibis, offset );
	
	if (!pos) return; /* not in list */

	if (offset == pos->offset)
	{
		if (offset + size == pos->end)
		  _i_delete_value( ibis->gaps, (list_value) pos );
		else
			pos->offset = offset + size;
	}
	else if (offset + size == pos->end)
	{
		pos->end = offset;
	}
	else
	{
		new = (XGAP *) calloc( 1L, sizeof (XGAP ) );
		new->offset = offset + size;
		new->end = pos->end;
		pos->end = offset;
		
		_i_insert_value( ibis->gaps, (list_value) new );
	}
	
	if (offset+size > ibis->extent)
		ibis->extent = offset+size;
}

/**
 **  Updates list of gaps to include offset & size.
 **  This is the only place that a new gap list is
 **  created.
 **/

void _i_deallocate_space(XIBIS* ibis, int offset, int size )
{
	XGAP *pos;
	XGAP *new;
	
	if (!ibis->gaps) ibis->gaps = _i_new_list( (void(*)(int*))free );

	if ((pos=find_offset( ibis, offset )))
	{
		pos->end = offset + size;
	}
	else if ((pos=find_offset( ibis, offset+size )))
	{
		pos->offset = offset;
	}
	else
	{
		new = (XGAP *) calloc( 1L, sizeof (XGAP ) );
		new->offset = offset;
		new->end = offset+size;
		
		_i_insert_value( ibis->gaps, (list_value) new );
	}
	if (offset + size == ibis->extent)
		ibis->extent = offset;
	
}

/**
 **  Looks through list of gaps in file and finds space
 **
 **  Returns 0 if no such offset found.
 **/

int _i_find_space(XIBIS* ibis, int* offset, int size)
{
	XGAP *pos;
	
	if ((pos = find_size(ibis, size )))
	{		
		*offset = pos->offset;
		return( 1 );
	}
	else return 0;

}
