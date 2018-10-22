#include "ibis.h"
#include <string.h>
#include "ibisdeclares.h"

XCOL *_i_new_column(void)
{
	XCOL *column;
	column = (XCOL *)calloc( 1L, sizeof(XCOL) );
	return (column);
}

void _i_free_column(XCOL *col)
{
	if (!col) return;
	if (col->trans) free( col->trans );
	free( col );
}

int _i_init_column_array(XIBIS *ibis )
{
	int status = 1;
	int col;
	XCOL *column;

	for (col=0;col<ibis->nc;col++)
	{
		column = _i_new_column();
		if (!column) goto failure; 
		column->id = col+1;
		ibis->column[col] = column;
	}
	
	return status;

failure:

	return (IBIS_MEMORY_FAILURE);
}

int _i_insert_column_pointers(XIBIS *ibis, int col, int ncol )
{
	int status=1;
	int nc;
	int i;
	XCOL *column;

	nc = ibis->nc - col; /* number to move up */

	_i_move_column_pointers( ibis, col, col+ncol, nc );
	
	/* Insert new columns and renumber */
	for (i=0; i< ncol; i++)
	{
		column = _i_new_column();
		if (!column) goto failure;
		column->id = col+i+1;
		ibis->column[col+i] = column;
	}

	return status;
	
failure:

	/* free up the new columns */
	for (i--; i>=0; i--)
		_i_free_column(ibis->column[col+i]);

	/* put everything back where we got it */
	_i_move_column_pointers( ibis, col+ncol, col, nc );
	
	return (IBIS_MEMORY_FAILURE);
}


/*
 * Remove all IBIS references to columns. 
 */

int _i_delete_column_pointers(XIBIS *ibis, int col, int ncol )
{
	int nc;
	int i;
	

	/* delete loop */
	for (i=0; i< ncol; i++)
	{
		if (ibis->column[col+i])
			_i_free_column( ibis->column[col+i] );
	}
	
	/*  Move the pointers back down */
	
	nc = ibis->nc  - (col+ncol);
	_i_move_column_pointers( ibis, col+ncol, col, nc );

	return 1;
}

/*
 * move columns starting at <srccol> to a location
 * starting at <destcol>, and renumber them.
 */

int _i_move_column_pointers(XIBIS * ibis, int srccol, int destcol, int ncol)
{
	int i;
	XCOL *buffer[MAX_COL];
	
	if (srccol==destcol) return 1;

	/*  Move the pointers  */
	
	if (ncol>0)
	{
		memcpy( buffer, ibis->column+srccol, sizeof( XCOL* ) * ncol );
		memcpy( ibis->column+destcol, buffer, sizeof( XCOL* ) * ncol );
	}

	/* renumber the moved columns */

	for (i=0; i< ncol; i++)
		ibis->column[destcol+i]->id = destcol+i+1;
	
	return 1;
}

void _i_free_col_array(XIBIS *ibis )
{
	int col;
	
	for (col=0;col<ibis->nc;col++)
	{
		if (ibis->column[col])
		{
			_i_free_column( ibis->column[col] );
			ibis->column[col]=(XCOL*)0;
		}
	}
	
}


int _i_attach_format_to_column(XIBIS *ibis,int fmt, int col)
{
	char *fmt_name;
	XCOL *colm=ibis->column[ col ];
	
	colm->format = fmt;
	
	if (fmt>FMT_ASCII) fmt=FMT_ASCII; /* only generic ASCII accepted */
	fmt_name = format_name[ fmt ];

	if (colm->trans) /* clear the old translation */
	{
		free( colm->trans );
		colm->trans = (trans_type *)0;
	}
	
	return (_i_attach_column_to_group( ibis->formats, fmt_name, colm )); 
}

int _i_attach_column_to_group(List* list,char *grp, XCOL *col)
{
	XGROUP *group;
	
	if (!(group = _i_request_group(list,grp)))
		return 0;

	/* add column to group's list */
	_i_append_value( group->columns, (list_value)col );

	return (1);
}


int _i_detach_col_from_format(XIBIS* ibis, int col)
{
	int fmt;
	XCOL *colm=ibis->column[ col ];
	XGROUP *fgroup;
	
	fmt = colm->format;
	if (fmt>FMT_ASCII) fmt=FMT_ASCII;
 
    fgroup = _i_find_group( ibis->formats, format_name[ fmt ] );
	if (!fgroup) return IBIS_INVALID_FORMAT;

	if (colm->trans)	/* revert to default translation */
	{
		free(colm->trans);
		colm->trans = (trans_type*)0;
	}

	_i_delete_value( fgroup->columns, (list_value) colm);
	
	return 1; 
}

int _i_detach_column_from_list(XIBIS *ibis,List* list,int col)
{
	List *ent;
	XGROUP *group;
	XCOL *colm=ibis->column[ col ];

	if (!list) return 0;
	
	for (ent=list->next; ent; ent=ent->next)
	{
		group = (XGROUP*)ent->value;
		_i_delete_value( group->columns, (list_value) colm);
	}
	return 1; 
}



