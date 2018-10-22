#include "ibis.h"

/************************************************************************/
/* C-Language Interface							*/
/************************************************************************/

#define _RBUFSIZE 200000

int IBISRowClear(int ibis_id,int srow,int nrows)
{
	XIBIS *ibis=IBIS_FETCH_ID(ibis_id);
	int status;
	char *buffer=(char *)0;
   	_ibis_current_module="IBISRowClear";
  	
	CHECK_WRITE( ibis );
	
	if (ibis->record)
	{
		status=_i_notify_records(ibis, NOTICE_NEED_ROWS, srow, srow+nrows-1);
		if (status != 1) return status;
		status=_i_notify_records(ibis, NOTICE_FLUSHING_ROWS, srow, srow+nrows-1);
		if (status != 1) return status;
	}
	
	/* allocate memory for clearing rows */
	buffer = (char *)calloc( 1L, ibis->recsize);
	if (!buffer) return IBIS_MEMORY_FAILURE;

	/* formally clear the rows using dofile */
	status = ibis->rowmethod->dofile(ibis, zvwrit, buffer, srow, nrows, 0);
	if (status != 1) goto end;
	
end:
	if (buffer) free (buffer);
	return status;
}


int IBISRowNew(int ibis_id,int srow,int nrows)
{
	XIBIS *ibis=IBIS_FETCH_ID(ibis_id);
	int status=1;
	int inc;
	int row, rows_now=0;
	int inrow;
	int size;
	char *buffer=(char *)0;
  	_ibis_current_module="IBISRowNew";
  	
	CHECK_WRITE( ibis );

	if (!srow) srow = ibis->nr+1;

	if (ibis->record)
	{
		status=_i_notify_records(ibis, NOTICE_NEED_ROWS, srow, ibis->nr);
		if (status != 1) return status;
		status=_i_notify_records(ibis, NOTICE_FLUSHING_ROWS, srow, ibis->nr);
		if (status != 1) return status;
	}
	
	/* let the file methods allocate file space */
	status = ibis->rowmethod->new( ibis, nrows );
	if (status != 1) return status;
		
	/* determine if we need to allocate memory for copying rows */
	size = ibis->rowmethod->getsize(ibis);
	inc = HOW_MANY(_RBUFSIZE, size );
	if (inc>ibis->nr +1 -srow) inc=ibis->nr+1-srow;
	if (inc > 0) 
	{
	   buffer = (char *)calloc( 1L, inc * size);
	   if (!buffer) return IBIS_MEMORY_FAILURE;
	}

	/* formally copy some rows down <nrows> using dofile */
	for (row = ibis->nr; row >=srow; row-=rows_now)
	{
		rows_now = (inc > row+1-srow ) ?  row+1-srow : inc;
		inrow = row+1-rows_now;
		status = ibis->rowmethod->dofile(ibis, zvread,
					 buffer, inrow, rows_now, 1);
		if (status != 1) goto end;
		status = ibis->rowmethod->dofile(ibis, zvwrit,
					 buffer, inrow+nrows, rows_now, 1);
		if (status != 1) goto end;
	}
	
	ibis->nr += nrows;
	ibis->flags |= FLAG_MOD_LABELS;
	
	if (ibis->flags & FLAG_AUTO_INIT)
		status = IBISRowClear(ibis_id,srow,nrows);
end:
	if (buffer) free (buffer);
	return status;
}


int IBISRowDelete(int ibis_id,int srow,int nrows)
{
	XIBIS *ibis=IBIS_FETCH_ID(ibis_id);
	int status;
	int inc;
	int size;
	int row, rows_now=0;
	char *buffer=(char *)0;
  	_ibis_current_module="IBISRowDelete";
  	
	CHECK_WRITE( ibis );

	if (ibis->record)
	{
		status=_i_notify_records(ibis, NOTICE_NEED_ROWS, srow, ibis->nr);
		if (status != 1) return status;
		status=_i_notify_records(ibis, NOTICE_FLUSHING_ROWS, srow, ibis->nr);
		if (status != 1) return status;
	}

	/* determine if we need to allocate memory for copying rows */
	size = ibis->rowmethod->getsize(ibis);
	inc = HOW_MANY(_RBUFSIZE, size );
	if (srow + nrows + inc > ibis->nr) inc=ibis->nr + 1 -(srow+nrows);
	if (inc > 0)
	{
	   buffer = (char *)calloc( 1L, inc * size);
	   if (!buffer) return IBIS_MEMORY_FAILURE;
	}
	
	/* formally copy the rows up <nrows> using dofile */
	for (row = srow+nrows; row <= ibis->nr; row+=rows_now)
	{
		rows_now = (row + inc <= ibis->nr) ? inc : ibis->nr + 1 - row;
		status = ibis->rowmethod->dofile(ibis, zvread,
					 buffer, row, rows_now, 1);
		if (status != 1) goto end;
		status = ibis->rowmethod->dofile(ibis, zvwrit,
					 buffer, row-nrows, rows_now, 1);
		if (status != 1) goto end;
	}
			
	/* let the file methods deallocate file space */
	status = ibis->rowmethod->delete( ibis, nrows );
	if (status != 1) return status;		
	
	ibis->nr -= nrows;
	ibis->flags |= FLAG_MOD_LABELS;

end:
	if (buffer) free(buffer);	
	return status;
}



/************************************************************************/
/* Fortran-Callable Versions						*/
/************************************************************************/

void FTN_NAME2_(ibis_row_clear, IBIS_ROW_CLEAR) (int *ibis_id, int *row,
		int *nrows, int *status)
{
   *status = IBISRowClear(*ibis_id, *row, *nrows);
   return;
}

void FTN_NAME2_(ibis_row_new, IBIS_ROW_NEW) (int *ibis_id, int *row,
		int *nrows, int *status)
{
   *status = IBISRowNew(*ibis_id, *row, *nrows);
   return;
}

void FTN_NAME2_(ibis_row_delete, IBIS_ROW_DELETE) (int *ibis_id, int *row,
		int *nrows, int *status)
{
   *status = IBISRowDelete(*ibis_id, *row, *nrows);
   return;
}
