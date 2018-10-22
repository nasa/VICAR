#include "ibis.h"

/**
 **   Implementation of IBIS-2 I/O for Column-Oriented Files
 **   The only visible routine is _i_install_cfile_methods()
 **/

static 	int grow_segments( XIBIS *ibis,  int newsegment)
{
	int status=1;
	int oldsegment=ibis->segment;
	int rows_now=0;
	int inrow;
	int inc;
	int row;
	int size;
	int *index=(int *)0;
	int i;
	char *buffer=(char *)0;
	XCOL *col;
	XMethod col_dofile=ibis->colmethod->dofile;

	/* create an offset index to column at location */
	index = (int *)calloc(1L, ibis->extent*sizeof(int));
	if (!index) return IBIS_MEMORY_FAILURE;
	for (i=0;i<ibis->nc;i++)
		index[ ibis->column[i]->offset ] = i;

	buffer = (char *)calloc(1L, I_TEMP_BUFSIZE );
	if (!buffer)
	{
		 status = IBIS_MEMORY_FAILURE;
		 goto end;
	}

	/* scan backwards through the file, copying columns as we go */

	for (i=ibis->extent-1; i>=0; i--)
	{
		if (!index[i]) continue;
		
		col = ibis->column[ index[i] ];
		size = ibis->format_size[ col->format ];
		
		/* 
		 * determine the number of rows we can process
		 * in memory
		 */
		inc = HOW_MANY( I_TEMP_BUFSIZE, size );
		if (inc > ibis->nr) inc = ibis->nr;
		
		for (row=ibis->nr; row > 0; row-=rows_now)
		{
			rows_now = row>inc ? inc : row;
			inrow = row+1-rows_now;
		
			/* read in with old segmentation */
			ibis->segment = oldsegment;
			status = col_dofile(ibis, zvread, buffer, index[i], inrow, rows_now, 1);
			if (status != 1) goto end;
			
			/* write out with new segmentation */
			ibis->segment = newsegment;
			status = col_dofile(ibis, zvwrit, buffer, index[i], inrow, rows_now, 1);
			if (status != 1) goto end;
		}
	}

	ibis->segment = newsegment;
end:	
	if (index) free (index );
	if (buffer) free (buffer);
	return status;
}

static int _column_dofile
(
  XIBIS *ibis,
  int (*function)(int, void*, ...), /* zvwrit or zvread */
  char *buffer,
  int column,
  int srow,
  int nrows,
  int inc
)
{
	int status=1; 
	int unit=ibis->unit; 
	XCOL *col=ibis->column[column]; 
	int fsize=ibis->format_size[col->format]; 
	int blocksize = ibis->blocksize; 
	int offset= ((srow-1) * fsize) + col->offset*ibis->segment; 

	status = _i_process_contiguous(unit,function,buffer,offset,
		fsize*nrows,blocksize,ibis->recsize,inc);
	
	return status;
}


static int _columnnew(XIBIS *ibis, int column, int ncols, int size)
{
	XCOL *colm;
	int offset;
	int col;
	int status=1;

	for (col=0;col<ncols;col++)
	{
		colm = ibis->column[column+col];
		if (_i_find_space( ibis, &offset, size ))
		{
			colm->offset = offset;
			_i_allocate_space( ibis, colm->offset , size );
		}
		else break;
	}
	
	if (col<ncols) /* ran out of space */
	{
		_i_compute_new_offsets( ibis, column+col, ncols-col );
		ibis->numblocks = HOW_MANY(ibis->extent * ibis->segment, ibis->blocksize);
	
		/* Update System Label */
		status = _i_install_numblocks(ibis, ILABEL_NL, ibis->numblocks);
		if (status !=1) return status;
	}
	
	return status;
}

static int _columndelete(XIBIS *ibis, int column, int ncols)
{
	XCOL **colm = ibis->column +column;
	XCOL *colp;
	int i;

	/*
	 *  We dont need to move file contents; just let the
	 *  Gap Manager keep track of available file space.
	 */
	
	for (i=0; i<ncols; i++,colm++)
	{
		colp= *colm;
		_i_deallocate_space( ibis, colp->offset,
			 ibis->format_size[ colp->format ] );
	}
	return 1;
}



static int _record_dofile
(
  XREC *record,
  int (*function)(int, void*, ...), /* zvwrit or zvread */
  int mod_row,
  int nrows
)
{
	int status=1;
	int col;
	int srow = record->top_row + mod_row;
	int ncols=record->num_cols;
	XIBIS *ibis=record->ibis;
	int *size = ibis->format_size;
	XCOL *colm;
	char **bufptr;
	
	bufptr = record->outbuffer;
	for (col=0; col<ncols; col++)
	{
		colm = record->column[col];
		status = _column_dofile( ibis, function, 
				bufptr[col] + size[ colm->format ] * mod_row,
				colm->id-1, srow, nrows,  1);
		if (status != 1) return status;
	}
	
	return status;
}

/*
 * Iteration loop for rows. This is only used externally for
 * clearing and transferring data, so we can effectively use
 * columns here and not ever tell the external routines what we did.
 */

static int _row_dofile
(
  XIBIS *ibis,
  int (*function)(int, void*, ...), /* zvwrit or zvread */
  char *buffer,
  int srow,
  int nrows,
  int inc
)
{
	int status=1;
	int col;
	int ncols=ibis->nc;
	int *size = ibis->format_size;
	XMethod col_dofile=ibis->colmethod->dofile;

 	for (col=0;col<ncols;col++)
 	{
 		status = col_dofile( ibis, function, buffer, col, srow, nrows,  inc);
		if (status!=1) return status;
		if (inc) buffer += nrows*size[ibis->column[col]->format];
 	}
 	
	return status;
}


/*
 * Allocate space for <nrows> rows
 */

static int _rownew(XIBIS *ibis, int nrows)
{
	int status=1;
	int new_segment;

	new_segment =  _i_useful_segment(ibis->nr + nrows);
	
	if (new_segment==ibis->segment) return 1; /* no need to resize */

	if (ibis->blocksize % new_segment)
		new_segment = ALIGN_UP( new_segment, ibis->blocksize);
	
			/* Update blocksize */

	ibis->numblocks = HOW_MANY(ibis->extent * new_segment, ibis->blocksize);		
	status = _i_install_numblocks(ibis,ILABEL_NL,ibis->numblocks);
	if (status !=1) return status;
	
	/* shuffle the data around */
	status=grow_segments( ibis, new_segment);
	if (status!=1) return status;
	
	return status;
}

/*
 * Deallocate space for <nrows> rows
 */


static int _rowdelete(XIBIS *ibis, int nrows)
{
	/* not necessary to do anything */

	return 1;
}

/* return size per row needed for row_dofile */

static int _rowsize(XIBIS *ibis)
{
	return ibis->extent;
}


/**
 **  This is the routine that sets up file blocking
 **  and offset parameters for a new IBIS file
 **  organized by columns.
 **/

static int _file_init(XIBIS *ibis)
{	
	/* For column files, we compute things based on number of rows */
	
	_i_compute_blocksize(  ibis, ibis->nr );
	ibis->numblocks = HOW_MANY(ibis->extent * ibis->segment, ibis->blocksize);
	if (!ibis->numblocks) ibis->numblocks = 1;

	return 1;
}

static int _file_clear( XIBIS *ibis )
{
	return _i_clear_ibis_file(ibis, 1);
}


void _i_install_cfile_methods(XIBIS* ibis )
{
	ibis->colmethod->dofile = _column_dofile;
	ibis->colmethod->new = _columnnew;
	ibis->colmethod->delete = _columndelete;
	
	ibis->recmethod->dofile = _record_dofile;

	ibis->rowmethod->dofile = _row_dofile;
	ibis->rowmethod->new = _rownew;
	ibis->rowmethod->delete = _rowdelete;
	ibis->rowmethod->getsize = _rowsize;

	ibis->filemethod->init = _file_init;
	ibis->filemethod->clear = _file_clear;
}
