#include "ibis.h"

/**
 **   Implementation of IBIS-1 File I/O.
 **   The only public (protected) routine is _i_install_ofile_methods()
 **/

static 	int grow_segments( XIBIS *ibis,  int newsegment)
{
	int status=1;
	int oldsegment=ibis->segment;
	int rows_now=0;
	int inrow;
	int inc;
	int row;
	int i;
	char *buffer=(char *)0;
	XCOL *col;
	XMethod col_dofile=ibis->colmethod->dofile;


	/* 
	 * determine the number of rows we can process
	 * in memory
	 */
	inc = HOW_MANY( I_TEMP_BUFSIZE, 4 );
	if (inc > ibis->nr) inc = ibis->nr;
	buffer = (char *)calloc(1L, inc*4 );
	if (!buffer)
	{
		 status = IBIS_MEMORY_FAILURE;
		 goto end;
	}

	/* scan backwards through the file, copying columns as we go */
	for (i=ibis->nc-1; i>=0; i--)
	{		
		col = ibis->column[ i ];
		
		for (row=ibis->nr; row > 0; row-=rows_now)
		{
			rows_now = row>inc ? inc : row;
			inrow = row+1-rows_now;
		
			/* read in with old segmentation */
			ibis->segment = oldsegment;
			status = col_dofile(ibis, zvread, buffer, i, inrow, rows_now, 1);
			if (status != 1) goto end;
			
			/* write out with new segmentation */
			ibis->segment = newsegment;
			status = col_dofile(ibis, zvwrit, buffer, i, inrow, rows_now, 1);
			if (status != 1) goto end;
		}
	}

	ibis->segment = newsegment;
end:	
	if (buffer) free (buffer);
	return status;
}

static 	int shrink_segments( XIBIS *ibis,  int newsegment)
{
	int status=1;
	int oldsegment=ibis->segment;
	int rows_now=0;
	int inc;
	int row;
	int i;
	char *buffer=(char *)0;
	XCOL *col;
	XMethod col_dofile=ibis->colmethod->dofile;


	/* 
	 * determine the number of rows we can process
	 * in memory
	 */
	inc = HOW_MANY( I_TEMP_BUFSIZE, 4 );
	if (inc > ibis->nr) inc = ibis->nr;
	buffer = (char *)calloc(1L, inc*4 );
	if (!buffer)
	{
		 status = IBIS_MEMORY_FAILURE;
		 goto end;
	}

	/* scan forwards through the file, copying columns as we go */
	for (i=0; i<ibis->nc; i++)
	{		
		col = ibis->column[ i ];
		
		for (row=1; row <= ibis->nr; row+=rows_now)
		{
			rows_now = (row + inc > ibis->nr) ?  ibis->nr+1- row: inc;
		
			/* read in with old segmentation */
			ibis->segment = oldsegment;
			status = col_dofile(ibis, zvread, buffer, i, row, rows_now, 1);
			if (status != 1) goto end;
			
			/* write out with new segmentation */
			ibis->segment = newsegment;
			status = col_dofile(ibis, zvwrit, buffer, i, row, rows_now, 1);
			if (status != 1) goto end;
		}
	}

	ibis->segment = newsegment;
end:	
	if (buffer) free (buffer);
	return status;
}



/* 
 *  Move ncols columns from column srccol to column destcol.
 */

static int move_columns( XIBIS *ibis, int srccol, int destcol, int ncols)
{
	int status=1;
	int unit=ibis->unit;
	int inc;
	int block;
	int blocks_per_col=(ibis->segment*4)/ibis->blocksize;
	int srcblock;
	int destblock;
	int nblocks=ncols*blocks_per_col;
	char *blockbuf;
	
	if (srccol<destcol) /* go backwards */
	{		
		inc = -1;
		srcblock=(srccol+ncols-1)*blocks_per_col+1;
		destblock=(destcol+ncols-1)*blocks_per_col+1;
	}
	else if (srccol > destcol) /* go forwards */
	{
		inc=1;
		srcblock=(srccol-1)*blocks_per_col+2;
		destblock=(destcol-1)*blocks_per_col+2;
	}
	else return 1; /* src==dest */

	
	blockbuf= (char *)malloc( ibis->blocksize);
	if (!blockbuf) return IBIS_MEMORY_FAILURE;

	/* transfer the blocks */
	for (block = 0; block<nblocks; block++)
	{
		status = zvread( unit, blockbuf, "line", srcblock, NULL);
		if (status !=1) break;
		status = zvwrit( unit, blockbuf, "line", destblock, NULL);
		if (status !=1) break;
		srcblock += inc;
		destblock += inc;
	}
	
	free (blockbuf);
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
	int offset = OLD_BLKSIZE+((column* ibis->segment) + (srow - 1))*4; 


        status=_i_process_contiguous(unit,function,buffer,offset,
                4*nrows,OLD_BLKSIZE,OLD_BLKSIZE,inc);

	return status;

}

static int _columnnew(XIBIS *ibis, int column, int ncols, int size)
{
	int status; 
	int newnc = ibis->nc+ncols;
	int blocks_per_col=(ibis->segment*4)/ibis->blocksize;
	
	/* 
	 * update the file parameters first, as we'll be
	 * writing to new places in file.
	 */

	ibis->numblocks = 1 + newnc * blocks_per_col;
	status = _i_install_numblocks(ibis,"nl",ibis->numblocks);
	if (status !=1) return status;

	status = move_columns( ibis, column+1, column+1+ncols, ibis->nc - column );
	
	return status;
}


static int _columndelete(XIBIS *ibis, int column, int ncols)
{
	int status; 
	int newnc = ibis->nc-ncols;
	int blocks_per_col=(ibis->segment*4)/ibis->blocksize;
	ibis->numblocks = 1 + newnc * blocks_per_col;
	
	status = move_columns( ibis, column+1+ncols, column+1, newnc - column );
	if (status != 1) return status;

	status = _i_install_numblocks(ibis,"nl",ibis->numblocks);
	if (status !=1) return status;
		
	return status;
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
	int offset = 4*mod_row;
	int srow = record->top_row + mod_row;
	int ncols=record->num_cols;
	char **bufptr;
	XIBIS *ibis=record->ibis;
	
	bufptr = record->outbuffer;
	for (col=0; col<ncols; col++)
	{
		status = _column_dofile( ibis, function, 
				bufptr[col] + offset,
				record->column[col]->id-1, srow, nrows,  1);
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
 		status = col_dofile( ibis, function, buffer,
				 col, srow, nrows,  inc);
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
	int blocks_per_col;
	int segblock = OLD_BLKSIZE/4;
	
	/* work out the new segment size */

	blocks_per_col = HOW_MANY((ibis->nr+nrows), segblock);
	new_segment = blocks_per_col * segblock ;
	if (new_segment==ibis->segment) return 1; /* no need to resize */

	/* Update blocksize */

	ibis->numblocks = 1 + ibis->nc * blocks_per_col;
	status = _i_install_numblocks(ibis,"nl",ibis->numblocks);
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
	int status=1;
	int new_segment;
	int blocks_per_col;
	int segblock = OLD_BLKSIZE/4;
	
	/* work out the new segment size */

	blocks_per_col = HOW_MANY((ibis->nr-nrows), segblock);
	new_segment = blocks_per_col * segblock ;
	if (new_segment==ibis->segment) return 1; /* no need to resize */

	/* shuffle the data around */

	status=shrink_segments( ibis, new_segment);
	if (status!=1) return status;
	
	/* Update blocksize */

	ibis->numblocks = 1 + ibis->nc * blocks_per_col;
	status = _i_install_numblocks(ibis,"nl",ibis->numblocks);
	if (status !=1) return status;
	
	return status;
}

/* return size per row needed for row_dofile */

static int _rowsize(XIBIS *ibis)
{
	return ibis->nc * 4;
}



static int _file_init(XIBIS *ibis)
{
	int blocks_per_col;
	int segblock = OLD_BLKSIZE/4;

	/* figure out the blocksizes, etc */
	ibis->blocksize = OLD_BLKSIZE;
	ibis->recsize = ibis->blocksize;
	blocks_per_col = HOW_MANY(ibis->nr, segblock);
	ibis->segment = blocks_per_col * segblock ;
	ibis->numblocks = 1 + ibis->nc * blocks_per_col;
	
	return 1;
}

static int _file_clear( XIBIS *ibis )
{
	return _i_clear_ibis_file(ibis, 2);
}

int _i_install_ofile_methods(XIBIS* ibis )
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
	
	return 1;
}
