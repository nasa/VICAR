#include "ibis.h"
#include <string.h>

/**
 **   Implementation of IBIS for Row-Oriented Files
 **   The only public (protected) routine is _i_install_rfile_methods()
 **/

static int _row_dofile();	/* forward ref to shut up compiler */

static 	int grow_segments( XIBIS *ibis, int newsegment)
{
	int status=1;
	int oldsegment=ibis->segment;
	int rows_now=0;
	int inc;
	int row;
	int i;
	int start_row;
	char *buffer=(char *)0;
	char *segbuf=(char *)0;
	char *inptr,*outptr;

	/* 
	 * determine the number of rows we can process
	 * in memory
	 */
	if (newsegment > I_TEMP_BUFSIZE)
	{
		inc = 1;
	}
	else inc = HOW_MANY( I_TEMP_BUFSIZE, newsegment );
	if (inc > ibis->nr) inc = ibis->nr;

	buffer = (char *)calloc(1L, inc * newsegment );
	if (!buffer) return IBIS_MEMORY_FAILURE;
	segbuf = (char *)calloc(1L, oldsegment);
	if (!segbuf)
	{
		status = IBIS_MEMORY_FAILURE;
		goto end;
	}

	for (row=ibis->nr; row > 0; row-=rows_now)
	{
		start_row = row + 1 - inc;
		if (start_row < 1) start_row =1;
		rows_now = row + 1 - start_row;
	
		/* read in with old segmentation */
		ibis->segment = oldsegment;
		status = _row_dofile(ibis, zvread, buffer, start_row, rows_now, 1);
		if (status != 1) return status;
		
		/* spread out segments in memory to new locations */
		inptr = buffer + (rows_now-1)*oldsegment;
		outptr = buffer + (rows_now-1)*newsegment;
		for (i=0;i<rows_now;i++)
		{
			memcpy( segbuf, inptr, oldsegment);
			memcpy( outptr, segbuf, oldsegment);
			inptr -= oldsegment;
			outptr -= newsegment;
		}
		
		/* write out with new segmentation */
		ibis->segment = newsegment;
		status = _row_dofile(ibis, zvwrit, buffer, start_row, rows_now, 1);
		if (status != 1) 
		{
			/* this is bad news */
			return status;
		}
	}

end:	
	ibis->segment = newsegment;
	free (buffer);
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
	int fsize = ibis->format_size[ col->format ]; 
	int size; 
	int segment = ibis->segment; 
	int blocksize = ibis->blocksize; 
	int coffset = col->offset; 
	int offset = ((srow-1) * segment) + coffset; 
	int end_offset = ((srow+nrows-1) * segment) + coffset; 
	int bytes_left, samp,line_left,offset1; 

	/* loop for each column element */
	for (; offset<end_offset; offset += segment) 
	{ 
		/* loop for a single column element */
		offset1=offset;
		for(bytes_left=fsize; bytes_left; bytes_left-=size) 
		{ 
			samp = (offset1%blocksize); 
			line_left = blocksize-samp; 
			size = line_left>bytes_left ? bytes_left : line_left; 
			status = (*function)( unit, buffer, 
				 "line", 1+offset1/blocksize, 
				 "samp", 1+samp, 
				 "nsamps", size, NULL); 
			if (status != 1) return status; 
			if (inc) buffer += size; 
			offset1 += size;
		} 
	} 
	
	return status;
}


static int _columnnew(XIBIS *ibis, int column, int ncols, int size)
{
	XCOL *colm;
	int offset;
	int col;
	int status = 0;

	for (col=0;col<ncols;col++)
	{
		/*
		 * Fill in the gaps, first, and then
		 * extend the file, if need be.
		 */
		colm = ibis->column[column+col];
		if (_i_find_space( ibis, &offset, size ))
		{
			colm->offset = offset;
			_i_allocate_space( ibis, colm->offset , size );
		}
		else break;
	}

	if (col<ncols) /* we ran out of space */
	{
		int new_segment;

		/* determine the new offsets and the extent of file */
		
		_i_compute_new_offsets( ibis, column+col, ncols-col );
				
		/*
		 * We have to shuffle file data around by resetting
		 * the 'segment' property. No need
		 * to call _i_allocate_space here, as no gap entries exist
		 * for these columns. Unless we're lucky enough that
		 * new_segment is a divisor of blocksize, we'll have
		 * to make new_segment a multiple of blocksize.
		 */
			 
		new_segment =  _i_useful_segment( ibis->extent );
		if (ibis->blocksize % new_segment)
			new_segment = ALIGN_UP( new_segment, ibis->blocksize);
		
				/* Update VICAR NL (NS cant be modified) */
	
		ibis->numblocks = HOW_MANY( ibis->nr*new_segment, ibis->blocksize);
		
		status = _i_install_numblocks(ibis,ILABEL_NL,ibis->numblocks);
		if (status !=1) return status;

		status=grow_segments( ibis, new_segment);
		if (status!=1) return status;
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
	XIBIS *ibis=record->ibis;
	XCOL *colm; 
	int status=1; 
	int unit=ibis->unit; 
	int *fsize = ibis->format_size; 
	int blocksize = ibis->blocksize; 
	int nc=record->num_cols;
	int top = (record->top_row + mod_row -1);
	int segment=ibis->segment;
	int topoff = top * segment;
	int row,col;
	int offset; 
	int foffsets[MAX_COL];
	int sizes[MAX_COL];
	char *buffers[MAX_COL];
	char *tempbuf,*inbuf,*outbuf;
	int cur_off,rows_now,rows_left,row_inc,size_now;
	int writing = (function==zvwrit);
	int have_gaps = (record->flags & FLAG_REC_GAPS);
	
	/* precompute values that don't change for rows */
	for (col=0;col<nc;col++)
	{
		colm = record->column[col];
		foffsets[col] = topoff + colm->offset;
		sizes[col] = fsize[ colm->format ];
		buffers[col] = record->outbuffer[col] + mod_row*sizes[col];
	}

	/* Determine buffer setup */	
	row_inc = HOW_MANY( I_TEMP_BUFSIZE, segment );
	if (row_inc > nrows) row_inc = nrows;
	tempbuf = (char *)calloc(1L, row_inc * segment);
	if (!tempbuf) return IBIS_MEMORY_FAILURE;

	cur_off = topoff;
	for (rows_left=nrows;rows_left;rows_left-=rows_now)
	{
		rows_now = rows_left > row_inc? row_inc : rows_left;
		size_now = rows_now*segment;		
		if (!writing || have_gaps)
		{
			status = _i_process_contiguous(unit,zvread,tempbuf,cur_off,
			  size_now,blocksize,ibis->recsize,1);
			if (status !=1) goto end;
		}
		
		/* Pure Memory Transfer from/to temporary buffer */
		for (row=0; row<rows_now; row++)
		{
			/* loop for a single row of record */
			for (col=0; col<nc; col++) 
			{ 
				/* Transfer a single column element */
				offset=foffsets[col] - cur_off;
				inbuf = writing ? buffers[col] : tempbuf+offset;
				outbuf = writing ? tempbuf+offset: buffers[col];
				memcpy(outbuf, inbuf,sizes[col]);
				buffers[col] += sizes[col];
				foffsets[col] += segment;
			} 
		}
		if (writing)
		{
			status = _i_process_contiguous(unit,zvwrit,tempbuf,cur_off,
			 size_now,blocksize,ibis->recsize,1);
			if (status !=1) goto end;
		}
		cur_off += size_now;
	}

end:
	free (tempbuf);	
	return status;
}


static int _row_dofile
(
  XIBIS *ibis,
  int (*function)(int, void*, ...),
  char *buffer,
  int srow,
  int nrows,
  int inc
)
{
	int status=1;
	int unit=ibis->unit;
	int segment=ibis->segment;
	int blocksize=ibis->blocksize;
	int offset=(segment*(srow-1));


        status = _i_process_contiguous(unit,function,buffer,offset,
           nrows*segment,blocksize,ibis->recsize,inc);
	
	return status;
}


/*
 * Allocate space for <nrows> rows. We don't need
 * to shuffle any data for row-oriented files.
 */

static int _rownew(XIBIS *ibis, int nrows)
{
	int status;
	int size = (ibis->nr + nrows)*ibis->segment;

	ibis->numblocks = HOW_MANY( size, ibis->blocksize);	
	status = _i_install_numblocks(ibis,ILABEL_NL,ibis->numblocks);

	return status;
}

/*
 * Deallocate space for <nrows> rows. We don't
 * need to shuffle data for row-oriented files.
 */


static int _rowdelete(XIBIS *ibis, int nrows)
{
	int status;
	int size = (ibis->nr - nrows)*ibis->segment;

	ibis->numblocks = HOW_MANY( size, ibis->blocksize);	
	status = _i_install_numblocks(ibis,ILABEL_NL,ibis->numblocks);

	return status;
}

/* return size per row needed for row_dofile */

static int _rowsize(XIBIS *ibis)
{
	return ibis->segment;
}



/**
 **  This is the routine that sets up file blocking
 **  and offset parameters for a new IBIS file
 **  organized by rows.
 **/


static int _file_init(XIBIS *ibis)
{
	/* For row files, we compute things based on column extent */

	_i_compute_blocksize( ibis,  ibis->extent );
	ibis->numblocks = HOW_MANY( ibis->nr*ibis->segment, ibis->blocksize);	
	if (!ibis->numblocks) ibis->numblocks = 1;
	
	return 1;
}

static int _file_clear( XIBIS *ibis )
{
	return _i_clear_ibis_file(ibis, 1);
}


void _i_install_rfile_methods(XIBIS* ibis )
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
