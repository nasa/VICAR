#include "ibis.h"
#include <string.h>

/**
 **   Implementation of GRAPHICS-1 File I/O.
 **   The only public (protected) routine is _i_install_grfile_methods()
 **/

static int _row_dofile();	/* forward ref to shut up compiler */

static 
void memory_segment_move
(
  char *buffer,
  char *segbuf,
  int oldsegment,
  int newsegment,
  int rows_now
)
{
	char *inptr, *outptr;
	int i, oldinc, newinc;
	int size;
	
	if (oldsegment < newsegment)
	{
		inptr = buffer + (rows_now-1)*oldsegment;
		outptr = buffer + (rows_now-1)*newsegment;
		oldinc = -oldsegment;
		newinc = -newsegment;
		size = oldsegment;
	}
	else
	{
		inptr = buffer;
		outptr = buffer;
		oldinc = oldsegment;
		newinc = newsegment;
		size = newsegment;
	}
	
	/* reset the segments in memory to new locations */

	for (i=0;i<rows_now;i++)
	{
		memcpy( segbuf, inptr, size);
		memcpy( outptr, segbuf, size);
		inptr += oldinc;
		outptr += newinc;
	}
}

static int move_columns( XIBIS *ibis, int srccol, int destcol, int ncols )
{
	int status=1;
	int segment=ibis->segment;
	int rows_now=0;
	int inc;
	int i;
	int row;
	char *buffer=(char *)0;
	char *segbuf=(char *)0;
	char *inptr,*outptr;

	if (srccol==destcol) return 1;

	/* 
	 * determine the number of rows we can process
	 * in memory
	 */
	 
	if (segment > I_TEMP_BUFSIZE)inc = 1;
	else inc = HOW_MANY( I_TEMP_BUFSIZE, segment );
	if (inc > ibis->nr) inc = ibis->nr;

	buffer = (char *)calloc(1L, inc * segment );
	if (!buffer) return IBIS_MEMORY_FAILURE;
	segbuf = (char *)calloc(1L, ncols*4 );
	if (!segbuf)
	{
		status = IBIS_MEMORY_FAILURE;
		goto end;
	}

	for (row=1; row<=ibis->nr; row+=rows_now)
	{
		rows_now = ibis->nr + 1 - row < inc ? ibis->nr + 1 - row : inc;
	
		/* read in rows  */
		status = _row_dofile(ibis, zvread, buffer, row, rows_now, 1);
		if (status != 1) goto end;
		
		/* shift columns to new locations */
		inptr = buffer + 4*(srccol-1);
		outptr = buffer + 4*(destcol-1);
		for (i=0;i<rows_now;i++)
		{
			memcpy( segbuf, inptr, ncols*4);
			memcpy( outptr, segbuf, ncols*4);
			inptr += segment;
			outptr += segment;
		}
	
		/* write out rows */
		status = _row_dofile(ibis, zvwrit, buffer, row, rows_now, 1);
		if (status != 1) 
		{
			/* this is bad news */
			return status;
		}
	}

end:	
	if (buffer) free (buffer);
	if (segbuf) free (segbuf);
	return status;
}

static 	int grow_segments( XIBIS *ibis,  int newsegment)
{
	int status=1;
	int oldsegment=ibis->segment;
	int rows_now=0;
	int inc;
	int row;
	int start_row;
	char *buffer=(char *)0;
	char *segbuf=(char *)0;

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
	segbuf = (char *)calloc(1L, newsegment );
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
		if (status != 1) goto end;

		memory_segment_move(buffer,segbuf,oldsegment,newsegment,rows_now);
		
		/* write out with new segmentation */
		ibis->segment = newsegment;
		status = _row_dofile(ibis, zvwrit, buffer, start_row, rows_now, 1);
		if (status != 1) goto end;
	}
	ibis->segment = newsegment;

end:	
	if (buffer) free (buffer);
	if (segbuf) free (segbuf);
	return status;
}

static 	int shrink_segments( XIBIS *ibis, int newsegment)
{
	int status=1;
	int oldsegment=ibis->segment;
	int rows_now=0;
	int inc;
	int row;
	char *buffer=(char *)0;
	char *segbuf=(char *)0;

	/* 
	 * determine the number of rows we can process
	 * in memory
	 */
	if (oldsegment > I_TEMP_BUFSIZE)
	{
		inc = 1;
	}
	else inc = HOW_MANY( I_TEMP_BUFSIZE, oldsegment );
	if (inc > ibis->nr) inc = ibis->nr;

	buffer = (char *)calloc(1L, inc * oldsegment );
	if (!buffer) return IBIS_MEMORY_FAILURE;
	segbuf = (char *)calloc(1L, oldsegment );
	if (!segbuf)
	{
		status = IBIS_MEMORY_FAILURE;
		goto end;
	}

	for (row=1; row <= ibis->nr; row+=rows_now)
	{
		rows_now = (row + inc > ibis->nr) ?  ibis->nr+1- row: inc;
	
		/* read in with old segmentation */
		ibis->segment = oldsegment;
		status = _row_dofile(ibis, zvread, buffer, row, rows_now, 1);
		if (status != 1) goto end;

		memory_segment_move(buffer,segbuf,oldsegment,newsegment,rows_now);
		
		/* write out with new segmentation */
		ibis->segment = newsegment;
		status = _row_dofile(ibis, zvwrit, buffer, row, rows_now, 1);
		if (status != 1) goto end;
	}
	ibis->segment = newsegment;

end:	
	if (buffer) free (buffer);
	if (segbuf) free (segbuf);
	return status;
}


/* The column routines use this common interface */

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
	int size = ibis->format_size[ FMT_REAL ]; 
	int segment = ibis->segment; 
	int blocksize = ibis->blocksize; 
	int coffset = column*size; 
	int offset = ((srow-1) * segment) + coffset; 
	int end_offset = ((srow+nrows-1) * segment) + coffset; 

	/* loop for each column element */
	for (; offset<end_offset; offset += segment) 
	{ 
		/* 
		 * Grab a single column element. We know
		 * that the element is completely contained
		 * within a single line, since SIZE=4, BLOCKSIZE=512.
		 */

			status = (*function)( unit, buffer, 
				 "line", 1+(offset/blocksize), 
				 "samp", 1+(offset%blocksize), 
				 "nsamps", size, NULL); 
			if (status != 1) return status; 
			if (inc) buffer += size; 
	} 
	
	return status;
}


static int _columnnew(XIBIS *ibis, int column, int ncols, int size)
{
	int newnc = ibis->nc+ncols;
	int status;
	int new_segment;

	/* determine the new offsets and the extent of file */
	
	ibis->extent = newnc*4;
	new_segment =  ibis->extent;
	ibis->numblocks = HOW_MANY( ibis->nr*new_segment, ibis->blocksize);
	
	status = _i_install_numblocks(ibis, "nl", ibis->numblocks);
	if (status !=1) return status;

	status=grow_segments( ibis, new_segment);
	if (status!=1) return status;

	status = move_columns( ibis, column+1, column+1+ncols, ibis->nc - column );

	return status;
}


static int _columndelete(XIBIS *ibis, int column, int ncols)
{
	int newnc = ibis->nc-ncols;
	int status;
	int new_segment;


	status = move_columns( ibis, column+1+ncols, column+1, newnc - column );
	if (status != 1) return status;

	/* determine the new offsets and the extent of file */
	
	ibis->extent = newnc*4;
	new_segment =  ibis->extent;

	status=shrink_segments( ibis, new_segment);
	if (status!=1) return status;

			/* Update numblocks */

	ibis->numblocks = HOW_MANY( ibis->nr*new_segment, ibis->blocksize);
	status = _i_install_numblocks(ibis, "nl", ibis->numblocks);
	if (status !=1) return status;

	return status;
}


static int _record_dofile(
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
	int blocksize = ibis->blocksize; 
	int nc=record->num_cols;
	int top = (record->top_row + mod_row -1);
	int segment=ibis->segment;
	int topoff = top * segment;
	int row,col;
	int offset; 
	int foffsets[MAX_COL];
	int size = ibis->format_size[ FMT_REAL ]; 
	char *buffers[MAX_COL];
	char *tempbuf,*inbuf,*outbuf;
	int cur_off,rows_now,rows_left,row_inc,size_now;
	int writing = (function==zvwrit);
	int have_gaps = (record->flags & FLAG_REC_GAPS);
	
	/* precompute values that don't change for rows */
	for (col=0;col<nc;col++)
	{
		colm = record->column[col];
		foffsets[col] = topoff + (colm->id-1)*size;
		buffers[col] = record->outbuffer[col] + mod_row*size;
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
				/* All elements are 4 bytes so no memcpy */
				*(int *)outbuf = *(int *)inbuf;
				buffers[col] += size;
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
	status = _i_install_numblocks(ibis,"nl",ibis->numblocks);

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
	status = _i_install_numblocks(ibis,"nl",ibis->numblocks);

	return status;
}

/* return size per row needed for row_dofile */

static int _rowsize(XIBIS *ibis)
{
	return ibis->segment;
}



static int _file_init(XIBIS *ibis)
{
	int segment;

	/* figure out the blocksizes, etc */
	ibis->blocksize = OLD_BLKSIZE;
	ibis->recsize = ibis->blocksize;
	ibis->ns = ibis->recsize;
	segment = ibis->nc * 4;
	ibis->segment = segment;
	ibis->numblocks = HOW_MANY( (segment * ibis->nr), OLD_BLKSIZE);
	
	return 1;
}


static int _file_clear( XIBIS *ibis )
{
	return _i_clear_ibis_file(ibis, 1);
}


int _i_install_grfile_methods(XIBIS *ibis )
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

