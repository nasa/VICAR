/* 
 *  util_file.c  -- common utility for setting up
 *  segments and blocksizes, etc. These routines
 *  happen to be used by both the mthd_io_row and mthd_io_col
 *  file methods, but with different lower_bounds.
 */

#include "ibis.h"

/**
 **  These are routines that set up file blocking
 **  and offset parameters for IBIS-2 files.
 **/

int _i_compute_blocksize(XIBIS* ibis, int lower_bound )
{
	int recsize=ibis->recsize;
	
	if (lower_bound <= 0) lower_bound=1;
	
	
	/*
	 *  Define a reasonable setup. Depending on the record
	 *  size, either blocksize will be a multiple of segment,
	 *  or segment will be a multiple of blocksize. And segment
	 *  is large enough to contain the lower_bound bytes.
	 *
	 *  ibis->recsize will only be non-zero if the PIX_NS
	 *  has been set. Otherwise, we have byte data as usual.
	 */
	
	if (!ibis->ns)
	{
		 recsize=NEW_BLKSIZE;
		ibis->recsize = recsize;
		ibis->ns = ibis->recsize / ibis->format_size[ ibis->pix_fmt ];
	}
	else recsize = ibis->recsize;
	
	ibis->recsize = recsize;

	if (lower_bound < recsize) /* blocksize is a mult of segment */
	{
		lower_bound = _i_useful_segment( lower_bound );
		ibis->segment = lower_bound;
		ibis->blocksize = (recsize/lower_bound)*lower_bound;
	}
	else	/* segment is a multiple of blocksize */
	{
		ibis->blocksize = recsize;
		ibis->segment = ALIGN_UP( lower_bound, recsize );
	}
	
	return 1;
}

/*
 *  Prior to creating a file, if the NS or image FORMAT
 *  is set, the recsize will be implicitly defined. compute
 *  its value here.
 */

int _i_reset_recsize(XIBIS* ibis)
{
	int pixsize, status=1;
	char intfmt[20],realfmt[20];
	
	if (!ibis->ns)
	{
		ibis->recsize = 0;
		return status;
	}
	
	status=zvhost( ibis->pix_host, intfmt, realfmt );
	if (status!=1) return status;
	status=zvpixsize( &pixsize, format_name[ ibis->pix_fmt ],
					intfmt, realfmt ); 
	if (status!=1) return status;
	
	ibis->recsize = pixsize * ibis->ns;
	
	return status;
}

int _i_useful_segment(int lower_bound )
{
	int bound=1;
	
	/* return first power of 2 not smaller than lower_bound */

	lower_bound--;
	while (lower_bound)
	{
		lower_bound = lower_bound >> 1;
		bound = bound << 1;
	}
	
	return bound;
}

void _i_compute_new_offsets(XIBIS* ibis,int scol,int ncol)
{
	int i;
	int csize,offset=ibis->extent;
	XCOL *col;

	/*
	 * compute the offsets, starting from current extent.
	 *
	 */
	
	for (i=scol; i<scol+ncol; i++)
	{
		col = ibis->column[i];
		col->offset = offset;
		csize = format_size[ col->format ];
		offset += csize;
	}
	ibis->extent = offset;
	
	return;
}

/* Access method for VICAR i/o which tries to
 * avoid as much paired-optional parsing as possible.
 */

int _i_process_contiguous
(
 int unit,
 int (*function)(int, void*, ...), /* zvwrit or zvread */
 char *buffer,
 int offset,
 int nbytes,
 int blksize, /* IBIS blocksize */
 int recsize, /* physical VICAR recsize */
 int inc /* increment buffer or not ? */
)
{
	int status=1; 
	int size=blksize; 
	int samp,line_left;
	int bytes_left=nbytes;
	int line=1+offset/blksize;
	int contiguous=(blksize==recsize);

	/* special if not at start of block */
	if (offset%blksize)
	{
	  samp = (offset%blksize); 
	  line_left = blksize-samp; 
	  size = line_left>bytes_left ? bytes_left : line_left; 

/* Don't call zvwrite or zvread with 0 nsamps, this defaults to
   read/write a whole row, which will likely stomp on the passed in
   buffer. Since there is nothing to read/write, there is no reason to
   call this anyways.
*/

	  if (size !=0) {
	    status = (*function)( unit, buffer, 
				  "line", line,
				  "samp", 1+samp, 
				  "nsamps", size, NULL); 
	    if (status != 1) return status; 
	  }
	  if (inc) buffer += size;
	  bytes_left-=size;
	  line++;
	} 
	else if (bytes_left >= blksize)
	{
	    if (contiguous)
		status = (*function)( unit, buffer,"line", line, NULL);
	    else
		status = (*function)( unit, buffer,"line", line,
						"nsamps",blksize, NULL);
	   if (status != 1) return status; 
	   if (inc) buffer += size;
	   bytes_left-=size;
	   line++;
	}


	/* From this point, we are block-aligned; read whole lines */
	for(; bytes_left>=blksize; bytes_left-=blksize)
	{
		if (!contiguous)
		    status = (*function)( unit, buffer,"line", line,
						"nsamps",blksize, NULL);
		else if (function==zvwrit)
		    status = (*function)( unit, buffer,"line", line,NULL);
		else
		    status = (*function)( unit, buffer,NULL);
		if (status != 1) return status; 
		if (inc) buffer += blksize;
		line++;
	}

	/* If any leftovers, read here */

	if (bytes_left > 0)
	{
		  status = (*function)( unit, buffer,"line", line,
			"nsamps",bytes_left, NULL); 
	}
	
	return status;
}

int _i_clear_ibis_file(XIBIS* ibis, int sblock)
{
	char *buffer;
	int status=1;
	int unit=ibis->unit;
	int blocksize=ibis->blocksize;
	int block, nblock=ibis->numblocks;

	/* clear the file */
	buffer = (char *)calloc(1L, ibis->blocksize);
	if (!buffer) return IBIS_MEMORY_FAILURE;
	
	for (block=sblock;block<=nblock;block++)
	{
		status = zvwrit( unit, buffer, "line", block, "nsamps",blocksize, NULL);
		if (status!=1) break;
	}

	free (buffer);
	return status;	
}

int _i_install_numblocks(XIBIS* ibis, char* label /* "NL" or "NLB" */,int numblocks)
{
	int status;
	int nblock=numblocks;
	
	if (ibis->flags & FLAG_FILE_IMAGE)
		return IBIS_CONTAINS_IMAGE_DATA;

	zldel( ibis->unit, "system",label,NULL);
	status = zladd( ibis->unit, "system",label,
			&nblock,"format","int", NULL);

	return status;
}

