#include "ibis.h"
#include <string.h>
#include <ctype.h>
#include <zvproto.h>

/*
 *  Private label methods for IBIS-1 files, read & write
 *  The only visible (protected) method is _i_install_olabel_methods.
 *   All others are called by method pointers.
 */
 
static int _flush_write(XIBIS *);	/* forward ref to shut up compiler */

/*
 *  Set up the file info prior to opening
 */

static int _pre_open_write( XIBIS *ibis )
{
	int col;
	int format;
	char *fmt=ibis->fmt;
	int status=1;
	int fmt_size=ibis->fmt_len;

	/* sanity check */
	if (ibis->nc > MAX_OLD_COL)
	{
		ibis->nc = 0; /* no columns allocated yet */
		return IBIS_FILE_OLD_IBIS;
	}

	/* create column structure */
	status = _i_init_column_array( ibis );
	if (status != 1) return status;

	/*
	 *  Unless someone actually manually changed the host-format
	 *  (using IBISFileUnit,IBISFileSet), we hardwire this to VAX-VMS host.
	 */

	if (!ibis->hostfmt[0]) strcpy(ibis->hostfmt, "VAX-VMS");
	strcpy(ibis->pix_host, ibis->hostfmt);
	_i_trans_init( ibis ); /* set up translation buffers */

	/* set the specified or defaulted format types */

	if (fmt && *fmt) 
	{
		for (col=0;col<ibis->nc; col++)
		{
			/* check that size is ok */
	
			format = _i_IBISFormatCode(fmt);
			if (format < FMT_BYTE)
				return IBIS_INVALID_FORMAT;
			if (ibis->format_size[format]>4)
				return IBIS_FILE_OLD_IBIS;
	
			_i_attach_format_to_column(ibis,format,col);
			fmt += fmt_size;
		}
		free (ibis->fmt);
		ibis->fmt = (char *)0;
		ibis->fmt_len=IFMT_SIZE;
	}
	else for (col=0;col<ibis->nc; col++)
		_i_attach_format_to_column(ibis,ibis->default_fmt,col);

	ibis->extent = ibis->nc * 4;

	/* Install file methods */

	status = _i_install_ofile_methods( ibis );
	if (status != 1) return status;

	
	ibis->filemethod->init( ibis );

	return (1);
}


/*
 *  Work out details from file info
 */

static int _post_open_read( XIBIS *ibis )
{
	int status=1;
	int unit = ibis->unit;
	int col;
	int nr_value;
	char format[20];

	/* 
	 * get the IBIS-1 information from the label
	 */

	zvget( ibis->unit, "recsize", &ibis->recsize,
			   "nl", &ibis->numblocks,
			   "format", format,
			    NULL );

	_i_install_ofile_methods( ibis );

	/*
	 * somehow, somewhere, someone might have tried
	 * to port the old IBIS routines. If so, the file
	 * would most likely not be in VAX-VMS format.
	 *
	 * We hardwire the format here to VAX-VMS host.
	 *
	 * If this is a "bogus-IBIS-port" file, a client program can always set
	 * the "host" label item using IBISFileUnit, IBISFileSet.
	 */

	if (!ibis->hostfmt[0]) strcpy(ibis->hostfmt, "VAX-VMS");
	strcpy( ibis->pix_host, ibis->hostfmt);
	_i_trans_init( ibis ); /* set up translation buffers */

	ibis->blocksize = ibis->recsize;
	
	/* sanity check for IBIS-1 */
	
	if (ibis->blocksize !=OLD_BLKSIZE || tolower(format[0]) != 'b')
		return(IBIS_FILE_IS_NOT_IBIS);

	/*
	 *  The old format contains one line of FULL, whose first sample
	 *  is the number of rows per column, in the ibis->host's FULL format.
	 */

	status = zvread( unit, &nr_value, "line", 1, "nsamps", sizeof(int), NULL);
	zvtrans((int*)&ibis->trans[FMT_FULL].intrans, &nr_value, &ibis->nr, 1 );
	
	/* work out the rest of the parameters from this */
	
	ibis->segment = ALIGN_UP(ibis->nr, OLD_BLKSIZE/4);
	ibis->nc = ((ibis->numblocks-1) * ibis->blocksize)/(ibis->segment*4) ;
	ibis->extent = ibis->nc * 4;
	
	/* Set up the columns */
	
	status = _i_init_column_array( ibis );
	if (status != 1) return status;
	for (col=0;col<ibis->nc;col++)
	{
		status=_i_attach_format_to_column(ibis,ibis->default_fmt,col);
		if (status!=1) return status;
	}

	return (status);
}

static int _post_open_write( XIBIS *ibis )
{
	int status;
	
	status = _flush_write( ibis ); /* make sure the label is set up right */
	
	return status;
}


	/*
	 * Write label stuff out to property label 
	 */


static int _flush_write( XIBIS *ibis )
{
	int status=1;
	int nl=ibis->numblocks;
	int nr_value;
	int size=ibis->format_size[FMT_FULL];
	
	ibis->extent = ibis->nc * 4;

	/*
	 * Write column-size parameter to first record of file;
	 *  The old format is one line of FULL, whose first sample
	 *  is the number of rows per column, in Host FULL format.
	 */

	zvtrans((int*)&ibis->trans[FMT_FULL].outtrans, &ibis->nr, &nr_value, 1);
	status = zvwrit( ibis->unit, &nr_value, "line", 1, "nsamps", size, NULL);
	if (status != 1) return status;

	/*
	 * Make sure NL is in sync with struct
	 */

	status=zldel( ibis->unit, "system","nl",NULL);
	status=zladd( ibis->unit, "system","nl",&nl,"format","int", NULL);

	return (status);
}



int _i_install_olabel_methods(XIBIS *ibis )
{
	int status=1;

	if (ibis) switch ( ibis->flags & MASK_MODE )
	{
		case FLAG_MODE_READ:
			ibis->labmethod->pre_open = _i_null_method;
			ibis->labmethod->post_open = _post_open_read;
			ibis->labmethod->flush = _i_null_method;
			break;
		case FLAG_MODE_WRITE:
			ibis->labmethod->pre_open = _pre_open_write;
			ibis->labmethod->post_open = _post_open_write;
			ibis->labmethod->flush = _flush_write;
			break;
		case FLAG_MODE_UPDATE:
			ibis->labmethod->pre_open = _i_null_method;
			ibis->labmethod->post_open = _post_open_read;
			ibis->labmethod->flush = _flush_write;
			break;
	}
	
	return (status);
}
