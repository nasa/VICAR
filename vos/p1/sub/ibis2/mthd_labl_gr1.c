#include "ibis.h"
#include <string.h>
#include <ctype.h>

/*
 *  Private label methods for old GRAPHICS-1 files, read & write
 *  The only visible (protected) method is _i_install_grlabel_methods.
 *   All others are called by method pointers.
 */
 

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
	if (ibis->nc <1) return IBIS_NC_REQUIRED;
	if (ibis->nc > MAX_OLD_COL)
	{
		ibis->nc = 0; /* no columns allocated yet */
		return IBIS_FILE_OLD_IBIS;
	}
	ibis->extent = ibis->nc * 4;

	/* create column structure */
	status = _i_init_column_array( ibis );
	if (status != 1) return status;

	/*
	 *  Unless someone actually manually changed the host-format
	 *  (using IBISFileUnit,IBISFileSet), we hardwire this to VAX-VMS host.
	 *  We need to do this here so that the "format_size" is known.
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

	/* Install file methods */

	status = _i_install_grfile_methods( ibis );
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
	int col;
	char format[20];

	/*
	 *  The GRAPHICS-1 format has no NC parameter; this must have
	 *  been specified by the user. Complain if not.
	 */

	if (ibis->nc <1) return IBIS_NC_REQUIRED;
	
	_i_install_grfile_methods( ibis );

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

	/* 
	 * get the IBIS-1 information from the label
	 */

	zvget( ibis->unit, "recsize", &ibis->recsize,
			   "nl", &ibis->numblocks,
			   "format", format,
			    NULL );
	ibis->blocksize = ibis->recsize;
	
	/* sanity check for IBIS-1 */
	
	if (ibis->blocksize !=OLD_BLKSIZE || tolower(format[0]) != 'b')
		return(IBIS_FILE_IS_NOT_IBIS);

	
	/* work out the sizing parameters from NC */
	
	ibis->segment = ibis->nc * ibis->format_size[ FMT_REAL ];
	ibis->extent = ibis->nc * 4;
	ibis->nr =  (ibis->numblocks * ibis->blocksize) / ibis->segment;
	
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

	/*
	 * Write label stuff out to property label 
	 */


static int _flush_write( XIBIS *ibis )
{
	int status=1;
	
	ibis->extent = ibis->nc * 4;

	/*
	 * There is no column-size parameter for GRAPHICS-1	 :-(
	 */

	/*
	 * Make sure NL is in sync with struct
	 */

	status = _i_install_numblocks(ibis, "nl",ibis->numblocks);

	return (status);
}


int _i_install_grlabel_methods(XIBIS * ibis )
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
			ibis->labmethod->post_open = _i_null_method;
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
