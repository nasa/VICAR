#include "ibis.h"

/**
 **   Implementation of XIBIS file-io methods for IBIS-2 files.
 **   The only public (protected) routine is _i_ibis2_install_methods()
 **/


static int open_read( ibis )
XIBIS *ibis;
{
	return (zvopen( ibis->unit, "op", "read", "cond", "binary","lab_act","  ", NULL ) );
}

static int open_update( ibis )
XIBIS *ibis;
{
	return (zvopen( ibis->unit, "op", "update", "cond", "binary","lab_act","  ", NULL ));
}


static int open_write( ibis )
XIBIS *ibis;
{
	int status;
	
	/*
	 * create the IBIS file using the
	 * parameters set by ibis->labmethod->pre_open()
	 * The data is now put into the Binary label.
	 */
		
	status = zvopen( ibis->unit, "op", "write", 
						"cond", "binary",
						"lab_act","  ",
						"o_format", format_name[ibis->pix_fmt],
						"host", ibis->pix_host,
						"type", "tabular",
						"u_nlb",ibis->numblocks,
						"u_ns", ibis->ns,
						"u_nl", 1,     /* so that VAX won't complain */
						 NULL ) ;
	if (status != 1) return (status);

	/* 
	 * Since this is a new file, we kill off the "NL" label
	 * to indicate that there is no image, and get rid of
         * any inherited IBIS groups, etc.
	 */
	
        status = IBISLabelRemove(ibis->unit);
	if (status != 1) return (status);

	status = _i_install_numblocks(ibis, "nl", 0);
	if (status != 1) return (status);
	
	status = zvclose( ibis->unit, NULL );
	if (status != 1) return (status);
	
	return (open_update(ibis));
}



static int close_read( ibis )
XIBIS *ibis;
{
	return (zvclose( ibis->unit, NULL ));
}

static int close_write( ibis )
XIBIS *ibis;
{
	return (zvclose( ibis->unit, NULL ));
}


/*
 *  Method set-up
 */

void _i_ibis2_install_methods(XIBIS* ibis )
{

	ibis->filemethod->init = _i_null_method;
	
	if (ibis) switch (ibis->flags & MASK_MODE)
	{
		case FLAG_MODE_READ:
			ibis->filemethod->open = open_read;
			ibis->filemethod->close = close_read;
			break;
		case FLAG_MODE_WRITE:
			ibis->filemethod->open = open_write;
			ibis->filemethod->close = close_write;
			break;
		case FLAG_MODE_UPDATE:
			ibis->filemethod->open = open_update;
			ibis->filemethod->close = close_write;
			break;
	}
	
	_i_install_nlabel_methods( ibis );

}
