#include "ibis.h"


/**
 **   Implementation of XIBIS file-io methods for old files
 **   The only public (protected) routine is _i_ibis1_install_methods()
 **/


static int open_read( ibis )
XIBIS *ibis;
{
	return (zvopen( ibis->unit, "op", "read", "convert", "off", NULL ) );
}

static int open_update( ibis )
XIBIS *ibis;
{
	return (zvopen( ibis->unit, "op", "update", "convert", "off", NULL ));
}

static int open_write( ibis )
XIBIS *ibis;
{
	int status;
	int tabular=ibis->flags & FLAG_ORG_COLUMN;
	
	/*
	 * create the IBIS file using the
	 * parameters set by ibis->labmethod->pre_open()
	 *
	 */

	status = zvopen( ibis->unit, "op", "write", "o_format", "byte",
						"type", (tabular ? "tabular" : "graph1"),
						"u_ns", ibis->blocksize,
						"u_nl", ibis->numblocks,
						"host", ibis->hostfmt,
						 NULL) ;
	if (status != 1) return (status);

	/* Get rid of IBIS-2 Property label, if any */
	status = IBISLabelRemove(ibis->unit);
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

void _i_ibis1_install_methods(XIBIS* ibis )
{

	if (!ibis) return;
	
	ibis->filemethod->init = _i_null_method;
	
	switch (ibis->flags & MASK_MODE)
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
	
	switch( ibis->flags & MASK_ORG)
	{
		case FLAG_ORG_ROW: /* graphics file */
			_i_install_grlabel_methods( ibis );
			break;
		case FLAG_ORG_COLUMN: /* standard ibis */
			_i_install_olabel_methods( ibis );
			break;
	}

}

