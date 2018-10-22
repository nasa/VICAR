#include "ibis.h"
#include "ibisdeclares.h"
#include <ctype.h>

/* XIBIS Column Format Code Routines */


int _i_IBISFormatCode( char *fmtstr )
{
	int code;
	
	code=_i_keymatch( fmtstr, format_name );
	if (code) /* good match */
		return code-1;
	
	/* If we got here, it's either ASCII or bad */
	
	if (tolower(*fmtstr)!='a') return 0; /* bad */

  	code = atoi( fmtstr+1 );
  	if (code<=0 || code > FMT_LAST-FMT_ASCII)
  		return IBIS_INVALID_FORMAT;
  	return FMT_ASCII+code;
}

int _i_IBISFormatSize( XIBIS *ibis, int format, int *size)
{
	int status=1;
	int old = (ibis->flags&FLAG_FILE_OLD) ? 1 : 0;
	
	if (format < FMT_NONE || format > FMT_LAST )
		return IBIS_INVALID_FORMAT;
	
	if (format > FMT_NONE && format < FMT_ASCII )
	{
		status = zvpixsize( size, format_name[ format ], 
		    ibis->intfmt,ibis->realfmt);
	}
	else if (old) /* Force ASCII size to be 4 or greater */
	{
		*size = format_size[ format ] - 1;
		if (*size < 4) *size=4;
	}
	else
		*size = format_size[ format ];
	
	return status;

}

