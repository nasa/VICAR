/**
 **   util_trans.c -- handles format translations
 **/

#include "ibis.h"
#include "string.h"
#include <zvproto.h>
/*
 * convert string data of different lengths,
 * appending null if required.
 */
 
static void ascii_to_ascii
(
  int inlen,
  int outlen,
  char *inbuf,
  char *outbuf,
  int num,
  int nullend
)
{
	register char *inptr=inbuf;
	register char *outptr=outbuf;
	char *nptr;
	int i,len0;
	
	len0 = inlen < outlen ? inlen : outlen-1;

	for (i=0;i<num;i++,inptr+=inlen,outptr+=outlen)
	{
		strncpy(outptr,inptr,outlen);
		if (nullend) outptr[len0]='\0'; /* just to be safe; */
		else  /* pad with spaces */
			for (nptr=outptr+outlen-1; (!*nptr) && nptr>=outptr; nptr--)
				*nptr=' ';
	}
}


int _i_set_trans(XIBIS* ibis,trans_type* trans,int buffmt,int filefmt)
{
	if (filefmt > FMT_ASCII) filefmt=FMT_ASCII; /* must be generic */
	
	/* make sure not ASCII-numeric translation */

	if ((filefmt == FMT_ASCII && buffmt < FMT_ASCII && buffmt>FMT_NONE) ||
		(filefmt < FMT_ASCII && buffmt>=FMT_ASCII)) 
		return IBIS_CANT_TRANSLATE;

	trans->infmt = buffmt;
	trans->outfmt = filefmt;
	trans->format_size = ibis->format_size;
	trans->new = (ibis->flags & FLAG_FILE_OLD) ? 0 : 1;
	
	if (buffmt==FMT_NONE || filefmt == FMT_ASCII) return 1;

	/* This is a numeric translation */

	zvtrans_in((int*)&trans->intrans, format_name[ filefmt ],
		format_name[buffmt],ibis->intfmt, ibis->realfmt);
	zvtrans_out((int*)&trans->outtrans, format_name[buffmt],
		format_name[filefmt],ibis->intfmt, ibis->realfmt);

	return 1;

}

/* initialize translation buffers based on hostfmt */

int _i_trans_init(XIBIS *ibis )
{
	int status;
	char intfmt[MAX_GRP_NAME+1], realfmt[MAX_GRP_NAME+1];

	/* 
	 * get the int and real formats. If the host is not
	 * recognized, we will hope that the individual fmts were
	 * set manually, but just to be sure, return the
	 * error status of zvhost.
	 */
	
	status = zvhost( ibis->hostfmt, intfmt, realfmt);
	
	if (status ==1)
	{
		strcpy( ibis->intfmt, intfmt);
		strcpy( ibis->realfmt, realfmt);
	}
	
	_i_make_uppercase(ibis->hostfmt);
	
	_i_trans_reset( ibis );
	
	return status;
}

/* Reset translators and format_size  */

void _i_trans_reset(XIBIS* ibis )
{
	int fmt;
	trans_type *trans=ibis->trans;
	
	/* reset all format translators */
	
	if (ibis->intfmt[0])
	{
		_i_make_uppercase(ibis->intfmt);
		for (fmt = FMT_BYTE; fmt<=FMT_FULL; fmt++)
			_i_set_trans( ibis, trans+fmt, fmt, fmt );
	}

	if (ibis->realfmt[0])
	{
		_i_make_uppercase(ibis->realfmt);
		for (fmt = FMT_REAL; fmt<=FMT_COMP; fmt++)
			_i_set_trans( ibis, trans+fmt, fmt, fmt );
	}

	_i_set_trans( ibis, trans+FMT_ASCII, FMT_ASCII, FMT_ASCII );
	
	/* reset size array */
	for (fmt=FMT_BYTE; fmt<=FMT_LAST; fmt++)
		_i_IBISFormatSize( ibis, fmt, ibis->format_size+fmt);
	
}

int _i_trans_buf_to_local(trans_type *trans, XCOL *col, char* inbuf, 
			  char *outbuf, int num )
{
	int infmt=trans->infmt;

	if (infmt<FMT_ASCII && infmt>FMT_NONE)
		zvtrans((int*)&trans->intrans, inbuf, outbuf, num );
	else
	{
		int format=col->format;
		int filesize=trans->format_size[format];
		
		if (infmt==FMT_NONE)
			memcpy( outbuf, inbuf, num * format_size[format]);
		else 
			ascii_to_ascii(filesize, format_size[infmt],  inbuf, outbuf, num, 1 );
	}

	return 1;
}


int _i_trans_buf_from_local(trans_type* trans, XCOL* col, char* inbuf, 
			    char* outbuf, int num )
{
	int infmt=trans->infmt;

	if (infmt<FMT_ASCII && infmt>FMT_NONE)
		zvtrans((int*)&trans->outtrans, inbuf, outbuf, num );
	else
	{
		int format=col->format;
		int filesize=trans->format_size[format];
		
		if (infmt==FMT_NONE)
			memcpy( outbuf, inbuf, num * format_size[format]);
		else 
			ascii_to_ascii(format_size[infmt], filesize, inbuf, outbuf, num, trans->new );
	}

	return 1;
}

