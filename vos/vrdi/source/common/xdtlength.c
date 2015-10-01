/*	xdtlength - description
 *
 *	Purpose:
 *
 *	Written by:  R. Mortensen
 *	Date:
 *
 *	Calling Sequence:
 *
 *		STATUS = xdtlength( parameters )
 *
 *	Parameter List:
 *
 *		Unit:	Display device unit number
 *
 *	Possible Error Codes:
 *
 */

#include "xvmaininc.h"
#include "ftnbridge.h"
#include "xdexterns.h"
#include "xdroutines.h"
#include "xderrors.h"
#include "xdfuncs.h"

FUNCTION FTN_NAME(xdtlength)( INTEGER Length, INTEGER NChars, STRING Text,
	ZFORSTR_PARAM )
   {
   ZFORSTR_BLOCK
   char		CString[256];

   zsfor2c(CString, 255, Text, &Length, 3, 3, 1, Text);
   return ( zdtlength( Length, *NChars, CString ) );
   }

FUNCTION zdtlength( length, nchars, text )
int	*length, nchars;
char	text[];
   {
   int		status, i;
   float	len;

   xd_current_call = TLENGTH;

   status = SUCCESS;
   if (!xd_initialized) {
      status = XD_Initialize();
      }
   if (nchars < 0) {
      status = INVALID_CHAR_COUNT;
      }

   if (status == SUCCESS) {

      len = 0;
      if (nchars == 0)
         nchars = strlen(text);
      else
         nchars = MIN( nchars, strlen(text) );

      for ( i = 0; i < nchars; i++ ) {
         len += xd_font_info.cWidths[text[i]];
         }

      *length = (len * FONT_SCALE * FONT_HEIGHT) + 0.5;

      status = SUCCESS;
      }

   xd_error_handler( &NoUnit, status );
   return (status);
   }
