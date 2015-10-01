/*	xdatext - description
 *
 *	Purpose:
 *
 *	Written by:  R. Mortensen
 *	Date:
 *
 *	Calling Sequence:
 *
 *		STATUS = xdatext( Unit, X, Y, NChars, Text )
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

FUNCTION FTN_NAME(xdatext)(INTEGER Unit, INTEGER X, INTEGER Y, INTEGER NChars,
	STRING Text, INTEGER Blink,INTEGER Reverse, ZFORSTR_PARAM)
   {
   ZFORSTR_BLOCK
   char		CString[256];

   zsfor2c(CString, 255, Text, &Unit, 7, 5, 1, Reverse);
   while (*NChars > strlen(CString))
      strcat(CString, " ");		/* blank pad out to NChars */
   return( zdatext( *Unit, *X, *Y, *NChars, CString, *Blink, *Reverse ) );
   }

FUNCTION zdatext( unit, x, y, nchars, text, blink, reverse )
int	unit, x, y, nchars, blink, reverse;
char	text[];
   {
   int		status;

   xd_current_call = ATEXT;

   if (!ZCHECK_UNIT_NUMBER) {
      status = UNIT_OUT_OF_RANGE;
      }
   else if (!ZCHECK_DEVICE_OPEN) {
      status = DEVICE_NOT_OPEN;
      }
   else if (!ZCHECK_DEVICE_ACTIVE) {
      status = DEVICE_NOT_ACTIVE;
      }
   else if (!ZAFG_AVAILABLE) {
      status = AFG_NOT_AVAILABLE;
      }
   else if (!ZAFG_ACTIVE) {
      status = AFG_NOT_ACTIVE;
      }
   else if (nchars < 0) {
      status = INVALID_CHAR_COUNT;
      }
   else {
      if (nchars == 0)
         nchars = strlen(text);
      else
         nchars = MIN( nchars, strlen(text) );

      if (nchars == 0)
         status = SUCCESS;
      else
         status = XD_Device_Interface( &unit, AFG_TEXT, x, y, nchars, text,
                                       blink, reverse );
      }

   xd_error_handler( &unit, status );
   return (status);
   }
