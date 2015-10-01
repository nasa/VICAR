/*	xdttext - description
 *
 *	Purpose:
 *
 *	Written by:  R. Mortensen
 *	Date:
 *
 *	Calling Sequence:
 *
 *		STATUS = xdttext( parameters )
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

FUNCTION FTN_NAME(xdttext)(INTEGER Unit,INTEGER Imp,INTEGER XPos,INTEGER YPos,
	INTEGER Location,INTEGER NChars,STRING Text, ZFORSTR_PARAM)
   {
   ZFORSTR_BLOCK
   char		CString[256];

   zsfor2c(CString, 255, Text, &Unit, 7, 7, 1, Text);
   return ( zdttext( *Unit, *Imp, *XPos, *YPos, *Location, *NChars, CString ));
   }

FUNCTION zdttext( unit, imp, xpos, ypos, location, nchars, text )
int	unit, imp, xpos, ypos, location, nchars;
char	text[];
   {
   int		status, i, batch_mode;
   int		startX, startY;
   char		iChar;
   float	sLength;

   xd_current_call = TTEXT;

   if (!ZCHECK_UNIT_NUMBER) {
      status = UNIT_OUT_OF_RANGE;
      }
   else if (!ZCHECK_DEVICE_OPEN) {
      status = DEVICE_NOT_OPEN;
      }
   else if (!ZCHECK_DEVICE_ACTIVE) {
      status = DEVICE_NOT_ACTIVE;
      }
   else if (!ZCHECK_IMP( imp )) {
      status = NO_SUCH_IMP;
      }
   else if ((location < 1) || (location > 3)) {
      status = INVALID_FONT_LOCATION;
      }
   else if (nchars < 0) {
      status = INVALID_CHAR_COUNT;
      }
   else {
      SET_FLAG(ZIMP_FLAG( imp ));
      SET_FLAG(ZIMP_FLAG( 0 ));

      if (nchars == 0)
         nchars = strlen(text);
      else
         nchars = MIN( nchars, strlen(text) );

      status = XD_Device_Interface( &unit, WRITE_TEXT, imp, xpos, ypos,
                                    location, &nchars, text );

      if ( status == DEVICE_CANNOT_DO_IT ) {
         batch_mode = ZBATCH_MODE;
         if (!batch_mode)
            zd_batch(unit, TRUE);
         sLength = 0.0;
         for ( i = 0; i < nchars; i++ ) {
            sLength += xd_font_info.cWidths[text[i]];
            }
         sLength *= (FONT_SCALE * FONT_HEIGHT);

         switch (location) {
 	    case 1: {
               startX = xpos;
               startY = ypos;
               break;
               }
	    case 2: {
	      startX = xpos - ROUND( FONT_COSINE * sLength / 2 );
	      startY = ypos + ROUND( FONT_SINE   * sLength / 2 );
	      break;
	      }
	    case 3: {
	      startX = xpos - ROUND( FONT_COSINE * sLength );
	      startY = ypos + ROUND( FONT_SINE   * sLength );
	      break;
	      }
	    }

	 for ( i = 0; i < nchars; i++ ) {
	    iChar = text[i];
	    if (xd_font_info.ptrMD[iChar] != 0) {
	       status = XD_Draw_Char( &unit, imp, iChar, startX, startY );
	       if (status != SUCCESS) break;

	       sLength = (xd_font_info.cWidths[iChar] * FONT_SCALE *
			      FONT_HEIGHT);
	       startX += ROUND( FONT_COSINE * sLength );
	       startY -= ROUND( FONT_SINE   * sLength );
	    }
         }
         if (!batch_mode)
            zd_batch(unit, batch_mode);
      }
   }

   xd_error_handler( &unit, status );
   return (status);
   }
