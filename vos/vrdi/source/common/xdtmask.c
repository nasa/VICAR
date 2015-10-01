/*	xdtmask - Set the text writing mask
 *
 *	Purpose:
 *
 *	Written by:  R. Mortensen
 *	Date:
 *
 *	Calling Sequence:
 *
 *		STATUS = xdtmask( parameters )
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

FUNCTION FTN_NAME(xdtmask)( Mask )
BYTE	Mask;
   {
   return ( zdtmask( *Mask ) );
   }

FUNCTION zdtmask( mask )
unsigned char	mask;
   {
   int	status;

   xd_current_call = TMASK;

   status = SUCCESS;
   if (!xd_initialized) {
      status = XD_Initialize();
      }

   if (status == SUCCESS) {
      FONT_MASK = mask;
      }

   xd_error_handler( &NoUnit, status );
   return (status);
   }
