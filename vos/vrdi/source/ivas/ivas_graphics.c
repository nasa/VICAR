/*	IVAS_Graphics - description
 *
 *	Purpose:
 *
 *	Written by:
 *	Date:
 *
 *	Calling Sequence:
 *
 *		STATUS = IVAS_Graphics( parameters )
 *
 *	Parameter List:
 *
 *		Unit:	Display device unit number
 *
 *	Possible Error Codes:
 *
 */

#include "xvmaininc.h"
#include "xdexterns.h"
#include "xdroutines.h"
#include "xderrors.h"
#include "xdfuncs.h"
#include "ivasinc.h"

FUNCTION IVAS_Graphics( Unit, function )
int	*Unit, function;

   {
   IVASgmFreeze( TRUE );
   if (function == GRAPHICS_ON) {
      IVASgmSelGraphic( -1, GMcolor, 0 );
      }
   else {
      IVASgmSelGraphic( -1, GMdisable, 0 );
      }
   IVASgmFreeze( FALSE );
   
   IVASflush();

   return (SUCCESS);
   }
