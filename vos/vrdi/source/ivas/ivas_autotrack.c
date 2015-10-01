/*	name - description
 *
 *	Purpose:
 *
 *	Written by:
 *	Date:
 *
 *	Calling Sequence:
 *
 *		STATUS = name( parameters )
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

FUNCTION IVAS_AutoTrack( Unit, function, device, cursor )
int	*Unit, function, device, cursor;

   {
   IVASmouseCursor( function == AUTO_ON );
   IVASflush();

   return (SUCCESS);
   }
