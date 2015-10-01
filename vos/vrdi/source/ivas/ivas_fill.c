/*	IVAS_Fill - description
 *
 *	Purpose:
 *
 *	Written by:
 *	Date:
 *
 *	Calling Sequence:
 *
 *		STATUS = IVAS_Fill( parameters )
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

FUNCTION IVAS_Fill( Unit, imp, color, mask, area )
int	*Unit, imp, color, mask, *area;

   {
   if (imp != OVERLAY_IMP) return (DEVICE_CANNOT_DO_IT);

   IVASgphValue( color, color );
   IVASgphMask( mask );

   IVASgphMove( IVAS_X_GPH(area[LEFT]),  IVAS_Y_GPH(area[TOP]),    GPHabs );
   IVASgphFill( IVAS_X_GPH(area[RIGHT]), IVAS_Y_GPH(area[BOTTOM]), GPHabs );

   IVASflush();

   return (SUCCESS);
   }
