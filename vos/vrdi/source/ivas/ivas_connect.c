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

FUNCTION IVAS_Connect( Unit )
int	*Unit;

   {
   IVASgmFreeze( TRUE );
   IVASgmDefImage( WHICH_IMP(RED_LUT)-1, WHICH_IMP(GREEN_LUT)-1,
			WHICH_IMP(BLUE_LUT)-1, TRUE, 0 );
   IVASgmFreeze( FALSE );

   IVASflush();
   
   return (SUCCESS);
   }
