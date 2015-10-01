/*	IVAS_Switch - description
 *
 *	Purpose:
 *
 *	Written by:
 *	Date:
 *
 *	Calling Sequence:
 *
 *		STATUS = IVAS_Switch( parameters )
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

FUNCTION IVAS_Switch( Unit, device, sw, value )
int	*Unit, device, sw, *value;

   {
   int	button;
   int	dum1,dum2;

   /*  The IVAS trackball and keypad are set up so that pressing the  */
   /*  rightmost switch returns 1, the middle switch returns 2, and   */
   /*  the leftmost switch returns 4.  The other devices supported by */
   /*  the VRDI have switch 1 on the left, rather than the right, so  */
   /*  we adjust the values accordingly.                              */

   IVASmouseStatus( &button, &dum1, &dum2, 3, 0 );
   *value = BIT_TEST( button, 3-sw );

   return (SUCCESS);
   }
