/*	IVAS_Read_2d - description
 *
 *	Purpose:
 *
 *	Written by:
 *	Date:
 *
 *	Calling Sequence:
 *
 *		STATUS = IVAS_Read_2d ( parameters )
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

FUNCTION IVAS_Read_2d( Unit, device, xvalue, yvalue, prox, pen )
int	*Unit, device, *prox, *pen;
float	*xvalue, *yvalue;

{
  int button;
  int xpos, ypos;

  /*  The IVAS trackball and keypad are set up so that pressing the  */
  /*  rightmost switch returns 1, the middle switch returns 2, and   */
  /*  the leftmost switch returns 4.  The other devices supported by */
  /*  the VRDI have switch 1 on the left, rather than the right, so  */
  /*  we adjust the values accordingly.                              */

  IVASmouseStatus( &button, &xpos, &ypos, 3, 0 );

  *xvalue  = ( ((float) xpos) - 512 )/512;
  *yvalue  = ( ((float) ypos) - 512 )/512;

  *prox = TRUE;
  *pen = BIT_TEST( button, 3-device );

  return (SUCCESS);
}
