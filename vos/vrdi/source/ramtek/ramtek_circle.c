/*	RAMTEK_Circle - description
 *
 *	Purpose:
 *
 *	Written by:
 *	Date:
 *
 *	Calling Sequence:
 *
 *		STATUS = RAMTEK_Circle( parameters )
 *
 *	Parameter List:
 *
 *		Unit:	Display device unit number
 *
 *	Possible Error Codes:
 *
 */

#include "xdexterns.h"
#include "xdroutines.h"
#include "xderrors.h"
#include "xdfuncs.h"

#include "rtekinc.h"

FUNCTION RAMTEK_Circle( Unit, imp, xcenter, ycenter, radius, value, mask )
int	*Unit, imp, xcenter, ycenter, radius, value;
unsigned char	mask;
{
  CIRCLE_COLOR = value;
  CIRCLE_X = xcenter - 1;
  CIRCLE_Y = ycenter - 1;
  CIRCLE_RAD = radius;

  CIRCLE_X_MIN = AW_LEFT( imp ) - 1;
  CIRCLE_Y_MIN = AW_TOP( imp ) - 1;
  CIRCLE_X_MAX = AW_RIGHT( imp ) - 1;
  CIRCLE_Y_MAX = AW_BOTTOM( imp ) - 1;

  CIRCLE_MASK1 = mask;
  CIRCLE_MASK2 = 0;

  rmout( &RM_Channel_No, &RM_Circle, &CIRCLE_WORDS );

  return ( SUCCESS );
}
