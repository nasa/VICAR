/*	RAMTEK_Fill - description
 *
 *	Purpose:
 *
 *	Written by:
 *	Date:
 *
 *	Calling Sequence:
 *
 *		STATUS = RAMTEK_Fill( parameters )
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

FUNCTION RAMTEK_Fill( Unit, imp, color, mask, area )
int	*Unit, imp, color, *area;
unsigned char	mask;
{
  ERASE_FG_COLOR = color;

  ERASE_MASK1 = mask;
  ERASE_MASK2 = 0;

  ERASE_X_MIN = area[LEFT] - 1;
  ERASE_Y_MIN = area[TOP] - 1;
  ERASE_X_MAX = area[RIGHT] - 1;
  ERASE_Y_MAX = area[BOTTOM] - 1;

  rmout( &RM_Channel_No, &RM_Erase, &ERASE_WORDS );

  return (SUCCESS);
}
