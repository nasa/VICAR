/*	TEK_SetDW - description
 *
 *	Purpose: 	Set the display window of the Tektronix display device,
 *			allowing panning and scrolling
 *
 *	Written by:	Paul Bartholomew
 *	Date:		November 9, 1989
 *
 *	Calling Sequence:
 *		STATUS = TEK_SetDW(Unit, imp, x, y)
 *
 *	Parameter List:
 *		Unit:	Display device unit number
 *		imp:	Image plane number to set
 *		x:	Coordinate of the left side of the display
 *		y:	Coordinate of the top of the display
 *
 *
 */

#include "xdexterns.h"
#include "xdroutines.h"
#include "xderrors.h"
#include "xdfuncs.h"
#include "xdsupport.h"
#include "tek.h"

FUNCTION TEK_SetDW(Unit, imp, x, y)
int *Unit, imp, x, y;
{
  int	size, area[4], status;
  unsigned char	*dummy;

  if ((x != TEK_DW_LEFT(imp)) || (y != TEK_DW_TOP(imp)))
  {
    TEK_DW_LEFT(imp) = x;
    TEK_DW_TOP(imp) = y;

    /*  If nothing has yet been written to the image plane, do not  */
    /*  refresh the screen.                                         */

    if (TEK_WRITTEN(imp) == FALSE)
      status = SUCCESS;
    else if ((imp == TEK_OVERLAY_IMP) && (TEK_OVERLAY_ON == FALSE))
      status = SUCCESS;
    else
      status = TEK_Area(Unit, WRITE_AREA, imp, size, area, dummy, ALL_BITS,
                        TEK_REFRESH_SCREEN);
  }
  else
    status = SUCCESS;

  return(status);
}
