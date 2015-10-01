/*	TEK_Graphics - description
 *
 *	Purpose: Turn on or off display of the graphics overlay on the
 *		 Tektronix display terminal.
 *
 *	Written by: Paul Bartholomew
 *	Date:	    September 18, 1989
 *
 *	Calling Sequence:
 *		STATUS = TEK_Graphics(Unit, function)
 *
 *	Parameter List:
 *		Unit:	Display device unit number
 *		function: Function code, either GRAPHICS_ON or GRAPHICS_OFF
 *
 *	Possible Error Codes:
 *		none
 *
 */

#include "xdexterns.h"
#include "xdroutines.h"
#include "xderrors.h"
#include "xdfuncs.h"
#include "xdsupport.h"
#include "tek.h"

FUNCTION TEK_Graphics(Unit, function)
int *Unit;
int function;
{
  int	size, area[4], status;
  unsigned char	*dummy;

  if (function == GRAPHICS_ON)
  {
    if (TEK_OVERLAY_ON == TRUE)
      status = SUCCESS;
    else
    {
      TEK_OVERLAY_ON = TRUE;
      if (TEK_WRITTEN(TEK_OVERLAY_IMP) == FALSE)
        status = SUCCESS;
      else
        status = TEK_Area(Unit, WRITE_AREA, TEK_OVERLAY_IMP, size, area,
                          dummy, ALL_BITS, TEK_REFRESH_SCREEN);
    }
  }
  else					/*  function == GRAPHICS_OFF  */
  {
    if (TEK_OVERLAY_ON == FALSE)
      status = SUCCESS;
    else
    {
      TEK_OVERLAY_ON = FALSE;
      if (TEK_WRITTEN(TEK_OVERLAY_IMP) == FALSE)
        status = SUCCESS;
      else
        status = TEK_Area(Unit, WRITE_AREA, TEK_OVERLAY_IMP, size, area,
                          dummy, ALL_BITS, TEK_REFRESH_SCREEN);
    }
  }

  return(status);
}
