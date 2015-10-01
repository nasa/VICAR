/*	TEK_Zoom - description
 *
 *	Purpose: Set the zoom factor and display window of the Tektronix
 *		 display device, allowing panning and scrolling
 *
 *	Written by: Paul Bartholomew
 *	Date:	    November 9, 1989
 *
 *	Calling Sequence:
 *		STATUS = TEK_ZoomDW(Unit, imp, zoom)
 *
 *	Parameter List:
 *		Unit:	Display device unit number
 *		imp:	Image plane number to set
 *		zoom:	Zoom factor 1=1:1, 2=2:1, ... 8=8:1
 *
 */

#include "xdexterns.h"
#include "xdroutines.h"
#include "xderrors.h"
#include "xdfuncs.h"
#include "xdsupport.h"
#include "tek.h"

FUNCTION TEK_Zoom(Unit, imp, zoom)
int *Unit, imp, zoom;
{
  int	size, area[4], status;
  unsigned char	*dummy;

  if (TEK_ZOOM(imp) != zoom)
  {
    TEK_ZOOM(imp) = zoom;

    /*  If nothing has yet been written to the image plane, do not  */
    /*  refresh the screen.                                         */

    if (TEK_WRITTEN(imp) == FALSE)
      status = SUCCESS;
    else
      status = TEK_Area(Unit, WRITE_AREA, imp, size, area, dummy, ALL_BITS,
                        TEK_REFRESH_SCREEN);

  }
  else
    status = SUCCESS;

  return (status);
}
