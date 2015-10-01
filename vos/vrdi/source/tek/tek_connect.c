/*	TEK_Connect - description
 *
 *	Purpose: Connects an image plane to a LUT (look-up table) on the
 *		 Tektronix display terminal.
 *
 *	Written by: Paul Bartholomew
 *	Date:	    October 18, 1989
 *
 *	Calling Sequence:
 *		STATUS = TEK_Connect(Unit, imp, lut, section, bypass)
 *
 *	Parameter List:
 *		Unit:	 Display device unit number
 *		imp:	 Image plane number to connect
 *		lut:	 LUT number to connect to
 *			 1=red, 2=green, 3=blue, 4=graphics
 *		section: Section number of LUT to use (not valid for graphics)
 *		bypass:  1=bypass LUT, 0=use LUT (not valid for graphics)
 *
 *	Possible Error Codes:
 *		none
 *
 */

#include "xdexterns.h"
#include "xdroutines.h"
#include "xderrors.h"
#include "xdfuncs.h"
#include "tek.h"

FUNCTION TEK_Connect(Unit, imp, lut, section, bypass)
int *Unit, imp, lut, section, bypass;
{
  int	size, area[4], status, previous;
  unsigned char  *dummy;

  if (TEK_WHICH_IMP(lut) != imp)
  {

    previous = TEK_WHICH_IMP(lut);
    TEK_WHICH_IMP(lut) = imp;

    /*  If nothing has yet been written to the image plane, do not  */
    /*  refresh the screen.                                         */

    if ((TEK_WRITTEN(previous) == FALSE) && (TEK_WRITTEN(imp) == FALSE))
      status = SUCCESS;
    else
      status = TEK_Area(Unit, WRITE_AREA, lut, size, area, dummy, ALL_BITS,
                        TEK_LUT_AREA);
  }
  else
    status = SUCCESS;

  return(status);
}
