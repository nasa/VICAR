/*	Dummy_Pixel - description
 *
 *	Purpose: Read or write a single pixel to the dummy display
 *               terminal.
 *
 *	Written by: Paul Bartholomew
 *	Date:	    May 30, 1990
 *
 *	Calling Sequence:
 *		STATUS = Dummy_Pixel(Unit, function, imp, x, y, pixel, mask)
 *
 *	Parameter List:
 *		Unit:	  Display device unit number
 *		function: Function code, either READ_PIXEL or WRITE_PIXEL
 *		imp:	  Image plane number to read/write from/to
 *		x:	  X coordinate of pixel
 *		y:	  Y coordinate of pixel
 *		pixel:	  Value of pixel
 *		mask:	  Bit plane mask for writing
 *
 *	Possible Error Codes:
 *		none
 */

#include "xvmaininc.h"
#include "xdexterns.h"
#include "xdroutines.h"
#include "xderrors.h"
#include "xdfuncs.h"
#include "dummy_dev.h"

FUNCTION Dummy_Pixel(Unit, function, imp, x, y, pixel, mask)
int *Unit, function, imp, x, y;
unsigned char *pixel, mask;
{

  if (function == WRITE_PIXEL)
    DUMMY_IMP(imp, x, y) = (DUMMY_IMP(imp, x, y) & ~mask) | ((*pixel) & mask);
  else					/* Function = READ_PIXEL */
    *pixel = DUMMY_IMP(imp, x, y);

  return (SUCCESS);
}
