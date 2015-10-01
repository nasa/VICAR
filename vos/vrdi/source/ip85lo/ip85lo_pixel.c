/*	IP85LO_Pixel - description
 *
 *	Purpose: Read or write a single pixel to the DeAnza display device.
 *
 *	Written by: Bob Deen
 *	Date:	    August 21, 1987
 *
 *	Calling Sequence:
 *		STATUS = IP85LO_Pixel(Unit, lun, function,
 *			 imp, x, y, pixel, mask)
 *
 *	Parameter List:
 *		Unit:	Display device unit number
 *		lun:	DeAnza logical unit number
 *		function: Function code, either READ_PIXEL or WRITE_PIXEL
 *		imp:	Image plane number to read/write from/to
 *		x:	X coordinate of pixel
 *		y:	Y coordinate of pixel
 *		pixel:	Value of pixel
 *		mask:	Bit plane mask for writing
 *
 *	Possible Error Codes:
 *		none
 *
 */
#include "xdexterns.h"
#include "xdroutines.h"
#include "xderrors.h"
#include "xdfuncs.h"

#include "ip85lo.h"

FUNCTION IP85LO_Pixel(Unit, lun, function, imp, x, y, pixel, mask)
int *Unit;
short lun;
int function, imp, x, y;
char *pixel;
char mask;
{
short impm1;
short xd, yd;

impm1 = imp - 1;
xd = IP85LO_X_DEV(x);
yd = IP85LO_Y_DEV(y);

MEM_SOFT_REG(&lun, &impm1, &1);		/* read soft regs */

if (function == WRITE_PIXEL) {
   MEM_BIT_PLANE_MASK(&lun, &impm1, &mask);
   MEMWR_SEQ(&lun, &0, &xd, &yd, &impm1, &1, pixel);
}
else
   MEMRD_SEQ(&lun, &0, &xd, &yd, &impm1, &1, pixel);

return (SUCCESS);
}
