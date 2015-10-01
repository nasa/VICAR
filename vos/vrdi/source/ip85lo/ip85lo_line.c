/*	IP85LO_Line - description
 *
 *	Purpose: Read or write a single image line to the DeAnza display device.
 *
 *	Written by: Bob Deen
 *	Date:	    August 21, 1987
 *
 *	Calling Sequence:
 *		STATUS = IP85LO_Line(Unit, lun, function,
 *				     imp, x, y, length, buf, mask)
 *
 *	Parameter List:
 *		Unit:	Display device unit number
 *		lun:	DeAnza logical unit number
 *		function: Function code, either READ_LINE or WRITE_LINE
 *		imp:	Image plane number to access
 *		x:	X coordinate of start of line
 *		y:	Y coordinate of line
 *		length: Number of bytes in the line
 *		buf:	Byte buffer containing the line to be read/written
 *		mask:	Bit plane mask for writes.
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

FUNCTION IP85LO_Line(Unit, lun, function, imp, x, y, length, buf, mask)
int *Unit;
short lun;
int function, imp, x, y, length;
char *buf;
char mask;
{
short impm1;
short xd, yd;

impm1 = imp - 1;
xd = IP85LO_X_DEV(x);
yd = IP85LO_Y_DEV(y);

MEM_SOFT_REG(&lun, &impm1, &1);		/* load soft registers */

if (function == WRITE_LINE) {
   MEM_BIT_PLANE_MASK(&lun, &impm1, &mask);
   MEMWR_SEQ(&lun, &0, &xd, &yd, &impm1, &length, buf);
}
else
   MEMRD_SEQ(&lun, &0, &xd, &yd, &impm1, &length, buf);

return (SUCCESS);
}
