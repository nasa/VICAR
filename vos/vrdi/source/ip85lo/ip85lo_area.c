/*	IP85LO_Area - description
 *
 *	Purpose: Read or write an area to the DeAnza display device.
 *
 *	Written by: Bob Deen
 *	Date:	    August 21, 1987
 *
 *	Calling Sequence:
 *		STATUS = IP85LO_Area(Unit, lun, function,
 *				     imp, size, area, buffer, mask)
 *
 *	Parameter List:
 *		Unit:	Display device unit number
 *		lun:	DeAnza logical unit number
 *		function: Function code, either READ_AREA or WRITE_AREA
 *		imp:	Image plane number
 *		size:	Size of buffer to transfer in bytes
 *		area:	Access Window array
 *		buffer:	Image to display
 *		mask:	bit plane mask
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

#define MAXTRANSFER	65534	/* why couldn't they use 0 for 65536??? */

FUNCTION IP85LO_Area(Unit, lun, function, imp, size, area, buffer, mask )
int *Unit;
short lun;
int function;
int imp, size;
int area[4];
unsigned char *buffer;
int mask;
{
short impm1;
short xmin, xmax, ymin, ymax, x, y;
short first;
int sizeleft, s;
unsigned char *buf;

impm1 = imp - 1;

MEM_SOFT_REG(&lun, &impm1, &1);		/* load soft regs */
MEM_BIT_PLANE_MASK(&lun, &impm1, &mask);

xmin = IP85LO_X_DEV(area[LEFT]);	/* set logical coordinates */
xmax = IP85LO_X_DEV(area[RIGHT]);
x = xmin;
ymin = IP85LO_Y_DEV(area[BOTTOM]); /* Y is reversed (top=N_LINES-1, bottom=0) */
ymax = IP85LO_Y_DEV(area[TOP]);
y = ymax;
MEM_LOG_COORD(&lun, &xmin, &xmax, &0, &ymin, &ymax, &0);

first = 0;			/* initialize X,Y first time through */
buf = buffer;
sizeleft = size;

while (sizeleft > 0) {
   s = MIN(sizeleft, MAXTRANSFER);
   if (function == WRITE_AREA)
      MEMWR_SEQ(&lun, &first, &x, &y, &impm1, &s, buf);
   else
      MEMRD_SEQ(&lun, &first, &x, &y, &impm1, &s, buf);
   first = 1;
   buf += s;
   sizeleft -= s;
}

/* restore logical coordinates */

xmin = IP85LO_X_DEV(1);
xmax = IP85LO_X_DEV(N_SAMPS);
ymin = IP85LO_Y_DEV(N_LINES);		/* Y is reversed */
ymax = IP85LO_Y_DEV(1);
MEM_LOG_COORD(&lun, &xmin, &xmax, &0, &ymin, &ymax, &0);

return (SUCCESS);
}
