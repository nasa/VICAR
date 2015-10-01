/*	IP85HI_Fill - description
 *
 *	Purpose: Fill a rectangular area with a constant value on the DeAnza
 *		 display device.
 *
 *	Written by: Bob Deen
 *	Date:	    June 17, 1988
 *
 *	Calling Sequence:
 *		STATUS = IP85HI_Fill(Unit, lun, imp, color, mask, area)
 *
 *	Parameter List:
 *		Unit:	Display device unit number
 *		lun:	DeAnza logical unit number
 *		imp:	Image plane number to fill
 *		color:	Byte value to fill with
 *		mask:	Bit plane mask selecting which planes to modify
 *		area:	Access window array
 *
 *	Possible Error Codes:
 *		none
 *
 */
#include "xdexterns.h"
#include "xdroutines.h"
#include "xderrors.h"
#include "xdfuncs.h"

#include "ip85hi.h"

globalvalue LR, WR, BMC;

FUNCTION IP85HI_Fill(Unit, lun, imp, color, mask, area)
int *Unit;
short lun;
int imp, color, mask;
int area[4];
{
short xmin, xmax, ymin, ymax;
short iosb[4];
short reg, dummy;

xmin = IP85HI_X_DEV(area[LEFT]);	/* set logical coordinates */
xmax = IP85HI_X_DEV(area[RIGHT]);
ymin = IP85HI_Y_DEV(area[BOTTOM]);	/* Y is reversed (bottom=0) */
ymax = IP85HI_Y_DEV(area[TOP]);

reg = xmin;
ip8qw(LR, &lun, iosb, 0,0, &reg, &2, &1, &16);	/* set X register */
reg = xmax-xmin;
ip8qw(LR, &lun, iosb, 0,0, &reg, &2, &1, &22);	/* set DX register */
reg = ymin;
ip8qw(LR, &lun, iosb, 0,0, &reg, &2, &1, &24);	/* set Y register */
reg = ymax-ymin;
ip8qw(LR, &lun, iosb, 0,0, &reg, &2, &1, &30);	/* set DY register */

reg = color;
ip8qw(LR, &lun, iosb, 0,0, &reg, &2, &1, &12);	/* set foreground register */

reg = mask;
ip8qw(BMC, &lun, iosb, 0,0, &reg, &2, &imp, &2); /* write bit plane mask */

ip8qw(WR, &lun, iosb, 0,0, &dummy, &2, &imp, &1); /* write the rectangle */

return (SUCCESS);
}
