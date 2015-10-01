/*	IP85HI_ReadCursor - description
 *
 *	Purpose: Read the cursor position from the DeAnza display device
 *
 *	Written by: Bob Deen
 *	Date:	    June 17, 1988
 *
 *	Calling Sequence:
 *		STATUS = IP85HI_ReadCursor(Unit, lun, cursor, x, y)
 *
 *	Parameter List:
 *		Unit:	Display device unit number
 *		lun:	DeAnza logical unit number
 *		cursor:	Cursor number to read
 *		x:	Returned X value of cursor
 *		y:	Returned Y value of cursor
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

globalvalue RCR;

FUNCTION IP85HI_ReadCursor(Unit, lun, cursor, x, y)
int *Unit;
short lun;
int cursor, *x, *y;
{
short reg, buf[2];

reg = 2*(cursor-1);	/* cursor 1 is in regs (0,1), 2 is in (2,3) */

ip8qw(RCR, &lun, 0,0,0, buf, &4, &0, &reg);

*x = IP85HI_VDEV_X(buf[0]);
*y = IP85HI_VDEV_Y(buf[1]);

return (SUCCESS);

}
