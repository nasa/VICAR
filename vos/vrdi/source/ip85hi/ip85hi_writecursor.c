/*	IP85HI_WriteCursor - description
 *
 *	Purpose: Sets the position of the cursor on the DeAnza display device.
 *
 *	Written by: Bob Deen
 *	Date:	    June 17, 1988
 *
 *	Calling Sequence:
 *		STATUS = IP85HI_WriteCursor(Unit, lun, cursor, x, y)
 *
 *	Parameter List:
 *		Unit:	Display device unit number
 *		lun:	DeAnza logical unit number
 *		cursor:	Cursor number to write to
 *		x:	New X coordinate
 *		y:	New Y coordinate
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

globalvalue LCR;

FUNCTION IP85HI_WriteCursor(Unit, lun, cursor, x, y)
int *Unit;
short lun;
int cursor, x, y;
{
short reg, buf[2];

reg = 2*(cursor-1);	/* cursor 1 is in regs (0,1), 2 is in (2,3) */

buf[0] = IP85HI_X_VDEV(x);
buf[1] = IP85HI_Y_VDEV(y);

ip8qw(LCR, &lun, 0,0,0, buf, &4, &0, &reg);

return (SUCCESS);

}
