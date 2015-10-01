/*	IP85LO_WriteCursor - description
 *
 *	Purpose: Sets the position of the cursor on the DeAnza display device.
 *
 *	Written by: Bob Deen
 *	Date:	    August 21, 1987
 *
 *	Calling Sequence:
 *		STATUS = IP85LO_WriteCursor(Unit, lun, cursor, x, y)
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

#include "ip85lo.h"

globalvalue LCR;

FUNCTION IP85LO_WriteCursor(Unit, lun, cursor, x, y)
int *Unit;
short lun;
int cursor, x, y;
{
short reg, buf[2];
short iosb[4];

reg = 2*(cursor-1);		/* cursor 1 is in regs (0,1), 2 is in (2,3) */

buf[0] = x-1;		/* IP85LO_xy_DEV() is for image not video sizes */
buf[1] = VIDEO_LINES-y;

ip8qw(LCR, &lun, iosb, 0,0, buf, &4, &0, &reg);

return (SUCCESS);

}
