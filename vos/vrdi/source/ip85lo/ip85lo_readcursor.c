/*	IP85LO_ReadCursor - description
 *
 *	Purpose: Read the cursor position from the DeAnza display device
 *
 *	Written by: Bob Deen
 *	Date:	    August 21, 1987
 *
 *	Calling Sequence:
 *		STATUS = IP85LO_ReadCursor(Unit, lun, cursor, x, y)
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

#include "ip85lo.h"

globalvalue RCR;

FUNCTION IP85LO_ReadCursor(Unit, lun, cursor, x, y)
int *Unit;
short lun;
int cursor, *x, *y;
{
short reg, buf[2];

reg = 2*(cursor-1);	/* cursor 1 is in regs (0,1), 2 is in (2,3) */

ip8qw(RCR, &lun, 0,0,0, buf, &4, &0, &reg);

*x = buf[0]+1;
*y = VIDEO_LINES-buf[1];

return (SUCCESS);

}
