/*	IP85LO_CursorOff - description
 *
 *	Purpose: Turn the display of a cursor off on the DeAnza display device
 *
 *	Written by: Bob Deen
 *	Date:	    August 21, 1987
 *
 *	Calling Sequence:
 *		STATUS = IP85LO_CursorOff(Unit, lun, cursor)
 *
 *	Parameter List:
 *		Unit:	Display device unit number
 *		lun:	DeAnza logical unit number
 *		cursor:	Cursor number to turn off
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

globalvalue RCR, LCR;

FUNCTION IP85LO_CursorOff(Unit, lun, cursor)
int *Unit;
short lun;
int cursor;
{
short iosb[4];
short reg;

/* disable programmable (hi-order) and fixed-form (low-order bits) cursors */

ip8qw(RCR, &lun, iosb, 0,0, &reg, &2, &0, &4);	/* read cursor control reg */

if (cursor == 2)
   reg &= ~ 0x8300;		/* disable cursor 2 */
else
   reg &= ~ 0x4030;		/* disable cursor 1 */

ip8qw(LCR, &lun, iosb, 0,0, &reg, &2, &0, &4);	/* write it back */

return (SUCCESS);

}
