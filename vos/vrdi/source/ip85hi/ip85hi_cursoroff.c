/*	IP85HI_CursorOff - description
 *
 *	Purpose: Turn the display of a cursor off on the DeAnza display device
 *
 *	Written by: Bob Deen
 *	Date:	    June 17, 1988
 *
 *	Calling Sequence:
 *		STATUS = IP85HI_CursorOff(Unit, lun, cursor)
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

#include "ip85hi.h"

globalvalue RCR, LCR;

FUNCTION IP85HI_CursorOff(Unit, lun, cursor)
int *Unit;
short lun;
int cursor;
{
short reg;
short iosb[4];

ip8qw(RCR, &lun, iosb, 0,0, &reg, &2, &0, &4);	/* read cursor ctrl reg */

if (cursor == 2)
    reg &= ~ 0x0010;		/* disable cursor 2 */
else
    reg &= ~ 0x0008;		/* disable cursor 1 */

ip8qw(LCR, &lun, iosb, 0,0, &reg, &2, &0, &4);	/* write cursor ctrl reg */

return (SUCCESS);

}
