/*	IP85LO_CursorColor - description
 *
 *	Purpose: Change the cursor color for the DeAnza display device.
 *
 *	Written by: Bob Deen
 *	Date:	    January 4, 1990
 *
 *	Calling Sequence:
 *		STATUS = IP85LO_CursorColor(Unit, lun, cursor, red, green, blue)
 *
 *	Parameter List:
 *		Unit:	Display device unit number
 *		lun:	DeAnza logical unit number
 *		cursor: Cursor number (ignored; all cursors must be same color)
 *		red:	Red value for the cursor
 *		green:	Green value for the cursor
 *		blue:	Blue value for the cursor
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

FUNCTION IP85LO_CursorColor(Unit, lun, cursor, red, green, blue)
int *Unit;
short lun;
int cursor;
int red, green, blue;
{
short lut[256];
short i, addr, value;

/* Copy color value from input to overlay LUT format */

value = ((red>>4)&0x000F) | ((green<<1)&0x1E0) | ((blue<<6)&0x3C00);
value |= 0x4210;	/* enable display */

for (i=0; i<256; i++)
   lut[i] = value;

addr = 0;	/* section # is in bits 9,10 */
VOCWR_OVERLAY_LUT(&lun, &VOCBUF, &addr, &256, lut);

return (SUCCESS);

}
