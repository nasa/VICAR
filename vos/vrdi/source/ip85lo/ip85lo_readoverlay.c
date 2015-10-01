/*	IP85LO_ReadOverlay - description
 *
 *	Purpose: Read an overlay LUT from the DeAnza display device into memory.
 *
 *	Written by: Bob Deen
 *	Date:	    August 21, 1987
 *
 *	Calling Sequence:
 *		STATUS = IP85LO_ReadOverlay(Unit, lun, red, green, blue)
 *
 *	Parameter List:
 *		Unit:	Display device unit number
 *		lun:	DeAnza logical unit number
 *		red:	Buffer to receive red values from the LUT
 *		green:	Buffer to receive green values from the LUT
 *		blue:	Buffer to receive blue values from the LUT
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

FUNCTION IP85LO_ReadOverlay(Unit, lun, red, green, blue)
int *Unit;
short lun;
int red[256], green[256], blue[256];
{
short lut[256];
short i, addr;

addr = 3 << 9;	/* section # is in bits 9,10 */

VOCRD_OVERLAY_LUT(&lun, &VOCBUF, &addr, &256, lut);

/* Convert LUT format to the output arrays */

for (i=0; i<256; i++) {
   red[i] = (lut[i]&0x000F) << 4;
   green[i] = (lut[i]&0x01E0) >> 1;
   blue[i] = (lut[i]&0x3C00) >> 6;
}

return (SUCCESS);

}
