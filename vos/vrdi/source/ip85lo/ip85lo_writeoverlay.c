/*	IP85LO_WriteOverlay - description
 *
 *	Purpose: Write an overlay LUT to the DeAnza display device.
 *
 *	Written by: Bob Deen
 *	Date:	    August 21, 1987
 *
 *	Calling Sequence:
 *		STATUS = IP85LO_WriteOverlay(Unit, lun, red, green, blue)
 *
 *	Parameter List:
 *		Unit:	Display device unit number
 *		lun:	DeAnza logical unit number
 *		red:	Array of red values for the LUT
 *		green:	Array of green values for the LUT
 *		blue:	Array of blue values for the LUT
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

FUNCTION IP85LO_WriteOverlay(Unit, lun, red, green, blue)
int *Unit;
short lun;
int red[256], green[256], blue[256];
{
short lut[256];
short i, addr;

/* Copy LUT values from input arrays to overlay LUT format */

for (i=0; i<256; i++) {
   lut[i]=((red[i]>>4)&0x000F) | ((green[i]<<1)&0x01E0) | ((blue[i]<<6)&0x3C00);
   if (OVERLAY_ON)
      lut[i] |= 0x4210;		/* enable display */
}
lut[0] = 0;	/* make graphics 0 transparent */

addr = 3 << 9;	/* section # is in bits 9,10 */
VOCWR_OVERLAY_LUT(&lun, &VOCBUF, &addr, &256, lut);

/* Section 2 is cursor black for blink... set to same as graphics plane	*/
/* so cursor will be truly transparent (graphics show through on blink)	*/

addr = 2 << 9;
VOCWR_OVERLAY_LUT(&lun, &VOCBUF, &addr, &256, lut);

return (SUCCESS);

}
