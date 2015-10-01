/*	IP85LO_Graphics - description
 *
 *	Purpose: Turn on or off display of the graphics overlay on the DeAnza
 *		 display device.
 *
 *	Written by: Bob Deen
 *	Date:	    August 21, 1987
 *
 *	Calling Sequence:
 *		STATUS = IP85LO_Graphics(Unit, lun, function)
 *
 *	Parameter List:
 *		Unit:	Display device unit number
 *		lun:	DeAnza logical unit number
 *		function: Function code, either GRAPHICS_ON or GRAPHICS_OFF
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

FUNCTION IP85LO_Graphics(Unit, lun, function)
int *Unit;
short lun;
int function;
{
short addr;
short lut[256];
int i;
short vocdat = { 0x4300 };		/* VOC control register */

addr = 3 << 9;		/* bits 9,10 select the section */

VOCRD_OVERLAY_LUT(&lun, &VOCBUF, &addr, &256, lut);

if (function == GRAPHICS_ON) {
   for (i=0; i<256; i++)
      lut[i] |= 0x4210;		/* enable r,g,b display */
}
else {
   for (i=0; i<256; i++)	/* disable r,g,b display but don't destroy */
      lut[i] &= 0x3DEF;		/* r,g,b values.  Don't just master disable */
				/* graphics - that will disable cursor also */ 
}

lut[0] = 0;	/* make 0 transparent */

VOCWR_OVERLAY_LUT(&lun, &VOCBUF, &addr, &256, lut);

/* section 2 is cursor black for blink... set to same as overlay so cursor */
/* will be truly transparent.	*/

addr = (2<<9);
VOCWR_OVERLAY_LUT(&lun, &VOCBUF, &addr, &256, lut);

/*	Re-enable master graphics just to be sure	*/

VOC_LOAD_REG(&lun, &VOCBUF, &2, &1, &vocdat);

return(SUCCESS);

}
