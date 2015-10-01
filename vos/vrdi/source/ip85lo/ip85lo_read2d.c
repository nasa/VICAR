/*	IP85LO_Read2d - description
 *
 *	Purpose: Read the trackball and button 1 from the DeAnza display device
 *
 *	Written by: Bob Deen
 *	Date:	    August 21, 1987
 *
 *	Calling Sequence:
 *		STATUS = IP85LO_Read2d(Unit, lun,
 *				       device, xvalue, yvalue, prox, pen )
 *
 *	Parameter List:
 *		Unit:	Display device unit number
 *		lun:	DeAnza logical unit number
 *		device:	Interactive I/O device number (not currently used)
 *		xvalue:	Normalized X value, in the range -1..+1 (floating point)
 *		yvalue: Normalized Y value, in the range -1..+1 (floating point)
 *		prox:	For light pen or digitizing tablet, not used (always 1)
 *		pen:	1=button 1 pressed, 0=button 1 released
 *
 *	Possible Error Codes:
		none
 *
 */
#include "xdexterns.h"
#include "xdroutines.h"
#include "xderrors.h"
#include "xdfuncs.h"

#include "ip85lo.h"

globalvalue RPR;

FUNCTION IP85LO_Read2d(Unit, lun, device, xvalue, yvalue, prox, pen )
int *Unit;
short lun;
int device;
float *xvalue, *yvalue;
int *prox, *pen;
{
short perbuf[4], iosb[4];

ip8qw(RPR, &lun, iosb, 0,0, perbuf, &8, &0, &0);  /* get peripheral status */

/* perbuf[1] bits 6 and 7 contain the peripheral ID (trackball or joystick). */
/* They are ignored at present since there are no joysticks.		     */

*prox = TRUE;
*pen = perbuf[0] & 0x01;	/* check button 1 */

/* Trackball values go from 0..4K, but that makes the cursor wrap 8 times    */
/* around the screen.  First limit it to one screen, then normalize to -1..1 */

if (VIDEO_LINES == 512) {
   *xvalue = (float)((perbuf[2]&511) - 256) / 256.0;
   *yvalue = (float)((perbuf[3]&511) - 256) / 256.0;
}
else {
   *xvalue = (float)((perbuf[2]&1023) - 512) / 512.0;
   *yvalue = (float)((perbuf[3]&1023) - 512) / 512.0;
}

return (SUCCESS);
}
