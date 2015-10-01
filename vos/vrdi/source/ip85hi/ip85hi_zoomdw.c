/*	IP85HI_ZoomDW - description
 *
 *	Purpose: Zoom an image plane, and set the display window (for
 *		 panning and scrolling) on the DeAnza display device.
 *
 *	Written by: Bob Deen
 *	Date:	    June 17, 1988
 *
 *	Calling Sequence:
 *		STATUS = IP85HI_ZoomDW(Unit, lun, imp, zoom, x, y)
 *
 *	Parameter List:
 *		Unit:	Display device unit number
 *		lun:	DeAnza logical unit number
 *		imp:	Image plane number to zoom
 *		zoom:	Zoom factor
 *			1=1:1
 *			2=2:1 ....  8=8:1
 *		x:	Coordinate of the left side of the display
 *		y:	Coordinate of the top of the display
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

globalvalue BMC, LVR;

#define XREG regs[0]
#define YREG regs[1]

FUNCTION IP85HI_ZoomDW(Unit, lun, imp, zz, xx, yy)
int *Unit;
short lun;
int imp, zz, xx, yy;
{
short regs[2], vocreg;
short iosb[4];
short impm1;
short x, y;
short zoom;

impm1 = imp-1;

zoom = zz-1;
x = IP85HI_X_DEV(xx);
y = IP85HI_Y_DEV(yy);

/* Set up the memory zoom/scroll registers */

XREG = x >> 2;	      /* Must divide X by 4 for hi-res (low 2 bits go in VOC) */
YREG = y;
if (YREG & 0x0400) {	/* top bit of Y scroll goes in X scroll register! */
    YREG &= ~ 0x0400;	/* (because there's only 10 bits in the register) */
    XREG |= 0x0200;
}
XREG |= zoom<<13;	/* Put in zoom */
YREG |= zoom<<13;

ip8qw(BMC, &lun, iosb, 0,0, regs, &4, &imp, &0);

/* Now set up the VOC zoom/scroll register */

vocreg = x & 0x0003;	/* only the low 2 bits of X */
if (zoom != 0) {
    vocreg = 0;		/* must be 0 for zooms other than 1:1 */
    DW_LEFT(imp) = ((xx-1) & ~3) + 1;	/* force to multiple of 4 (one-based) */
}
vocreg |= zoom<<2;

ip8qw(LVR, &lun, iosb, 0,0, &vocreg, &2, &0, &impm1);

return(SUCCESS);

}
