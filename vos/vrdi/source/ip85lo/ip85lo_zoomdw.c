/*	IP85LO_ZoomDW - description
 *
 *	Purpose: Set the zoom factor and display window of the DeAnza
 *		 display device, allowing panning and scrolling
 *
 *	Written by: Bob Deen
 *	Date:	    August 21, 1987
 *
 *	Calling Sequence:
 *		STATUS = IP85LO_ZoomDW(Unit, lun, imp, z, x, y)
 *
 *	Parameter List:
 *		Unit:	Display device unit number
 *		lun:	DeAnza logical unit number
 *		imp:	Image plane number to set
 *		z:	Zoom factor 1=1:1, 2=2:1, ... 8=8:1
 *		x:	Coordinate of the left side of the display
 *		y:	Coordinate of the top of the display
 *
 *
 */
#include "xdexterns.h"
#include "xdroutines.h"
#include "xderrors.h"
#include "xdfuncs.h"
#include "xdsupport.h"

#include "ip85lo.h"

globalvalue LMC;

FUNCTION IP85LO_ZoomDW(Unit, lun, imp, z, x, y)
int *Unit;
short lun;
int imp, x, y, z;
{
int zoom;
short impm1;
short xx, yy;
short reg[2], iosb[4];

xx = IP85LO_X_DEV(x);
yy = IP85LO_Y_DEV(y);
zoom = z-1;

impm1 = imp-1;

reg[0] = xx & 0x03FF;		/* low 10 bits of scroll */
reg[0] |= (zoom & 0x07)<<13;	/* low 3 bits of zoom */
reg[1] = yy & 0x03FF;		/* low 10 bits of scroll */
reg[1] |= (zoom & 0x07)<<13;	/* low 3 bits of zoom */

ip8qw(LMC, &lun, iosb, 0,0, reg, &4, &impm1, &0);    /* x&y scroll&zoom regs */

if (DEV_TYPE == DEANZA_IP9000) {
   reg[0] = (xx & 0x0C00) >> 8;		/* scroll extension */
   reg[0] |= (yy & 0x0C00);
   reg[1] = (zoom & 0x08) << 5;		/* zoom extension (x part) */
   reg[1] |= (zoom & 0x08) << 6;		/* (y part) */

   ip8qw(LMC, &lun, iosb, 0,0, reg, &5, &impm1, &5); /* scroll/zoom extension */
}

return(SUCCESS);

}
