/*	IP85HI_Vector - description
 *
 *	Purpose: Draw a set of vectors on the DeAnza display device.
 *
 *	Written by: Bob Deen
 *	Date:	    June 17, 1988
 *
 *	Calling Sequence:
 *		STATUS = IP85HI_Vector(Unit, lun, imp, n, x, y, color, mask)
 *
 *	Parameter List:
 *		Unit:	Display device unit number
 *		lun:	DeAnza logical unit number
 *		imp:	Image plane number to write to
 *		n:	Number of points
 *		x:	Array of X coordinates
 *		y:	Array of Y coordinates
 *		color:	Byte value to write
 *		mask:	Bit plane mask
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

#define MAXVECTORS	256		/* it can actually handle 393 (!) */

globalvalue BMC, WV;

FUNCTION IP85HI_Vector(Unit, lun, imp, n, x, y, color, mask)
int *Unit;
short lun;
int imp, n, *x, *y;
char color;
char mask;
{
short coord[MAXVECTORS*2];
int i, n4, nleft, nn;
int *xx, *yy;
short reg, iosb[4];

reg = mask;					    /* convert to short */
ip8qw(BMC, &lun, iosb, 0,0, &reg, &2, &imp, &2);    /* write bit plane mask */

nleft = n;
xx = x;
yy = y;

while (nleft > 1) {		/* need at least 2 to plot */
    nn = MIN(nleft, MAXVECTORS);
    n4 = nn*4;

    for (i=0; i<nn; i++) {
	coord[i*2]   = IP85HI_X_DEV(*(xx+i));
	coord[i*2+1] = IP85HI_Y_DEV(*(yy+i));
    }

    ip8qw(WV, &lun, iosb, 0,0, coord, &n4, &imp, &color);

    xx += nn-1;		/* use nn-1 so next pass will have starting point */
    yy += nn-1;
    nleft -= nn-1;
}

return (SUCCESS);
}
