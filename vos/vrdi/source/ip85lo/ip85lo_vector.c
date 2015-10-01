/*	IP85LO_Vector - description
 *
 *	Purpose: Draw a set of vectors on the DeAnza display device.
 *
 *	Written by: Bob Deen
 *	Date:	    August 21, 1987
 *
 *	Calling Sequence:
 *		STATUS = IP85LO_Vector(Unit, lun, imp, n, x, y, color, mask)
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

#include "ip85lo.h"

#define MAXVECTORS	256		/* it can actually handle 393 (!) */

globalvalue LR;

FUNCTION IP85LO_Vector(Unit, lun, imp, n, x, y, color, mask)
int *Unit;
short lun;
int imp, n, *x, *y;
char color;
char mask;
{
short impm1;
short coord[MAXVECTORS*2];
int i, n4, nleft, nn;
int *xx, *yy;
long cmr0;		/* must be 32 bits */
short iosb[4];

impm1 = imp - 1;

MEM_SOFT_REG(&lun, &impm1, &1);		/* load soft regs */
MEM_BIT_PLANE_MASK(&lun, &impm1, &mask);

nleft = n;
xx = x;
yy = y;

while (nleft > 1) {		/* need at least 2 to plot */
   nn = MIN(nleft, MAXVECTORS);
   n4 = nn*4;

   for (i=0; i<nn; i++) {
      coord[i*2]   = IP85LO_X_DEV(*(xx+i));
      coord[i*2+1] = IP85LO_Y_DEV(*(yy+i));
   }

/* Now set up CMR register 0.  This should not have to be done if it weren't */
/* for brain damage on the part of DeAnza.  EVERY other MEMWR call uses a    */
/* logical channel number (if it uses a channel at all).  The Vector routine */
/* uses a CMR register number, not a channel number!  This is made even      */
/* worse by the fact that to implement the logical channel number the other  */
/* MEMWR calls MODIFY CMR0!!!!!!  Major brain damage here.                   */

/* Actually, we are setting two registers here (a cmr takes two 16 bit regs) */
/* but since the low order register is first, we can just write a longword.  */
/*		***This is VAX specific***				     */
   cmr0 = 1<<impm1;
   ip8qw(LR, &lun, iosb, 0, 0, &cmr0, &4, &1, &32);

   MEMWR_VECTOR(&lun, &color, &0, &n4, coord, &1);		/* use CMR 0 */

   xx += nn-1;		/* use nn-1 so next pass will have starting point */
   yy += nn-1;
   nleft -= nn-1;
}

return (SUCCESS);
}
