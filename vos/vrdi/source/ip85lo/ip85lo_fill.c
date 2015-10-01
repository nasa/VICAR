/*	IP85LO_Fill - description
 *
 *	Purpose: Fill a rectangular area with a constant value on the DeAnza
 *		 display device.
 *
 *	Written by: Bob Deen
 *	Date:	    August 21, 1987
 *
 *	Calling Sequence:
 *		STATUS = IP85LO_Fill(Unit, lun, imp, color, mask, area)
 *
 *	Parameter List:
 *		Unit:	Display device unit number
 *		lun:	DeAnza logical unit number
 *		imp:	Image plane number to fill
 *		color:	Byte value to fill with
 *		mask:	Bit plane mask selecting which planes to modify
 *		area:	Access window array
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

globalvalue LMC, LR, WR;

FUNCTION IP85LO_Fill(Unit, lun, imp, color, mask, area)
int *Unit;
short lun;
int imp, color, mask;
int area[4];
{
short impm1;
short xmin, xmax, ymin, ymax;
short iosb[4], brain_damage;
long cmr0;		/* must be 32 bits */
short reg, dummy;

impm1 = imp - 1;

reg = mask | 0xFF00;		/* set DVP mask to FF */
ip8qw(LMC, &lun, iosb, 0,0, &reg, &2, &impm1, &2);   /* write bit plane mask */

xmin = IP85LO_X_DEV(area[LEFT]);	/* set logical coordinates */
xmax = IP85LO_X_DEV(area[RIGHT]);
ymin = IP85LO_Y_DEV(area[BOTTOM]);	/* Y is reversed (top=511, bottom=0) */
ymax = IP85LO_Y_DEV(area[TOP]);

/* More brain damage.  MEMWR_RECT explicitly sets the bit plane mask to all  */
/* bits on for no apparent reason, so we have to go to level 0 calls!!!	     */

/* We are actually setting two registers here (a cmr takes two 16 bit regs)  */
/* but since the low order register is first, we can just write a longword.  */
/*		***This is VAX specific***				     */
cmr0 = 1<<impm1;
ip8qw(LR, &lun, iosb, 0, 0, &cmr0, &4, &1, &32);

reg = xmin;
ip8qw(LR, &lun, iosb, 0,0, &reg, &2, &1, &16);	/* set X register */
reg = xmax-xmin;
ip8qw(LR, &lun, iosb, 0,0, &reg, &2, &1, &22);	/* set DX register */
reg = ymin;
ip8qw(LR, &lun, iosb, 0,0, &reg, &2, &1, &24);	/* set Y register */
reg = ymax-ymin;
ip8qw(LR, &lun, iosb, 0,0, &reg, &2, &1, &30);	/* set DY register */
reg = color;
ip8qw(LR, &lun, iosb, 0,0, &reg, &2, &1, &12);	/* set foreground register */

ip8qw(WR, &lun, iosb, 0,0, &dummy, &2, &0, &1);	/* write the rectangle */

return (SUCCESS);
}
