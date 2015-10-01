/*	IP85HI_Area - description
 *
 *	Purpose: Read or write an area to the DeAnza display device.
 *
 *	Written by: Bob Deen
 *	Date:	    June 17, 1988
 *
 *	Calling Sequence:
 *		STATUS = IP85HI_Area(Unit, lun, function,
 *				     imp, size, area, buffer, mask)
 *
 *	Parameter List:
 *		Unit:	Display device unit number
 *		lun:	DeAnza logical unit number
 *		function: Function code, either READ_AREA or WRITE_AREA
 *		imp:	Image plane number
 *		size:	Size of buffer to transfer in bytes
 *		area:	Access Window array
 *		buffer:	Image to display
 *		mask:	bit plane mask
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

/* There is a hardware bug on the Cartographic Lab's hi-res DeAnza device */
/* which makes large transfers using the area write function unreliable.  */
/* It will occaisionally get an error from the driver during the write,   */
/* which messes up the display.  Therefore, transfers are limited to 1024 */
/* at a time, which seems to be the limit of reliable operation.  Other   */
/* hardware may or may not have this bug.  Try increasing the limit for   */
/* better performance, and see if it works.  The maximum is 65534.  Note, */
/* that's two less than 64K (65536, which is a natural number to use), so */
/* watch out for that.							  */

#define MAXTRANSFER	65534	/* why couldn't they use 0 for 65536??? */

static short cmrdat[] = {0x000F, 0x00F0, 0x0F00, 0xF000};

globalvalue LR, RR, WI, RI, BMC;

FUNCTION IP85HI_Area(Unit, lun, function, imp, size, area, buffer, mask )
int *Unit;
short lun;
int function;
int imp, size;
int area[4];
unsigned char *buffer;
int mask;
{
short xmin, xmax, ymin, ymax, x, y;
short first;
int sizeleft, s;
unsigned char *buf;
short reg, ctrlreg, regs[14];
short iosb[4];

/* Since WI and RI only work with CMR 0, we must set it up first! */
/* (cmr0 is scratch, cmr1-4 are set to memory plane sets 1-4)	  */

ip8qw(LR, &lun, iosb, 0,0, &cmrdat[imp-1], &2, &0, &32);	/* set cmr0 */

if (function == WRITE_AREA) {
    reg = mask;						/* convert to short */
    ip8qw(BMC, &lun, iosb, 0,0, &reg, &2, &0, &2);    /* write bit plane mask */
}

/* Set addressing mode to non-matrix, +X, -Y */

ip8qw(RR, &lun, iosb, 0,0, &ctrlreg, &2, &0, &10);
ctrlreg &= ~ 0x000F;
ctrlreg |= 0x0002;
ip8qw(LR, &lun, iosb, 0,0, &ctrlreg, &2, &0, &10);

/* Read the X and Y window info */

ip8qw(RR, &lun, iosb, 0,0, regs, &28, &0, &16);

xmin = IP85HI_X_DEV(area[LEFT]);	/* set logical coordinates */
xmax = IP85HI_X_DEV(area[RIGHT]);
x = xmin;
ymin = IP85HI_Y_DEV(area[BOTTOM]);	/* Y is reversed (top=511, bottom=0) */
ymax = IP85HI_Y_DEV(area[TOP]);
y = ymax;

regs[0] = x;			/* (reg 16) X register */
regs[2] = xmin;			/* (reg 18) X logical minimum */
regs[3] = xmax;			/* (reg 19) X logical maximum */
regs[4] = 0;			/* (reg 20) X access window minimum */
regs[5] = N_SAMPS-1;		/* (reg 21) X acesss window maximum */

regs[8] = y;			/* (reg 24) Y register */
regs[10] = ymin;		/* (reg 26) Y logical minimum */
regs[11] = ymax;		/* (reg 27) Y logical maximum */
regs[12] = 0;			/* (reg 28) Y access window minimum */
regs[13] = N_LINES-1;		/* (reg 29) Y acesss window maximum */

/* Write the location info */

ip8qw(LR, &lun, iosb, 0,0, regs, &28, &0, &16);

first = 0;			/* initialize X,Y first time through */
buf = buffer;
sizeleft = size;

while (sizeleft > 0) {
    s = MIN(sizeleft, MAXTRANSFER);
    if (function == WRITE_AREA)
	ip8qw(WI, &lun, iosb, 0,0, buf, &s, &1, &first);
    else
	ip8qw(RI, &lun, iosb, 0,0, buf, &s, &1, &first);
   first = 1;
   buf += s;
   sizeleft -= s;
}

/* restore logical coordinates */

regs[2] = 0;			/* (reg 18) X logical minimum */
regs[3] = N_SAMPS-1;		/* (reg 19) X logical maximum */

regs[10] = 0;			/* (reg 26) Y logical minimum */
regs[11] = N_LINES-1;		/* (reg 27) Y logical maximum */

ip8qw(LR, &lun, iosb, 0,0, regs, &28, &0, &16);

return (SUCCESS);
}
