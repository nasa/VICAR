/*	IP85HI_Line - description
 *
 *	Purpose: Read or write a single image line to the DeAnza display device.
 *
 *	Written by: Bob Deen
 *	Date:	    June 17, 1988
 *
 *	Calling Sequence:
 *		STATUS = IP85HI_Line(Unit, lun, function,
 *				     imp, x, y, length, buf, mask)
 *
 *	Parameter List:
 *		Unit:	Display device unit number
 *		lun:	DeAnza logical unit number
 *		function: Function code, either READ_LINE or WRITE_LINE
 *					    (or READ_PIXEL or WRITE_PIXEL)
 *		imp:	Image plane number to access
 *		x:	X coordinate of start of line
 *		y:	Y coordinate of line
 *		length: Number of bytes in the line
 *		buf:	Byte buffer containing the line to be read/written
 *		mask:	Bit plane mask for writes.
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

static short cmrdat[] = {0x000F, 0x00F0, 0x0F00, 0xF000};

FUNCTION IP85HI_Line(Unit, lun, function, imp, x, y, length, buf, mask)
int *Unit;
short lun;
int function, imp, x, y, length;
char *buf;
char mask;
{
short impm1;
short reg[18];
short maskreg;
short iosb[4];
int i;

globalvalue RR, LR, BMC, WI, RI;

impm1 = imp - 1;

ip8qw(RR, &lun, iosb, 0,0, reg, &32, &2, &16);

/* Since WI and RI only work with CMR 0, we must set it up first! */

reg[0] = IP85HI_X_DEV(x);			/* set up coordinates */
reg[8] = IP85HI_Y_DEV(y);
reg[16] = cmrdat[impm1];			/* cmr0, low word */
reg[17] = 0;					/* cmr0, high word */

ip8qw(LR, &lun, iosb, 0,0, reg, &36, &2, &16);

if (function == WRITE_LINE || function == WRITE_PIXEL) {
    maskreg = mask;					/* convert to short */
    ip8qw(BMC, &lun, iosb, 0,0, &maskreg, &2, &0, &2);  /* wrt bit plane mask */

    ip8qw(WI, &lun, iosb, 0,0, buf, &length, &1, &0);	/* write the line */
}
else
    ip8qw(RI, &lun, iosb, 0,0, buf, &length, &1, &0);	/* read the line */

return (SUCCESS);
}
