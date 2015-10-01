/*	IP85HI_ReadOverlay - description
 *
 *	Purpose: Read an overlay LUT from the DeAnza display device into memory.
 *
 *	Written by: Bob Deen
 *	Date:	    June 17, 1988
 *
 *	Calling Sequence:
 *		STATUS = IP85HI_ReadOverlay(Unit, lun, red, green, blue)
 *
 *	Parameter List:
 *		Unit:	Display device unit number
 *		lun:	DeAnza logical unit number
 *		red:	Buffer to receive red values from the LUT
 *		green:	Buffer to receive green values from the LUT
 *		blue:	Buffer to receive blue values from the LUT
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

globalvalue RR, LR, RPA;

FUNCTION IP85HI_ReadOverlay(Unit, lun, red, green, blue)
int *Unit;
short lun;
int red[256], green[256], blue[256];
{
short res, saveres, iosb[4];
short channel;
unsigned char luttbl[256];
int tbl[4] = { 0, 255, 127, 0 };	/* convert LUT value to DN */
short lut[256];
short i, addr;

/* Make sure it uses full-word transfers */

ip8qw(RR, &lun, iosb, 0,0, &res, &2, &1, &11);
saveres = res;
res &= ~ 0x40;			/* full-word transfers */
ip8qw(LR, &lun, iosb, 0,0, &res, &2, &1, &11);

/* Section number is already set up, by xdgconnect or xddopen */

channel = (4-1) * 4;		/* first memory channel of graphics plane */

ip8qw(RPA, &lun, iosb, 0,0, &luttbl, &256, &channel, &0);   /* read the ITT's */

ip8qw(LR, &lun, iosb, 0,0, &saveres, &2, &1, &11);   /* restore transfer mode */

/* Convert LUT format to the output arrays */

for (i=0; i<256; i++) {
    luttbl[i] = ~ luttbl[i];	/* un-invert it (all graphics are inverted!) */
    red[i] =   tbl[ luttbl[i]     & 0x0003];
    green[i] = tbl[(luttbl[i]>>2) & 0x0003];
    blue[i] =  tbl[(luttbl[i]>>4) & 0x0003];
}

return (SUCCESS);
}
