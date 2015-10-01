/*	IP85HI_WriteOverlay - description
 *
 *	Purpose: Write an overlay LUT to the DeAnza display device.
 *
 *	Written by: Bob Deen
 *	Date:	    June 17, 1988
 *
 *	Calling Sequence:
 *		STATUS = IP85HI_WriteOverlay(Unit, lun, red, green, blue)
 *
 *	Parameter List:
 *		Unit:	Display device unit number
 *		lun:	DeAnza logical unit number
 *		red:	Array of red values for the LUT
 *		green:	Array of green values for the LUT
 *		blue:	Array of blue values for the LUT
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

/* Maps 0 -> 3, 1-127 -> 2, 128-255 -> 1 */
#define MAP(x) (((x)==0) ? 3 : (((~((x)>>7))&1) + 1))

globalvalue RR, LR, BPA;

FUNCTION IP85HI_WriteOverlay(Unit, lun, red, green, blue)
int *Unit;
short lun;
int red[256], green[256], blue[256];
{
int i;
unsigned char luttbl[256];
short res, saveres, iosb[4];

/* Copy LUT values from input arrays to overlay LUT format/		*/
/* There are three output values: 0, 127, and 255.  Input values of 0	*/
/* map to 0 (encoded as 11), 1-127 map to 127 (encoded as 10), and	*/
/* 128-255 map to 255 (encoded as 01).					*/

for (i=0; i<256; i++)
    luttbl[i] = (MAP(blue[i]) << 4) | (MAP(green[i]) << 2) | MAP(red[i]);

luttbl[0] = 0;	/* make graphics 0 transparent */

for (i=0; i<256; i++)
    luttbl[i] = ~ luttbl[i];		/* must invert LUT for graphics! */

/* Make sure it uses full-word transfers */

ip8qw(RR, &lun, iosb, 0,0, &res, &2, &1, &11);
saveres = res;
res &= ~ 0x40;			/* full-word transfers */
ip8qw(LR, &lun, iosb, 0,0, &res, &2, &1, &11);

ip8qw(BPA, &lun, iosb, 0,0, &luttbl, &256, &4, &0); /* write the ITT's */

ip8qw(LR, &lun, iosb, 0,0, &saveres, &2, &1, &11);   /* restore transfer mode */

return (SUCCESS);
}
