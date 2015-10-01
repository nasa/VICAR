/*	IP85HI_ReadLut - description
 *
 *	Purpose: Read a LUT (look-up table) from the DeAnza display device
 *		 into memory.
 *
 *	Written by: Bob Deen
 *	Date:	    June 17, 1988
 *
 *	Calling Sequence:
 *		STATUS = IP85HI_ReadLut(Unit, lun, lut, section, array)
 *
 *	Parameter List:
 *		Unit:	Display device unit number
 *		lun:	DeAnza logical unit number
 *		lut:	LUT number to read (1=red, 2=green, 3=blue)
 *		section: Section number of LUT to read
 *		array:	Buffer for LUT data.
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

globalvalue RR, LR, BMC, RPA;

FUNCTION IP85HI_ReadLut(Unit, lun, lut, section, array)
int *Unit;
short lun;
int lut, section, array[256];
{
unsigned char luttbl[256];
short i, addr, channel;
short res, saveres, iosb[4];

/* Make sure it uses full-word transfers */

ip8qw(RR, &lun, iosb, 0,0, &res, &2, &1, &11);
saveres = res;
res &= ~ 0x40;			/* full-word transfers */
ip8qw(LR, &lun, iosb, 0,0, &res, &2, &1, &11);

addr = (section-1) << 8;	/* section number is in bits 8,9 */
addr |= 0x1000;			/* enable ITT's to VOC */

ip8qw(BMC, &lun, iosb, 0,0, &addr, &2, &lut, &3);

channel = (lut-1) * 4;		/* first memory channel of each plane */
ip8qw(RPA, &lun, iosb, 0,0, &luttbl, &256, &channel, &0);   /* read the ITT's */

ip8qw(LR, &lun, iosb, 0,0, &saveres, &2, &1, &11);   /* restore transfer mode */

addr = (WHICH_SECTION(lut) - 1) << 8;	/* restore the section being used */
addr |= 0x1000;
if (WHICH_SECTION(lut) == 0)		/* bypass was on */
    addr = 0;
ip8qw(BMC, &lun, iosb, 0,0, &addr, &2, &lut, &3);

/* Copy LUT values from byte array to int array */

for (i=0; i<256; i++)
   array[i] = luttbl[i] & 0xFF;	/* hi byte should be clear, but just in case */

return (SUCCESS);
}
