/*	IP85LO_WriteLut - description
 *
 *	Purpose: Write an image LUT to the DeAnza display device
 *
 *	Written by: Bob Deen
 *	Date:	    August 21, 1987
 *
 *	Calling Sequence:
 *		STATUS = IP85LO_WriteLut(Unit, lun, lut, section, array)
 *
 *	Parameter List:
 *		Unit:	Display device unit number
 *		lun:	DeAnza logical unit number
 *		lut:	Look-up table number (1=red, 2=green, 3=blue)
 *		section: Section number of LUT to use
 *		array:	Values to go in the LUT
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

FUNCTION IP85LO_WriteLut(Unit, lun, lut, section, array)
int *Unit;
short lun;
int lut, section, array[256];
{
short luttbl[256];
short i, addr, lutbit;

/* Copy LUT values from int array to short array */

for (i=0; i<256; i++)
   luttbl[i] = array[i];

addr = (section-1) << 8;	/* section # is in bits 8,9 */

/* Figure out which lut to write to */

if (lut == 1)			/* red */
   lutbit = 1;
else if (lut == 2)		/* green */
   lutbit = 2;
else				/* blue */
   lutbit = 4;

VOCWR_IMAGE_LUT(&lun, &VOCBUF, &lutbit, &1, &addr, &256, luttbl);

return (SUCCESS);
}
