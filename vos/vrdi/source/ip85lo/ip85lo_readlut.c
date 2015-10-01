/*	IP85LO_ReadLut - description
 *
 *	Purpose: Read a LUT (look-up table) from the DeAnza display device
 *		 into memory.
 *
 *	Written by: Bob Deen
 *	Date:	    August 21, 1987
 *
 *	Calling Sequence:
 *		STATUS = IP85LO_ReadLut(Unit, lun, lut, section, array)
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

#include "ip85lo.h"

FUNCTION IP85LO_ReadLut(Unit, lun, lut, section, array)
int *Unit;
short lun;
int lut, section, array[256];
{
short luttbl[256];
short i, addr, lutm1;

addr = (section-1) << 8;		/* section # is in bits 8,9 */

lutm1 = lut - 1;

VOCRD_IMAGE_LUT(&lun, &VOCBUF, &lutm1, &1, &addr, &256, luttbl);

/* Copy LUT values from short array to int array */

for (i=0; i<256; i++)
   array[i] = luttbl[i] & 0xFF;	/* hi byte should be clear, but just in case */

return (SUCCESS);
}
