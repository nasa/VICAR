/*	IP85LO_Connect - description
 *
 *	Purpose: Connects an image plane to a LUT (look-up table) on the
 *		 DeAnza display device.
 *
 *	Written by: Bob Deen
 *	Date:	    August 21, 1987
 *
 *	Calling Sequence:
 *		STATUS = IP85LO_Connect(Unit, lun, imp, lut, section, bypass)
 *
 *	Parameter List:
 *		Unit:	Display device unit number
 *		lun:	DeAnza logical unit number
 *		imp:	Image plane number to connect
 *		lut:	LUT number to connect to
 *			1=red, 2=green, 3=blue, 4=graphics
 *		section: Section number of LUT to use (not valid for graphics)
 *		bypass: 1=bypass LUT, 0=use LUT (not valid for graphics)
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

globalvalue LMC;

FUNCTION IP85LO_Connect(Unit, lun, imp, lut, section, bypass)
int *Unit;
short lun;
int imp, lut, section, bypass;
{
int impm1, lutm1, sectionm1;
short iosb[4], xxx;

if (lut == 4) {		/* graphics plane must use section 3 but VOCCTL */
   section = 1;		/* wants zeroes passed for bypass and section */
   bypass = 0;		/* (sections 0-2 are for when cursor is over image) */
}

impm1 = imp - 1;
lutm1 = lut - 1;
sectionm1 = section - 1;
VOC_FCR(&lun, &VOCBUF, &1, &lutm1, &impm1, &bypass, &sectionm1);

if (lut == 4)				/* Set this image plane for graphics */
    xxx = 0x0400;
else					/* Set this image plane for images */
    xxx = 0x0000;
ip8qw(LMC, &lun, iosb, 0,0, &xxx, &2, &impm1, &3);
return (SUCCESS);
}
