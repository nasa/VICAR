/*	IP85HI_Connect - description
 *
 *	Purpose: Connects an image plane to a LUT (look-up table) on the
 *		 DeAnza display device.
 *
 *	Written by: Bob Deen
 *	Date:	    June 17, 1988
 *
 *	Calling Sequence:
 *		STATUS = IP85HI_Connect(Unit, lun, imp, lut, section, bypass)
 *
 *	Parameter List:
 *		Unit:	Display device unit number
 *		lun:	DeAnza logical unit number
 *		imp:	Image plane number to connect
 *		lut:	LUT number to connect to
 *			1=red, 2=green, 3=blue, 4=graphics
 *		section: Section number of LUT to use
 *		bypass: 1=bypass LUT, 0=use LUT
 *
 *	Possible Error Codes:
 *		 CANNOT_CONNECT_LUT - Returned if an attempt is made to
 *			connect imp n to any lut other than n, since the
 *			hardware is not capable of it.
 *
 *	The hardware cannot connect an image plane n to any lut other than n.
 *	An attempt will produce an error.  This call is useful only for
 *	changing the section number or the bypass flag - not for rearranging
 *	the LUTs.
 *
 */
#include "xdexterns.h"
#include "xdroutines.h"
#include "xderrors.h"
#include "xdfuncs.h"

#include "ip85hi.h"

globalvalue BMC;

FUNCTION IP85HI_Connect(Unit, lun, imp, lut, section, bypass)
int *Unit;
short lun;
int imp, lut, section, bypass;
{
short reg, iosb[4];

if (imp != lut)
    return CANNOT_CONNECT_LUT;		/* HW limitation */

reg = (section-1) << 8;

if (!bypass)
    reg |= 0x1000;

if (imp == 4)				/* graphics plane */
    reg |= 0x0400;

ip8qw(BMC, &lun, iosb, 0,0, &reg, &2, &imp, &3);

return (SUCCESS);
}
