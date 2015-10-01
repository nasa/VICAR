/*	IP85HI_Graphics - description
 *
 *	Purpose: Turn on or off display of the graphics overlay on the DeAnza
 *		 display device.
 *
 *	Written by: Bob Deen
 *	Date:	    June 17, 1988
 *
 *	Calling Sequence:
 *		STATUS = IP85HI_Graphics(Unit, lun, function)
 *
 *	Parameter List:
 *		Unit:	Display device unit number
 *		lun:	DeAnza logical unit number
 *		function: Function code, either GRAPHICS_ON or GRAPHICS_OFF
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

globalvalue RVR, LVR;

FUNCTION IP85HI_Graphics(Unit, lun, function)
int *Unit;
short lun;
int function;
{
short reg, iosb[4];

ip8qw(RVR, &lun, iosb, 0,0, &reg, &2, &0, &4);	/* read VOC Enable register */

if (function == GRAPHICS_ON)
    reg |= 0x2007;			/* enable graphics */
else
    reg &= ~ 0x2007;			/* disable graphics */

ip8qw(LVR, &lun, iosb, 0,0, &reg, &2, &0, &4);	/* write VOC Enable register */

return(SUCCESS);
}
