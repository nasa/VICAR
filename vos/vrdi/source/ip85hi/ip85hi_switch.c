/*	IP85HI_Switch - description
 *
 *	Purpose: Read the switches on the DeAnza trackball
 *
 *	Written by: Bob Deen
 *	Date:	    June 17, 1988
 *
 *	Calling Sequence:
 *		STATUS = IP85HI_Switch(Unit, lun, device, sw, value)
 *
 *	Parameter List:
 *		Unit:	Display device unit number
 *		lun:	DeAnza logical unit number
 *		device:	Interactive I/O device number (not currently used)
 *		sw:	Switch number to read (1-6)
 *		value:	Returned value of the switch (0=off, 1=on)
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

globalvalue RPR;

FUNCTION IP85HI_Switch(Unit, lun, device, sw, value)
int *Unit;
short lun;
int device, sw;
int *value;
{
short perbuf[2], iosb[4];

ip8qw(RPR, &lun, iosb, 0,0, perbuf, &4, &0, &0);  /* get peripheral status */

/* perbuf[1] bits 6 and 7 contain the peripheral ID (trackball or joystick). */
/* They are ignored at present since there are no joysticks.		     */

/* There should be 6 switches, but only 4 appear to be connected.  The	     */
/* software supports 6 however, in case the hardware is connected in the     */
/* future.  The extra two should return 0 (open).			     */

*value = BIT_TEST(perbuf[0], sw-1);

return (SUCCESS);
}
