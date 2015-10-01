/*	IP85LO_AutoTrack - description
 *
 *	Purpose: Turn on or off automatic movement of the cursor when the
 *		 trackball is moved on the DeAnza display device.
 *
 *	Written by: Bob Deen
 *	Date:	    August 21, 1987
 *
 *	Calling Sequence:
 *
 *		STATUS = IP85LO_AutoTrack(Unit, lun, function, device, cursor)
 *
 *	Parameter List:
 *		Unit:	Display device unit number
 *		lun:	DeAnza logical unit number
 *		function: Function code, either AUTO_ON or AUTO_OFF
 *		device: Interactive I/O device number (not currently used)
 *		cursor: Cursor number to track
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

globalvalue RPR, LPR;

FUNCTION IP85LO_AutoTrack(Unit, lun, function, device, cursor)
int *Unit;
short lun;
int function, device, cursor;
{
short istat;
short perreg;
short iosb[4];

/* read peripheral input device control register */

istat = ip8qw(RPR, &lun, iosb, 0, 0, &perreg, &2, &0, &1);

/* set auto track flag on/off for the cursor */
if (function == AUTO_ON)
   perreg |= cursor;		/* set bits 0 or 1 */
else
   perreg &= ~cursor;		/* clear bits 0 or 1 */

perreg |= (1<<5);	/* set go bit */

/* Reset device and halt */

istat = ip8qw(LPR, &lun, iosb, 0, 0, &0, &2, &0, &1);

/* Load peripheral input device control register */

istat = ip8qw(LPR, &lun, iosb, 0, 0, &perreg, &2, &0, &1);

return (SUCCESS);

}
