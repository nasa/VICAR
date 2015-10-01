/*	IP85HI_CloseDevice - description
 *
 *	Purpose: Closes the I/O channel to the DeAnza display device
 *
 *	Written by: Bob Deen
 *	Date:	    June 17, 1988
 *
 *	Calling Sequence:
 *		STATUS = IP85HI_CloseDevice(Unit, lun)
 *
 *	Parameter List:
 *		Unit:	Display device unit number
 *		lun:	DeAnza logical unit number
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

globalvalue DET;

FUNCTION IP85HI_CloseDevice(Unit, lun)
int *Unit;
short lun;
{
short istat;
short iosb[4];

/* Detatch the unit */

istat = ip8qw(DET, &lun, iosb);

return (SUCCESS);
}
