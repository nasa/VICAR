/*	IP85LO_CloseDevice - description
 *
 *	Purpose: Closes the I/O channel to the DeAnza display device
 *
 *	Written by: Bob Deen
 *	Date:	    August 21, 1987
 *
 *	Calling Sequence:
 *		STATUS = IP85LO_CloseDevice(Unit, lun)
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

#include "ip85lo.h"

globalvalue DET;

FUNCTION IP85LO_CloseDevice(Unit, lun)
int *Unit;
short lun;
{
short istat;
short iosb[4];

/* Detatch the unit */

istat = ip8qw(DET, &lun, iosb);

return (SUCCESS);
}
