/*	Dummy_CloseDevice - description
 *
 *	Purpose:	Releases the device-specific shared memory required
 *			for the dummy display device.
 *
 *	Written by:	Paul Bartholomew
 *	Date:		May 30, 1990
 *
 *	Calling Sequence:
 *		STATUS = Dummy_CloseDevice(Unit)
 *
 *	Parameter List:
 *		Unit:	Display device unit number
 *
 *	Possible Error Codes:
 *		none
 *
 */

#include "xvmaininc.h"
#include "xdexterns.h"
#include "xdroutines.h"
#include "xderrors.h"
#include "xdfuncs.h"

#include "dummy_dev.h"

FUNCTION Dummy_CloseDevice(Unit)
int *Unit;
{
  return (Detach_Shmem(*Unit, &DUMMY_LUTS, SHMEM_DUMMY));
}
