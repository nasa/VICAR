/*	TEK_CloseDevice - description
 *
 *	Purpose:	Releases the device-specific shared memory required
 *			to run the VRDI on the Tektronix terminal.
 *
 *	Written by:	Paul Bartholomew
 *	Date:		January 25, 1990
 *
 *	Calling Sequence:
 *		STATUS = TEK_CloseDevice(Unit)
 *
 *	Parameter List:
 *		Unit:	Display device unit number
 *
 *	Possible Error Codes:
 *		none
 *
 */

#include "xdexterns.h"
#include "xdroutines.h"
#include "xderrors.h"
#include "xdfuncs.h"
#include "xdsupport.h"

#include "tek.h"

FUNCTION TEK_CloseDevice(Unit)
int *Unit;
{
  int	status;

  switch(DEV_TYPE)
  {
    case TEK_4237:
      status = Detach_Shmem(*Unit, &TEK_LUTS, SHMEM_TEK);
      break;
    case TEK_3D_LEFT:
      status = Detach_Shmem(*Unit, &TEK_LUTS_3D_LEFT, SHMEM_TEK);
      break;
    case TEK_3D_RIGHT:
      status = Detach_Shmem(*Unit, &TEK_LUTS_3D_RIGHT, SHMEM_TEK);
      break;
  }
  return (status);
}
