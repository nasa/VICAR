/*	name - description
 *
 *	Purpose:
 *
 *	Written by:
 *	Date:
 *
 *	Calling Sequence:
 *
 *		STATUS = name( parameters )
 *
 *	Parameter List:
 *
 *		Unit:	Display device unit number
 *
 *	Possible Error Codes:
 *
 */
#include "xdexterns.h"
#include "xdroutines.h"
#include "xderrors.h"
#include "xdfuncs.h"

#include "rtekinc.h"

FUNCTION RAMTEK_AutoTrack( Unit, function, device, cursor )
int	*Unit, function, device, cursor;

{
  if ( function == AUTO_OFF ) return(DEVICE_CANNOT_DO_IT);

  AUTO_DEVICE = RM_Device[RM_Channel_No];
  AUTO_CURSOR = 1 << RM_Cursor_No[cursor-1];

  rmout( &RM_Channel_No, &RM_Auto, &AUTO_WORDS );

  return (SUCCESS);
}
