/*	RAMTEK_AFG - description
 *
 *	Purpose:
 *
 *	Written by:
 *	Date:
 *
 *	Calling Sequence:
 *
 *		STATUS = RAMTEK_AFG( parameters )
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

FUNCTION RAMTEK_AFG( Unit, function )
int	*Unit, function;
{
  if ( function == AFG_ON ) AFG_ACTIVE = TRUE;
  else AFG_ACTIVE = FALSE;

  return( SUCCESS );
}
