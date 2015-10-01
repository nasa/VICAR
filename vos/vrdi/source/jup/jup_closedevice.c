/*	JUP_CloseDevice - description
 *
 *	Purpose:	To close Jupiter J-Station Display Device
 *
 *	Written by:	Fred Burnette
 *	Date:		September 22, 1989
 *
 *	Calling Sequence:
 *
 *		STATUS = JUP_CloseDevice( Unit )
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

#include "jupinc.h"

FUNCTION JUP_CloseDevice( Unit )

int *Unit;

{
   jfclose(J12file);
   J12file = NULL;

   return (SUCCESS);
}
