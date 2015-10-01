/*	JUP_Switch
 *
 *	Purpose:	To read switch settings on mouse of Jupiter J-Station
 *			Display Device
 *
 *	Written by:	Fred Burnette
 *	Date:		December 1989
 *
 *	Calling Sequence:
 *
 *		STATUS = JUP_Switch( parameters )
 *
 *	Parameter List:
 *
 *		Unit:	Display device unit number
 *		Device: ...
 *		Sw:	...
 *		Value:	...
 *
 *	Possible Error Codes:
 *
 */

#include "xdexterns.h"
#include "xdroutines.h"
#include "xderrors.h"
#include "xdfuncs.h"

#include "jupinc.h"
#include "jupmouse.h"

FUNCTION JUP_Switch( Unit, Device, Sw, Value )

int *Unit, Device, Sw, *Value;

{
   *Value = BIT_TEST(SHMEM->buttons, 3-Sw );	/* Make switch 1 the left one */

   return (SUCCESS);
}
