/*	JUP_Graphics  description
 *
 *	Purpose: Turn on or off display of the graphics overlay on the Jupiter
 *		 J-station display device.
 *
 *	Written by: Bob Deen
 *	Date:	    February, 1990
 *
 *	Calling Sequence:
 *		STATUS = JUP_Graphics(Unit, function)
 *
 *	Parameter List:
 *		Unit:	Display device unit number
 *		function: Function code, either GRAPHICS_ON or GRAPHICS_OFF
 *
 *	Possible Error Codes:
 *		none
 *
 */

#include "xdexterns.h"
#include "xdroutines.h"
#include "xderrors.h"
#include "xdfuncs.h"

#include "jupinc.h"

FUNCTION JUP_Graphics( Unit, Function )
int *Unit, Function;
{

   if (Function == GRAPHICS_ON)
      j_ormask(0x0F);		/* Set the display mask to all on for overlay */
   else
      j_ormask(0x00);		/* Set the display mask to all off */
   jfflush(J12file);

   JUP_CursorOverlay(Unit);

   return (SUCCESS);
}
