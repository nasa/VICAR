/*	JUP_AutoTrack
 *
 *	Purpose:	To turn on/off autotracking so that the graphics
 *			cursor follows the mouse input device
 *
 *	Written by:	Fred Burnette
 *	Date:		December 1989
 *
 *	Calling Sequence:
 *
 *		STATUS = JUP_AutoTrack( parameters )
 *
 *	Parameter List:
 *
 *		Unit:		Display device unit number
 *		Function:	AUTO_ON or AUTO_OFF
 *		Device:		...
 *		Cursor:		...
 *
 *	Possible Error Codes:
 */

#include "xdexterns.h"
#include "xdroutines.h"
#include "xderrors.h"
#include "xdfuncs.h"

#include "jupinc.h"
#include "jupmouse.h"

FUNCTION JUP_AutoTrack( Unit, Function, Device, Cursor )

int *Unit, Function, Device, Cursor;

{
   int status;
   JUP_MESSAGE mess;

   mess.type = MESS_AUTO;
   if (Function == AUTO_ON)
      mess.autot.onoff = TRUE;
   else
      mess.autot.onoff = FALSE;

   status = JUP_SendMessage(Unit, &mess);
   return (status);
}
