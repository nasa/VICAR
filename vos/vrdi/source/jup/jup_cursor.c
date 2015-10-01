/*	JUP_Cursor
 *
 *	Purpose:	To turn on/off the graphics cursor on the Jupiter
 *			J-Station Display Device
 *
 *	Written by:	Fred Burnette
 *	Date:		December 1989
 *
 *	Calling Sequence:
 *
 *		STATUS = JUP_Cursor( parameters )
 *
 *	Parameter List:
 *
 *		Unit:		Display device unit number
 *		Function:	CURSOR_ON or CURSOR_OFF
 *		Cursor:		Cursor number
 *		Form:		...
 *		Rate:		Blink Rate (N/A)
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

FUNCTION JUP_Cursor( Unit, Function, Cursor, Form, Rate )

int *Unit, Function, Cursor, Form, Rate;

{
   int status;
   JUP_MESSAGE mess;

   mess.type = MESS_ONOFF;

   if (Function == CURSOR_OFF) {
      mess.onoff.onflag = FALSE;
   }
   else {
      mess.onoff.onflag = TRUE;
      mess.onoff.cursor = Cursor;
      mess.onoff.shape = Form-1;
      mess.onoff.rate = Rate;
   }
   status = JUP_SendMessage(Unit, &mess);
   return (status);
}
