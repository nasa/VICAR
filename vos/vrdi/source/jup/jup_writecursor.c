/*	JUP_WriteCursor
 *
 *	Purpose:	To write a cursor position to the Jupiter
 *			J-Station Display Device
 *
 *	Written by:	Fred Burnette
 *	Date:		December 1989
 *
 *	Calling Sequence:
 *
 *		STATUS = JUP_WriteCursor( parameters )
 *
 *	Parameter List:
 *
 *		Unit:		Display device unit number
 *		Cursor:		Cursor Number
 *		X:		X-coordinate to read/write
 *		Y:		Y-coordinate to read/write
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

FUNCTION JUP_WriteCursor( Unit, Cursor, X, Y )

int *Unit, Cursor, X, Y;

{
   int status;
   JUP_MESSAGE mess;

   mess.type = MESS_NEWPOS;
   mess.newpos.x = X;
   mess.newpos.y = Y;

   status = JUP_SendMessage(Unit, &mess);
   return (status);

}
