/*	JUP_CursorColor
 *
 *	Purpose:	To change the cursor color (when possible) on the
 *			Jupiter J-Station Display Device
 *
 *	Written by:	Bob Deen
 *	Date:		May 1990
 *
 *	Calling Sequence:
 *
 *		STATUS = JUP_CursorColor( parameters )
 *
 *	Parameter List:
 *
 *		Unit:		Display device unit number
 *		Cursor:		Cursor Number
 *		Red, Green, Blue: Color to use
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

FUNCTION JUP_CursorColor( Unit, Cursor, Red, Green, Blue )

int *Unit, Cursor, Red, Green, Blue;

{
   int status;
   JUP_MESSAGE mess;

   mess.type = MESS_COLOR;
   mess.color.red = Red;
   mess.color.green = Green;
   mess.color.blue = Blue;

   mess.color.curs_dn = zdgrgb(*Unit, Red, Green, Blue);
   mess.color.half_dn = zdgrgb(*Unit, Red/2, Green/2, Blue/2);
   mess.color.black_dn = zdgrgb(*Unit, 0, 0, 0);

   status = JUP_SendMessage(Unit, &mess);
   return (status);

}
