/*	JUP_CurPosition
 *
 *	Purpose:	To read a cursor position from the Jupiter
 *			J-Station Display Device
 *
 *	Written by:	Fred Burnette
 *	Date:		December 1989
 *
 *	Calling Sequence:
 *
 *		STATUS = JUP_ReadCursor( parameters )
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

FUNCTION JUP_ReadCursor( Unit, Cursor, X, Y )

int *Unit, Cursor, *X, *Y;

{

   *X = SHMEM->x;
   *Y = SHMEM->y;

   return (SUCCESS);
}
