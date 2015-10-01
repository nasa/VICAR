/*	Dummy_ReadCursor - description
 *
 *	Purpose: Read the cursor position from the dummy terminal
 *
 *	Written by:	Paul Bartholomew
 *	Date:		May 30, 1990
 *
 *	Calling Sequence:
 *		status = Dummy_ReadCursor(Unit, cursor, x, y)
 *
 *	Parameter List:
 *		Unit:	Display device unit number
 *		cursor:	Cursor number to read
 *		x:	Returned X value of cursor
 *		y:	Returned Y value of cursor
 *
 *	Possible Error Codes:
 *		none
 */

#include "xvmaininc.h"
#include "xdexterns.h"
#include "xdroutines.h"
#include "xderrors.h"
#include "xdfuncs.h"
#include "dummy_dev.h"

FUNCTION Dummy_ReadCursor(Unit, cursor, x, y)
int	*Unit, cursor, *x, *y;
{ 
  *x = DUMMY_CURSOR_X(cursor);
  *y = DUMMY_CURSOR_Y(cursor);

  return (SUCCESS);
}
