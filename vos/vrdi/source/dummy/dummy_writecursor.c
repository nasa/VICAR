/*	Dummy_WriteCursor - description
 *
 *	Purpose: Sets the position of the cursor on the dummy display
 *	         terminal.
 *
 *	Written by: Paul Bartholomew
 *	Date:	    May 30, 1990
 *
 *	Calling Sequence:
 *		STATUS = Dummy_WriteCursor(Unit, cursor, x, y)
 *
 *	Parameter List:
 *		Unit:	Display device unit number
 *		cursor:	Cursor number to write to
 *		x:	New X coordinate
 *		y:	New Y coordinate
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

FUNCTION Dummy_WriteCursor(Unit, cursor, x, y)
int *Unit, cursor, x, y;
{
  DUMMY_CURSOR_X(cursor) = x;
  DUMMY_CURSOR_Y(cursor) = y;

  return (SUCCESS);
}
