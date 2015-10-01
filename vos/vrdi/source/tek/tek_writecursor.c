/*	TEK_WriteCursor - description
 *
 *	Purpose: Sets the position of the cursor on the Tektronix display
 *	         terminal.
 *
 *	Written by: Paul Bartholomew
 *	Date:	    October 18, 1989
 *
 *	Calling Sequence:
 *		STATUS = TEK_WriteCursor(Unit, cursor, x, y)
 *
 *	Parameter List:
 *		Unit:	Display device unit number
 *		cursor:	Cursor number to write to
 *		x:	New X coordinate
 *		y:	New Y coordinate
 *
 *	Possible Error Codes:
 *		none
 *
 */

#include "xdexterns.h"
#include "xdroutines.h"
#include "xderrors.h"
#include "xdfuncs.h"
#include "tek.h"

FUNCTION TEK_WriteCursor(Unit, cursor, x, y)
int *Unit, cursor, x, y;
{
  long		jterm;

  llinit(&jterm);			/* Start STI */
  llcode(TEK);				/* Set mode to TEK */
  llkblk(TEK_ON);			/* Lock keyboard */

  x = TEK_PIXEL2SCREEN(X_VRDI2TEK(x));
  y = TEK_PIXEL2SCREEN(Y_VRDI2TEK(y));
  lltnsg(TEK_CURSOR, x, y);

  llkblk(TEK_OFF);			/* Unlock keyboard when finished */
  llcode(TEK_ANSI);			/* Reset terminal mode to ANSI */
  llstop();
  return (SUCCESS);
}
