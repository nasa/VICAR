/*	TEK_Cursor - description
 *
 *	Purpose: Turn the display of a cursor on/off for the Tektronix display
 *	         terminal.
 *
 *	Written by: Paul Bartholomew
 *	Date:	    October 17, 1989
 *
 *	Calling Sequence:
 *		STATUS = TEK_Cursor(Unit, function, cursor, form, rate,
 *		                    red, green, blue)
 *
 *	Parameter List:
 *		Unit:	   Display device unit number
 *		function:  CURSOR_ON, CURSOR_OFF, or COLOR_CURSOR
 *		cursor:	   Cursor number to turn on/off
 *		form:	   Cursor form
 *			   0 = default (same as 1)
 *			   1 = Full-screen pixel-wide crosshairs
 *		rate:	   Cursor blink rate
 *			   0 = no blink
 *		red:	   red part of 24-bit color index (0-255)
 *		green:	   green part of 24-bit color index (0-255)
 *		blue:	   blue part of 24-bit color index (0-255)
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

FUNCTION TEK_Cursor(Unit, function, cursor, form, rate, red, green, blue)
int *Unit, function, cursor, form, rate, red, green, blue;
{
  long		jterm, tekindex;

  llinit(&jterm);			/* Start STI */
  llcode(TEK);				/* Set mode to TEK */
  llkblk(TEK_ON);			/* Lock keyboard */

  switch (function)
  {
    case CURSOR_ON:
      llvisg(TEK_CURSOR, TEK_ON);	/* Turn cursor plane on */
      break;
    case CURSOR_OFF:
      llvisg(TEK_CURSOR, TEK_OFF);	/* Turn cursor plane off */
      break;
    case COLOR_CURSOR:
      tekindex = rgb2tek(red, green, blue);
      llvwat(TEK_ALL_SURFACES, 0, tekindex);
      break;
  }

  llkblk(TEK_OFF);			/* Unlock keyboard when finished */
  llcode(TEK_ANSI);			/* Reset terminal mode to ANSI */
  llstop();

  return (SUCCESS);
}
