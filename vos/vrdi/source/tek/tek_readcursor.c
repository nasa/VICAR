/*	TEK_ReadCursor - description
 *
 *	Purpose: Read the cursor position from the Tektronix terminal
 *
 *	Written by:	Paul Bartholomew
 *	Date:		February 12, 1990
 *
 *	Calling Sequence:
 *		status = TEK_ReadCursor(Unit, cursor, x, y)
 *
 *	Parameter List:
 *		Unit:	Display device unit number
 *		cursor:	Cursor number to read
 *		x:	Returned X value of cursor
 *		y:	Returned Y value of cursor
 *
 *	Possible Error Codes:
 *		none
 *
 */

#include "xdexterns.h"
#include "xdroutines.h"
#include "xderrors.h"
#include "xdfuncs.h"
#include "xdsupport.h"
#include "tek.h"

FUNCTION TEK_ReadCursor(Unit, cursor, x, y)
int	*Unit, cursor, *x, *y;
{ 
  long	jterm;
  char	sig_char, key_char;
  int	jdum1, jdum2, jx, jy;

  llinit(&jterm);			/* Start STI */
  llcode(TEK);				/* Set mode to TEK */
  llkblk(TEK_ON);			/* Lock keyboard */

  llptgn(TEK_TRACKBALL);
  llgtgn(&sig_char, &key_char, &jx, &jy, &jdum1, &jdum2);
  *x = X_TEK2VRDI(TEK_SCREEN2PIXEL(jx));
  *y = Y_TEK2VRDI(TEK_SCREEN2PIXEL(jy));

  llkblk(TEK_OFF);			/* Unlock keyboard when finished */
  llcode(TEK_ANSI);			/* Reset terminal mode to ANSI */
  llstop();
  return (SUCCESS);
}
