/*	IVAS_CurPosition - description
 *
 *	Purpose:
 *
 *	Written by:
 *	Date:
 *
 *	Calling Sequence:
 *
 *		STATUS = IVAS_CurPosition( parameters )
 *
 *	Parameter List:
 *
 *		Unit:	Display device unit number
 *
 *	Possible Error Codes:
 *
 */

#include "xvmaininc.h"
#include "xdexterns.h"
#include "xdroutines.h"
#include "xderrors.h"
#include "xdfuncs.h"
#include "ivasinc.h"

FUNCTION IVAS_CurPosition( Unit, function, cursor, x, y )
int	*Unit, function, cursor, *x, *y;
{
  struct {
    int IVAS_x;
    int IVAS_y;
  } IVAS_xy;

  int button, tempx, tempy;

  if (function == READ_CURSOR) {
    IVASmouseClear();
    IVASmouseRead( &button, &IVAS_xy, 3, 0 );
    if (CURSOR_FORM(cursor) != -1) {
      *x = IVAS_GPH_X(IVAS_xy.IVAS_x);
      *y = IVAS_GPH_Y(IVAS_xy.IVAS_y);
    }
    else {            /*  Cursor is resizable cursor  */
      *x = IVAS_GPH_X(IVAS_xy.IVAS_x - ((CURSOR_XSIZE(cursor)-1)/2));
      *y = IVAS_GPH_Y(IVAS_xy.IVAS_y + ((CURSOR_YSIZE(cursor)-1)/2));
      if (*x < 1)  *x = 1;
      if (*y < 1)  *y = 1;
    }
  }
  else {              /*  Function = WRITE_CURSOR  */
    if (CURSOR_FORM(cursor) != -1) {
      tempx = (int) x;
      tempy = (int) y;
    }
    else {    /* Cursor is resizable.  Adjust to get center of cursor. */
      tempx = (int) x + ((CURSOR_XSIZE(cursor)-1)/2);
      tempy = (int) y + ((CURSOR_YSIZE(cursor)-1)/2);
    }

    if ((AUTO_TRACK_CURSOR == 0) && (AUTO_TRACK_DEVICE == 0)) {
      IVAS_xy.IVAS_x = IVAS_X_IMG(tempx);
      IVAS_xy.IVAS_y = IVAS_Y_IMG(tempy);
    }
    else {
      IVAS_xy.IVAS_x = IVAS_X_GPH(tempx);
      IVAS_xy.IVAS_y = IVAS_Y_GPH(tempy);
    }

    IVASgphSetCursor( IVAS_xy.IVAS_x, IVAS_xy.IVAS_y );

    IVAS_xy.IVAS_x = IVAS_X_GPH(tempx);
    IVAS_xy.IVAS_y = IVAS_Y_GPH(tempy);
    IVASmousePut( IVAS_xy.IVAS_x, IVAS_xy.IVAS_y, 3 );

    IVASflush();
  }

  return (SUCCESS);
}
