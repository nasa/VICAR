/*	RAMTEK_Curposition - description
 *
 *	Purpose:
 *
 *	Written by:
 *	Date:
 *
 *	Calling Sequence:
 *
 *		STATUS = RAMTEK_Curposition( parameters )
 *
 *	Parameter List:
 *
 *		Unit:	Display device unit number
 *
 *	Possible Error Codes:
 *
 */

#include "xdexterns.h"
#include "xdroutines.h"
#include "xderrors.h"
#include "xdfuncs.h"

#include "rtekinc.h"

FUNCTION RAMTEK_CurPosition( Unit, Function, Cursor, X, Y )
int	*Unit, Function, Cursor, *X, *Y;
{
  short curloc[3], RAMTEK_x, RAMTEK_y;

  CURSOR_DEVICE = RM_Cursor_No[Cursor-1];

  if (Function == READ_CURSOR) {
    CURSOR_OP = READ_GLOBAL;
    rmotin( &RM_Channel_No, &RM_Cursor, &CREAD_WORDS, &curloc, &3 );
    *X = curloc[0] + 1;
    *Y = curloc[1] + 1;
  }
  else {
    RAMTEK_x = (int) X;
    RAMTEK_y = (int) Y;
    CURSOR_OP = WRITE_GLOBAL;
    GLOBAL_X = RAMTEK_x - 1;
    GLOBAL_Y = RAMTEK_y - 1;
    CURSOR_STATE = RM_Cursor_State[Cursor-1];
    rmout( &RM_Channel_No, &RM_Cursor, &CWRITE_WORDS );
  }

  return (SUCCESS);
}
