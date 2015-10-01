/*	RAMTEK_Cursor - description
 *
 *	Purpose:
 *
 *	Written by:
 *	Date:
 *
 *	Calling Sequence:
 *
 *		STATUS = RAMTEK_Cursor( parameters )
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

FUNCTION RAMTEK_Cursor( Unit, Function, Cursor, Form, Blink )
int	*Unit, Function, Cursor, Form, Blink;

{
  int i;

  CURSOR_DEVICE = RM_Cursor_No[Cursor-1];
  CURSOR_OP = WRITE_GLOBAL;
  GLOBAL_X = CURSOR_X( Cursor ) - 1;
  GLOBAL_Y = CURSOR_Y( Cursor ) - 1;

  if (Function == CURSOR_OFF) {
    RM_Cursor_State[Cursor-1] &= CURSOR_INVISIBLE;
  } else {
    RM_Cursor_State[Cursor-1] |= CURSOR_VISIBLE;
    SET_BLINK( Cursor, Blink );

    for ( i = 0; i < CFONT_NPAIRS; i++ ) {
      CFONT_PAIRS(i) = cur_shapes[Form-1][i];
    }
    CFONT_CURSOR = RM_Cursor_No[Cursor-1];
    rmout( &RM_Channel_No, &RM_CFont, &CFONT_WORDS );
  }
  CURSOR_STATE = RM_Cursor_State[Cursor-1];
  rmout( &RM_Channel_No, &RM_Cursor, &CWRITE_WORDS );

  return (SUCCESS);
}
