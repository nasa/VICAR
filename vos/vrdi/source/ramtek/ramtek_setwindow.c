/*	RAMTEK_SetWindow - description
 *
 *	Purpose:
 *
 *	Written by:
 *	Date:
 *
 *	Calling Sequence:
 *
 *		STATUS = RAMTEK_SetWindow( parameters )
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

FUNCTION RAMTEK_SetWindow( Unit, Imp, Left, Top )
int	*Unit, Imp, Left, Top;

{
  SETDW_LEFT = Left - 1;
  SETDW_TOP = Top - 1;

  rmout( &RM_Channel_No, &RM_Set_Window, &SETDW_WORDS );

   return (SUCCESS);
}
