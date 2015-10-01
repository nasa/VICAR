/*	ADAGE_Cursor - description
 *
 *	Purpose:
 *
 *	Written by:
 *	Date:
 *
 *	Calling Sequence:
 *
 *		STATUS = ADAGE_Cursor( parameters )
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

#include "ikoninc.h"

FUNCTION ADAGE_Cursor( Unit, function, cursor, form, rate )
int	*Unit, function, cursor, form, rate;

{
  Set_Word_Mode_DMA;

  if ( function == CURSOR_ON ) {
    ikbwr( &IK_CODE, &CURSOR_MEMORY, &256, &IK_Cursor[form-1] );
    FBC_CURSOR_ON = TRUE;
  } else FBC_CURSOR_ON = FALSE;

  ikbwr( &IK_CODE, &FBC_VIDEO_CREG, &1, &FBC_VIDEO );

  return (SUCCESS);
}
