/*	ADAGE_SetWindow - description
 *
 *	Purpose:
 *
 *	Written by:
 *	Date:
 *
 *	Calling Sequence:
 *
 *		STATUS = ADAGE_SetWindow( parameters )
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

FUNCTION ADAGE_SetWindow( Unit, Imp, Left, Top )
int	*Unit, Imp, Left, Top;

{
  int i;

  for ( i = 1; i <= N_LUTS; i++ ) {
    if ( Imp == WHICH_IMP( i ) ) IK_Set_Window ( Unit, &Imp );
  }

  return (MUST_SET_ALL_DWS);
}
