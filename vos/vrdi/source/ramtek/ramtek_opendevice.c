/*	RAMTEK_OpenDevice - description
 *
 *	Purpose:
 *
 *	Written by:
 *	Date:
 *
 *	Calling Sequence:
 *
 *		STATUS = RAMTEK_OpenDevice( parameters )
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

FUNCTION RAMTEK_OpenDevice( Unit )
int	*Unit;

{
/*	General Initialization	 */


  rmact( &-1 );

  strncpy( &RM_string, DIB[*Unit]->DeviceName, 4 );
  rmdev( &RM_desc, &RM_Channel_No );
  DEV_UNIT_NO = RM_Channel_No;
  rmopen( &RM_Channel_No );

  rmact( &1 );

  RM_Device[RM_Channel_No] = irmmcp( &RM_Channel_No );

  RM_Cursor_No[0] = irmgcr( &RM_Channel_No );
  RM_Cursor_No[1] = irmgcr( &RM_Channel_No );
  if ((RM_Cursor_No[0] == -1) || (RM_Cursor_No[1] == -1)) N_CURSORS = 0;

/* Pending response from RAMTEK 

  RM_Autotrack_Device = irmgtk( &RM_Channel_No );
  if (RM_Autotrack_Device == -1) AUTO_TRACK_AVAILABLE = FALSE;
*/
  return (SUCCESS);
}
