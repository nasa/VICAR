/*	RAMTEK_Zoom_IMP- description
 *
 *	Purpose:
 *
 *	Written by:
 *	Date:
 *
 *	Calling Sequence:
 *
 *		STATUS = RAMTEK_Zoom_IMP( parameters )
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

FUNCTION RAMTEK_Zoom_IMP( Unit, imp, zoom )
int	*Unit, imp, zoom;

{
  ZOOM_LINE = zoom - 1;
  ZOOM_ELEMENT = zoom - 1;

  rmout( &RM_Channel_No, &RM_Zoom, &ZOOM_WORDS );

  return ( SUCCESS );
}
