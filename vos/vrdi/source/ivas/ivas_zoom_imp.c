/*	IVAS_Zoom_IMP- description
 *
 *	Purpose:
 *
 *	Written by:
 *	Date:
 *
 *	Calling Sequence:
 *
 *		STATUS = IVAS_Zoom_IMP( parameters )
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

FUNCTION IVAS_Zoom_IMP( Unit, imp, zoom )
int	*Unit, imp, zoom;

{
  int status;

  if (( zoom < 1) || ( zoom > MAX_ZOOM_FACTOR )) return ( CANNOT_ZOOM );

  if ( imp == OVERLAY_IMP ) {
    status = SUCCESS;
    IVASvpZoomScroll( -1, -1, zoom, zoom, 2 );
  }
  else {
    status = MUST_ZOOM_ALL;
    IVASvpZoomScroll( -1, -1, zoom, zoom, 1 );
  }
  IVASflush();

  return ( status );
}
