/*	IVAS_Scroll_IMP- description
 *
 *	Purpose:
 *
 *	Written by:
 *	Date:
 *
 *	Calling Sequence:
 *
 *		STATUS = IVAS_Scroll_IMP( parameters )
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

FUNCTION IVAS_Scroll_IMP( Unit, imp, X, Y )
int	*Unit, imp, X, Y;
{
  int status;

  if ( imp == OVERLAY_IMP ) {
    status = SUCCESS;
    IVASvpZoomScroll( IVAS_X_IMG(X), IVAS_Y_IMG(Y), 0, 0, 2 );
  }
  else {
    status = MUST_SET_ALL_DWS;
    IVASvpZoomScroll( IVAS_X_IMG(X), IVAS_Y_IMG(Y), 0, 0, 1 );
  }
  IVASflush();

  return ( status );
}
