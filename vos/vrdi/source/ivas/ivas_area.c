/*	IVAS_Area - description
 *
 *	Purpose:
 *
 *	Written by:
 *	Date:
 *
 *	Calling Sequence:
 *
 *		STATUS = IVAS_Area( parameters )
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

#define MAX_TRANSFER 0xFFFF

FUNCTION IVAS_Area( Unit, function, imp, size, area, buffer, mask )
int	*Unit, function, imp, size, *area, *buffer, mask;

{
  int	IVAS_OP, IVAS_Mask, IVAS_width, IVAS_x, IVAS_width2, IVAS_size;
  int   *buf_x, i, Current_Transfer, Transfer, IVAS_y;
  

  if (function == WRITE_AREA) {
    IVAS_OP   = PassIn;
    IVAS_Mask = mask;
  }
  else {
    IVAS_OP   = PassOut;
    IVAS_Mask = ALL_BITS;
  }

  IVAS_width = area[RIGHT] - area[LEFT] + 1;

  IVAS_width2 = MAKE_EVEN(IVAS_width);
  IVAS_size = MAKE_EVEN(size);
  IVAS_x = MAKE_EVEN(IVAS_X_IMG(area[LEFT]));
  IVAS_y = IVAS_Y_IMG(area[TOP]);

  Current_Transfer = MAX_TRANSFER / IVAS_width2 * IVAS_width2;
  IVASsyncRestrict( 0, 0 );

  if (function == WRITE_AREA) {
    if (imp == OVERLAY_IMP) {
      IVASmaGraphics( buffer, IVAS_size, IVAS_OP, IVAS_width2,
			IVAS_x, IVAS_y ,IVAS_Mask );
    }
    else {
      IVASmaImage( buffer, IVAS_size, IVAS_OP, PassByte, IVAS_width2,
			IVAS_x, IVAS_y, imp-1, IVAS_Mask, 0 );
    }
  }
  else {
    for ( i = 0; i < IVAS_size; i+= Current_Transfer ) {
      if (( i + Current_Transfer ) > IVAS_size ) Transfer = IVAS_size - i;
      else Transfer = Current_Transfer;
      buf_x = buffer + (i/4);
      if (imp == OVERLAY_IMP) {
	IVASmaGraphics( buf_x, Transfer, IVAS_OP, IVAS_width2,
			IVAS_x, IVAS_y, IVAS_Mask );
      }
      else {
	IVASmaImage( buf_x, Transfer, IVAS_OP, PassByte, IVAS_width2,
			IVAS_x, IVAS_y, imp-1, IVAS_Mask, 0 );
      }
      IVAS_y = IVAS_y + ( Current_Transfer / IVAS_width2 );
    }
  }
  IVASflush();
  IVASsyncRestrict( 3, 3 );

  return (SUCCESS);
}
