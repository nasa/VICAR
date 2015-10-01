/*	IVAS_Pixel - description
 *
 *	Purpose:
 *
 *	Written by:
 *	Date:
 *
 *	Calling Sequence:
 *
 *		STATUS = IVAS_Pixel( parameters )
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

#define MAKE_EVEN(Val) ((Val) & 0xFFFFFFFE)
#define ODD_TEST(Val) (((Val) & 1) == 1)

FUNCTION IVAS_Pixel( Unit, Function, Imp, X, Y, pixel, mask )
int	*Unit, Function, Imp, X, Y;
char	*pixel,mask;

{
  int	IVAS_Mask, IVAS_OP, IVAS_x;
  int	*IVAS_Addr;
  char	IVAS_Pixel[2];

  IVAS_Addr = &IVAS_Pixel;

  if (Function == WRITE_PIXEL) {
    IVAS_OP = PassOut;
    IVAS_Mask = ALL_BITS;
    if (Imp == OVERLAY_IMP) {
      IVAS_x = MAKE_EVEN(IVAS_X_IMG(X));
      IVASmaGraphics( IVAS_Addr, 2, IVAS_OP, 2, 
		IVAS_x, IVAS_Y_IMG(Y), Imp-1, IVAS_Mask );
      IVAS_Pixel[ODD_TEST(IVAS_X_IMG(X))] = *pixel;
    }
    else {
      IVAS_x = MAKE_EVEN(IVAS_X_IMG(X));
      IVASmaImage( IVAS_Addr, 2, IVAS_OP, PassByte, 2, 
		IVAS_x, IVAS_Y_IMG(Y), Imp-1, IVAS_Mask, 0 );
      IVAS_Pixel[ODD_TEST(IVAS_X_IMG(X))] = *pixel;
    }
    IVAS_OP    = PassIn;
    IVAS_Mask  = mask;
  }
  else {
    IVAS_OP = PassOut;
    IVAS_Mask = ALL_BITS;
  }
  if (Imp == OVERLAY_IMP) {
    IVAS_x = MAKE_EVEN(IVAS_X_IMG(X));
    IVASmaGraphics( IVAS_Addr, 2, IVAS_OP, 2, 
		IVAS_x, IVAS_Y_IMG(Y), Imp-1, IVAS_Mask );
    if (Function == READ_PIXEL) {
      *pixel = IVAS_Pixel[ODD_TEST(IVAS_X_IMG(X))];
    }
  }
  else {
    IVAS_x = MAKE_EVEN(IVAS_X_IMG(X));
    IVASmaImage( IVAS_Addr, 2, IVAS_OP, PassByte, 2, 
		IVAS_x, IVAS_Y_IMG(Y), Imp-1, IVAS_Mask, 0 );
    if (Function == READ_PIXEL) {
      *pixel = IVAS_Pixel[ODD_TEST(IVAS_X_IMG(X))];
    }
  }

  IVASflush();

  return (SUCCESS);
}
