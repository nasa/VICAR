/*	RAMTEK_Pixel - description
 *
 *	Purpose:
 *
 *	Written by:
 *	Date:
 *
 *	Calling Sequence:
 *
 *		STATUS = RAMTEK_Pixel( parameters )
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

#define MAKE_EVEN(Val) ((Val) & 0xFFFFFFFE)
#define ODD_TEST(Val) (((Val) & 1) == 1)

FUNCTION RAMTEK_Pixel( Unit, Function, Imp, X, Y, pixel, mask )
int	*Unit, Function, Imp, X, Y;
unsigned char	*pixel, mask;
{
  int RAMTEK_x;
  unsigned char Rtek_Pixel[2];

  RAMTEK_x = X - 1;
  IMAGE_X = MAKE_EVEN( RAMTEK_x );
  IMAGE_Y = Y - 1;
  IMAGE_LENGTH = 2;

  IMAGE_MASK1 = mask;
  IMAGE_MASK2 = 0;

  IMAGE_X_MIN = AW_LEFT( Imp ) - 1;
  IMAGE_Y_MIN = AW_TOP( Imp ) - 1;
  IMAGE_X_MAX = AW_RIGHT( Imp ) - 1;
  IMAGE_Y_MAX = AW_BOTTOM( Imp ) - 1;

  if (Function == WRITE_PIXEL) {
    IMAGE_OP = READ_IMAGE;
    rmotin( &RM_Channel_No, &RM_Image, &IMAGE_WORDS, &Rtek_Pixel, &1 );
    Rtek_Pixel[ODD_TEST( RAMTEK_x )] = *pixel;

    IMAGE_OP = WRITE_IMAGE;
    rmout2( &RM_Channel_No, &RM_Image, &IMAGE_WORDS, &Rtek_Pixel, &1 );
  }
  else {
    IMAGE_OP = READ_IMAGE;
    rmotin( &RM_Channel_No, &RM_Image, &IMAGE_WORDS, &Rtek_Pixel, &1 );
    *pixel = Rtek_Pixel[ODD_TEST( RAMTEK_x )];
  }
  return (SUCCESS);
}
