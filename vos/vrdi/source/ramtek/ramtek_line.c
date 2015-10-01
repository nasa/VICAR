/*	RAMTEK_Line - description
 *
 *	Purpose:
 *
 *	Written by:
 *	Date:
 *
 *	Calling Sequence:
 *
 *		STATUS = RAMTEK_Line( parameters )
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
#define ODD_TEST(Val) (((Val) & 1) == 1)

FUNCTION RAMTEK_Line( Unit, Function, Imp, X, Y, length, buf, mask )
int	*Unit, Function, Imp, X, Y, length, buf;
unsigned char	mask;

{
  int RAMTEK_length;
  short data_words;
  unsigned char *last;

  IMAGE_X = X - 1;
  IMAGE_Y = Y - 1;

  IMAGE_X_MIN = AW_LEFT( Imp ) - 1;
  IMAGE_Y_MIN = AW_TOP( Imp ) - 1;
  IMAGE_X_MAX = AW_RIGHT( Imp ) - 1;
  IMAGE_Y_MAX = AW_BOTTOM( Imp ) - 1;

  IMAGE_MASK1 = mask;
  IMAGE_MASK2 = 0;

  RAMTEK_length = length;
  if ( ODD_TEST( RAMTEK_length )) RAMTEK_length--;
  IMAGE_LENGTH = RAMTEK_length;
  data_words = RAMTEK_length / 2;

  if (Function == WRITE_LINE) {

    if ( RAMTEK_length > 0 ) {
      IMAGE_OP = WRITE_IMAGE;
      rmout2( &RM_Channel_No, &RM_Image, &IMAGE_WORDS, buf, &data_words );
    }
    if ( length != RAMTEK_length ) {
      RAMTEK_Pixel( Unit, WRITE_PIXEL, Imp, X+length-1, Y, buf+length-1, mask);
    }
  }
  else {
    if ( RAMTEK_length > 0 ) {
      IMAGE_OP = READ_IMAGE;
      rmotin( &RM_Channel_No, &RM_Image, &IMAGE_WORDS, buf, &data_words );
    }

    if ( length != RAMTEK_length ) {
      RAMTEK_Pixel( Unit, READ_PIXEL, Imp, X+length-1, Y, buf+length-1, mask);
    }
  }

  return (SUCCESS);
}
