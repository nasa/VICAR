/*	RAMTEK_Area - description
 *
 *	Purpose:
 *
 *	Written by:
 *	Date:
 *
 *	Calling Sequence:
 *
 *		STATUS = RAMTEK_Area( parameters )
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
#define MAX_TRANSFER 64000

FUNCTION RAMTEK_Area( Unit, function, imp, size, area, buffer, mask )
int	*Unit, function, imp, size, *area, *buffer;
unsigned char	mask;
{
  unsigned short xfer,data_words;
  int *buf_x;
  int first_xfer,i;

  IMAGE_X_MIN = area[LEFT] - 1;
  IMAGE_Y_MIN = area[TOP] - 1;
  IMAGE_X_MAX = area[RIGHT] - 1;
  IMAGE_Y_MAX = area[BOTTOM] - 1;

/*  If the access window defines an odd width, an additional pixel will
    be written per line with WRITE_AREA eventually offsetting everything;
    so let WRITE_LINE do it right */

  if ( (IMAGE_X_MAX - IMAGE_X_MIN + 1) & 1 == 1 ) return( DEVICE_CANNOT_DO_IT );

  IMAGE_X = area[LEFT] - 1;
  IMAGE_Y = area[TOP] - 1;

  IMAGE_MASK1 = mask;
  IMAGE_MASK2 = 0;

  first_xfer = TRUE;
  for ( i = 1; i <= size; i += MAX_TRANSFER ) {
    if (( i + MAX_TRANSFER - 1 ) > size ) {
      xfer = size - i + 1;
    } else {
      xfer = MAX_TRANSFER;
    }

    data_words = ( xfer + 1 ) / 2;
    if (first_xfer) {
      IMAGE_LENGTH = xfer;
      if ( function == WRITE_AREA ) {
	IMAGE_OP = WRITE_IMAGE;
	rmout2( &RM_Channel_No, &RM_Image, &IMAGE_WORDS, buffer, &data_words );
      }
      else {
	IMAGE_OP = READ_IMAGE;
	rmotin( &RM_Channel_No, &RM_Image, &IMAGE_WORDS, buffer, &data_words );
      }
    }
    else {
      buf_x = buffer + (( i - 1 ) / 4);
      IMAGE2_LENGTH = xfer;
      if ( function == WRITE_AREA ) {
	IMAGE2_OP = WRITE_IMAGE;
	rmout2( &RM_Channel_No, &RM_Image2, &IMAGE2_WORDS, buf_x, &data_words );
      }
      else {
	IMAGE2_OP = READ_IMAGE;
	rmotin( &RM_Channel_No, &RM_Image2, &IMAGE2_WORDS, buf_x, &data_words );
      }
    }
    first_xfer = FALSE;
  }
  return (SUCCESS);
}
