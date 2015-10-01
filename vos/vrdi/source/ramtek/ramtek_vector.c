/*	RAMTEK_Vector - description
 *
 *	Purpose:
 *
 *	Written by:
 *	Date:
 *
 *	Calling Sequence:
 *
 *		STATUS = RAMTEK_Vector( parameters )
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

FUNCTION RAMTEK_Vector( Unit, imp, npts, x, y, value, mask )
int	*Unit, imp, npts, *x, *y, value;
unsigned char	mask;
{
  int VECTOR_WORDS, V_Ptr, i;

  VECTOR_OPCODE = WRITE_VECTOR_LINKED;
  VECTOR_FLAGS = 0x03;
  VECTOR_FLAG_1 = 0x8003;

  VECTOR_MASK1 = mask;
  VECTOR_MASK2 = 0;

  VECTOR_COLOR = value;
  VECTOR_COP_X = x[0] - 1;
  VECTOR_COP_Y = y[0] - 1;

  V_Ptr = 0;
  for ( i = 1; i < npts; i++, V_Ptr++ ) {

    if ( V_Ptr >= MAX_VECTOR_PAIRS ) {

      VECTOR_LENGTH = MAX_VECTOR_PAIRS * 4;
      VECTOR_WORDS = MAX_VECTOR_PAIRS * 2 + HEADER_WORDS;
      rmout ( &RM_Channel_No, &RM_Vector, &VECTOR_WORDS );
      VECTOR_COP_X = x[i-1] - 1;
      VECTOR_COP_Y = y[i-1] - 1;
      V_Ptr = 0;
    }
    VECTOR_X(V_Ptr) = x[i] - 1;
    VECTOR_Y(V_Ptr) = y[i] - 1;
  }

  if ( V_Ptr > 0 ) {
    VECTOR_LENGTH = V_Ptr * 4;
    VECTOR_WORDS = V_Ptr * 2 + HEADER_WORDS;
    rmout ( &RM_Channel_No, &RM_Vector, &VECTOR_WORDS );
  }

  return ( SUCCESS );
}
