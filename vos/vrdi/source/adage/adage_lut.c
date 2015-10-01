/*	ADAGE_Lut - description
 *
 *	Purpose:
 *
 *	Written by:
 *	Date:
 *
 *	Calling Sequence:
 *
 *		STATUS = ADAGE_Lut( parameters )
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

#include "ikoninc.h"

FUNCTION ADAGE_Lut( Unit, function, lut, section, buffer )
int *Unit, function, lut, section, buffer[256];

{
  int i, shift, rmask, wmask;

  shift = (lut - 1) * 10 + 2;
  rmask = (0xFF << shift);
  wmask = rmask ^ 0xFFFFFFFF;

  Set_Word_Mode_DMA;

  if ( function == WRITE_LUT ) {
    for ( i = 0; i < 256; i++ ) {
      IK_Lut[i] = (IK_Lut[i] & wmask) | (buffer[i] << shift);
    }
    ikbwr( &IK_CODE, &COLORMAP, &256, &IK_Lut );
  }
  else {
    ikbrd( &IK_CODE, &COLORMAP, &256, &IK_Lut );
    for ( i = 0; i < 256; i++ ) {
      buffer[i] = (IK_Lut[i] & rmask) >> shift;
    }
  }
  return (SUCCESS);
}
