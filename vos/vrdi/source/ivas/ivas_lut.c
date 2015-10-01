/*	IVAS_Lut - description
 *
 *	Purpose:
 *
 *	Written by:
 *	Date:
 *
 *	Calling Sequence:
 *
 *		STATUS = IVAS_Lut( parameters )
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

FUNCTION IVAS_Lut( Unit, function, lut, section, buffer )
int *Unit, function, lut, section, buffer[256];

{
  int i;
  int start;
  int count = 256;
  int mech;
  int banks;

  if (function == WRITE_LUT) {
    mech = PassIn;
    banks = 3 << (2 * (lut-1));
  }
  else {
    mech = PassOut;
    banks = 1 << (2 * (lut-1));
  }

  start = 256 * (section - 1);
   
  IVASvpOfm( buffer, start, count, mech, banks );
  IVASflush();

  for ( i = 0; i < 256; i++ ) buffer[i] = buffer[i] & 0xFF;

  return (SUCCESS);
}
