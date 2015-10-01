/*	ADAGE_Overlay - description
 *
 *	Purpose:
 *
 *	Written by:
 *	Date:
 *
 *	Calling Sequence:
 *
 *		STATUS = ADAGE_Overlay( parameters )
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

FUNCTION ADAGE_Overlay( Unit, Function, red, green, blue )
int	*Unit, Function, *red, *green, *blue;

{
  int i;

  Set_Word_Mode_DMA;

  if ( Function == WRITE_OVERLAY_LUT ) {
    Graph_Lut[0] = 0x3FCFF3FC;
    for ( i = 1; i < 256; i++ ) {
      Graph_Lut[i] = (red[i] << 2) | (green[i] << 12) | (blue[i] << 22);
    }
    ikbwr( &IK_CODE, &GRAPHICS_LUT, &256, &Graph_Lut );
  }
  else {
    ikbrd( &IK_CODE, &GRAPHICS_LUT, &256, &Graph_Lut );
    for ( i = 1; i < 256; i++ ) {
      red[i] = (Graph_Lut[i] & 0x3FC) >> 2;
      green[i] = (Graph_Lut[i] & 0xFF000) >> 12;
      blue[i] = (Graph_Lut[i] & 0x3FC00000) >> 22;
    }
    red[0] = green[0] = blue[0] = 0;
  }
  return (SUCCESS);
}
