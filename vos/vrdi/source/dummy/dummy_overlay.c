/*	Dummy_Overlay - description
 *
 *	Purpose:	Read or write an overlay LUT from the dummy display
 *		 	terminal. 
 *
 *	Written by: 	Paul Bartholomew
 *	Date:		May 30, 1990
 *
 *	Calling Sequence:
 *		STATUS = Dummy_Overlay(Unit, function, red, green, blue)
 *
 *	Parameter List:
 *		Unit:	   Display device unit number
 *		function:  READ_OVERLAY or WRITE_OVERLAY
 *		red:	   Buffer containing red values
 *		green:	   Buffer containing green values
 *		blue:	   Buffer containing blue values
 *
 *	Possible Error Codes:
 *		none
 */

#include "xvmaininc.h"
#include "xdexterns.h"
#include "xdroutines.h"
#include "xderrors.h"
#include "xdfuncs.h"
#include "dummy_dev.h"

FUNCTION Dummy_Overlay(Unit, function, red, green, blue)
int *Unit, function, red[256], green[256], blue[256];
{
  int	i;

  if (function == WRITE_OVERLAY_LUT)
  {
    for (i = 0; i <= MAX_LUT_VALUE; i++)
      DUMMY_OVLY_LUT(DUMMY_RED, i) = red[i];
    for (i = 0; i <= MAX_LUT_VALUE; i++)
      DUMMY_OVLY_LUT(DUMMY_GREEN, i) = green[i];
    for (i = 0; i <= MAX_LUT_VALUE; i++)
      DUMMY_OVLY_LUT(DUMMY_BLUE, i) = blue[i];
  }
  else					/*  Function = READ_OVERLAY_LUT  */
  {
    for (i = 0; i <= MAX_LUT_VALUE; i++)
      red[i] = DUMMY_OVLY_LUT(DUMMY_RED, i);
    for (i = 0; i <= MAX_LUT_VALUE; i++)
      green[i] = DUMMY_OVLY_LUT(DUMMY_GREEN, i);
    for (i = 0; i <= MAX_LUT_VALUE; i++)
      blue[i] = DUMMY_OVLY_LUT(DUMMY_BLUE, i);
  }
  return (SUCCESS);
}
