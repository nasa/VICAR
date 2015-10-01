/*	TEK_Overlay - description
 *
 *	Purpose:	Read or write an overlay LUT from the Tektronix display
 *		 	terminal. 
 *
 *	Written by: 	Paul Bartholomew
 *	Date:		February 14, 1990
 *
 *	Calling Sequence:
 *		STATUS = TEK_Overlay(Unit, function, red, green, blue)
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

#include "xdexterns.h"
#include "xdroutines.h"
#include "xderrors.h"
#include "xdfuncs.h"
#include "xdsupport.h"
#include "tek.h"

FUNCTION TEK_Overlay(Unit, function, red, green, blue)
int *Unit, function, red[256], green[256], blue[256];
{
  int	i, size, area[4], status;
  unsigned char  *dummy;

  if (function == WRITE_OVERLAY_LUT)
  {
    switch (DEV_TYPE)
    {
      case TEK_4237:
        for (i = 0; i <= MAX_LUT_VALUE; i++)
          TEK_OVLY_LUT(TEK_RED, i) = red[i];
        for (i = 0; i <= MAX_LUT_VALUE; i++)
          TEK_OVLY_LUT(TEK_GREEN, i) = green[i];
        for (i = 0; i <= MAX_LUT_VALUE; i++)
          TEK_OVLY_LUT(TEK_BLUE, i) = blue[i];
        break;
      case TEK_3D_LEFT:
        for (i = 0; i <= MAX_LUT_VALUE; i++)
          TEK_OVLY_LUT_L(TEK_RED, i) = red[i];
        for (i = 0; i <= MAX_LUT_VALUE; i++)
          TEK_OVLY_LUT_L(TEK_GREEN, i) = green[i];
        for (i = 0; i <= MAX_LUT_VALUE; i++)
          TEK_OVLY_LUT_L(TEK_BLUE, i) = blue[i];
        break;
      case TEK_3D_RIGHT:
        for (i = 0; i <= MAX_LUT_VALUE; i++)
          TEK_OVLY_LUT_R(TEK_RED, i) = red[i];
        for (i = 0; i <= MAX_LUT_VALUE; i++)
          TEK_OVLY_LUT_R(TEK_GREEN, i) = green[i];
        for (i = 0; i <= MAX_LUT_VALUE; i++)
          TEK_OVLY_LUT_R(TEK_BLUE, i) = blue[i];
        break;
    }

    if (TEK_WRITTEN(TEK_OVERLAY_IMP) == FALSE)
      status = SUCCESS;
    else if (TEK_OVERLAY_ON == FALSE)
      status = SUCCESS;
    else
      status = TEK_Area(Unit, WRITE_AREA, TEK_OVERLAY_IMP, size, area,
                        dummy, ALL_BITS, TEK_REFRESH_SCREEN);
  }
  else					/*  Function = READ_OVERLAY_LUT  */
  {
    switch (DEV_TYPE)
    {
      case TEK_4237:
        for (i = 0; i <= MAX_LUT_VALUE; i++)
          red[i] = TEK_OVLY_LUT(TEK_RED, i);
        for (i = 0; i <= MAX_LUT_VALUE; i++)
          green[i] = TEK_OVLY_LUT(TEK_GREEN, i);
        for (i = 0; i <= MAX_LUT_VALUE; i++)
          blue[i] = TEK_OVLY_LUT(TEK_BLUE, i);
        break;
      case TEK_3D_LEFT:
        for (i = 0; i <= MAX_LUT_VALUE; i++)
          red[i] = TEK_OVLY_LUT_L(TEK_RED, i);
        for (i = 0; i <= MAX_LUT_VALUE; i++)
          green[i] = TEK_OVLY_LUT_L(TEK_GREEN, i);
        for (i = 0; i <= MAX_LUT_VALUE; i++)
          blue[i] = TEK_OVLY_LUT_L(TEK_BLUE, i);
        break;
      case TEK_3D_RIGHT:
        for (i = 0; i <= MAX_LUT_VALUE; i++)
          red[i] = TEK_OVLY_LUT_R(TEK_RED, i);
        for (i = 0; i <= MAX_LUT_VALUE; i++)
          green[i] = TEK_OVLY_LUT_R(TEK_GREEN, i);
        for (i = 0; i <= MAX_LUT_VALUE; i++)
          blue[i] = TEK_OVLY_LUT_R(TEK_BLUE, i);
        break;
    }
    status = SUCCESS;
  }
  return (status);
}
