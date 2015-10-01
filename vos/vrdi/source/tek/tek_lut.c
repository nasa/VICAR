/*	TEK_Lut - description
 *
 *	Purpose: Read or write a look-up table for the Tektronix terminal
 *
 *	Written by: Paul Bartholomew
 *	Date:	    January 24, 1990
 *
 *	Calling Sequence:
 *		STATUS = TEK_Lut(Unit, function, lut, section, array)
 *
 *	Parameter List:
 *		Unit:	  Display device unit number
 *		function: Function code, either READ_LUT or WRITE_LUT
 *		lut:	  Look-up table number
 *		section:  Look-up table section number
 *		array:	  Array of look-up table values
 *
 *	Possible Error Codes:
 *		none
 *
 */

#include "xdexterns.h"
#include "xdroutines.h"
#include "xderrors.h"
#include "xdfuncs.h"
#include "xdsupport.h"
#include "tek.h"

TEK_Lut(Unit, function, lut, section, array)
int	*Unit, function, lut, section, array[];
{
  int	i, size, imp, area[4], status;
  unsigned char  *dummy;

  if (function == WRITE_LUT)
  {
    switch (DEV_TYPE)
    {
      case TEK_4237:
        for (i = 0; i <= MAX_LUT_VALUE; i++)
          TEK_LUT(lut, i) = array[i];
        break;
      case TEK_3D_LEFT:
        for (i = 0; i <= MAX_LUT_VALUE; i++)
          TEK_LUT_3DL(lut, i) = array[i];
        break;
      case TEK_3D_RIGHT:
        for (i = 0; i <= MAX_LUT_VALUE; i++)
          TEK_LUT_3DR(lut, i) = array[i];
        break;
    }

    imp = WHICH_IMP(lut);

    /*  If nothing has yet been written to the image plane, do not  */
    /*  refresh the screen.                                         */

    if (TEK_WRITTEN(imp) == FALSE)
      status = SUCCESS;
    else
      status = TEK_Area(Unit, WRITE_AREA, lut, size, area, dummy, ALL_BITS,
                        TEK_LUT_AREA);
  }
  else					/*  Function = READ_LUT  */
  {
    status = SUCCESS;
    switch (DEV_TYPE)
    {
      case TEK_4237:
        for (i = 0; i <= MAX_LUT_VALUE; i++)
          array[i] = TEK_LUT(lut, i);
        break;
      case TEK_3D_LEFT:
        for (i = 0; i <= MAX_LUT_VALUE; i++)
          array[i] = TEK_LUT_3DL(lut, i);
        break;
      case TEK_3D_RIGHT:
        for (i = 0; i <= MAX_LUT_VALUE; i++)
          array[i] = TEK_LUT_3DR(lut, i);
        break;
    }
  }

  return(status);
}
