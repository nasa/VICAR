/*	Dummy_Lut - description
 *
 *	Purpose: Read or write a look-up table for the dummy terminal
 *
 *	Written by: Paul Bartholomew
 *	Date:	    May 30, 1990
 *
 *	Calling Sequence:
 *		STATUS = Dummy_Lut(Unit, function, lut, section, array)
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
 */

#include "xvmaininc.h"
#include "xdexterns.h"
#include "xdroutines.h"
#include "xderrors.h"
#include "xdfuncs.h"
#include "dummy_dev.h"

Dummy_Lut(Unit, function, lut, section, array)
int	*Unit, function, lut, section, array[];
{
  int	i;

  if (function == WRITE_LUT)
  {
    for (i = 0; i <= MAX_LUT_VALUE; i++)
      DUMMY_LUT(lut, i) = array[i];
  }
  else					/*  Function = READ_LUT  */
  {
    for (i = 0; i <= MAX_LUT_VALUE; i++)
      array[i] = DUMMY_LUT(lut, i);
  }

  return(SUCCESS);
}
