/*	Dummy_Read2d - description
 *
 *	Purpose: Read the trackball and button 1 from the dummy display device
 *
 *	Written by: Paul Bartholomew
 *	Date:	    May 30, 1990
 *
 *	Calling Sequence:
 *		STATUS = Dummy_Read2d(Unit, device, xvalue, yvalue, prox, pen )
 *
 *	Parameter List:
 *		Unit:	Display device unit number
 *		device:	Interactive I/O device number (not currently used)
 *		xvalue:	Normalized X value, in the range -1..+1 (floating point)
 *		yvalue: Normalized Y value, in the range -1..+1 (floating point)
 *		prox:	For light pen or digitizing tablet, not used (always 1)
 *		pen:	1=button 1 pressed, 0=button 1 released
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
#include <math.h>

FUNCTION Dummy_Read2d(Unit, device, xvalue, yvalue, prox, pen )
int *Unit, device;
float *xvalue, *yvalue;
int *prox, *pen;
{
  *prox = TRUE;
  *pen = rand() & 1;

  if (VIDEO_LINES == 512)
  {
     *xvalue = (float)((DUMMY_CURSOR_X(1)) - 256) / 256.0;
     *yvalue = (float)((DUMMY_CURSOR_Y(1)) - 256) / 256.0;
  }
  else
  {
     *xvalue = (float)((DUMMY_CURSOR_X(1)) - 512) / 512.0;
     *yvalue = (float)((DUMMY_CURSOR_Y(1)) - 512) / 512.0;
  }

  return (SUCCESS);
}
