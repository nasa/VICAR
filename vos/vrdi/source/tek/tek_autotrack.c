/*	TEK_AutoTrack - description
 *
 *	Purpose: Turn on or off automatic movement of the cursor when the
 *		 trackball is moved on the Tektronix display terminal.
 *
 *	Written by: Paul Bartholomew
 *	Date:	    October 18, 1989
 *
 *	Calling Sequence:
 *
 *		STATUS = TEK_AutoTrack(Unit, function, device, cursor)
 *
 *	Parameter List:
 *		Unit:	Display device unit number
 *		function: Function code, either AUTO_ON or AUTO_OFF
 *		device: Interactive I/O device number (not currently used)
 *		cursor: Cursor number to track
 *
 *	Possible Error Codes:
 *		none
 *
 */

#include "xdexterns.h"
#include "xdroutines.h"
#include "xderrors.h"
#include "xdfuncs.h"
#include "tek.h"

FUNCTION TEK_AutoTrack(Unit, function, device, cursor)
int *Unit, function, device, cursor;
{
  int	status;

  if (function == AUTO_ON)
    status = SUCCESS;
  else
    status = DEVICE_CANNOT_DO_IT;

  return (status);
}
