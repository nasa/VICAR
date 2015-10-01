/*	Dummy_Switch - description
 *
 *	Purpose: Read the switches on the dummy device
 *
 *	Written by: Paul Bartholomew
 *	Date:	    May 30, 1990
 *
 *	Calling Sequence:
 *		STATUS = Dummy_Switch(Unit, device, sw, value)
 *
 *	Parameter List:
 *		Unit:	Display device unit number
 *		device:	Interactive I/O device number (not currently used)
 *		sw:	Switch number to read (1-6)
 *		value:	Returned value of the switch (0=off, 1=on)
 *
 *	Possible Error Codes:
 *		none
 *
 */

#include "xvmaininc.h"
#include "xdexterns.h"
#include "xderrors.h"
#include "dummy_dev.h"
#include <math.h>

FUNCTION Dummy_Switch(Unit, device, sw, value)
int *Unit, device, sw, *value;
{
  *value = rand() & 1;

  return (SUCCESS);
}
