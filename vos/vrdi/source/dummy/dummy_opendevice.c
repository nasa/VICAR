/*	Dummy_OpenDevice - description
 *
 *	Purpose: Set up the dummy display terminal
 *
 *	Written by: Paul Bartholomew
 *	Date:	    May 30, 1990
 *
 *	Calling Sequence:
 *
 *		STATUS = Dummy_OpenDevice(Unit)
 *
 *	Parameter List:
 *		Unit:	Display device unit number
 *
 *	Possible Error Codes:
 *
 */

#include "xvmaininc.h"
#include "xdexterns.h"

FUNCTION Dummy_OpenDevice(Unit)
int *Unit;
{
  return (Dummy_ConfigDevice(Unit, DIB[*Unit]->DefaultConfig));
}
