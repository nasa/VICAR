/*	Dummy_Line - description
 *
 *	Purpose: Read or write a single image line to the dummy display
 *               terminal.
 *
 *	Written by: Paul Bartholomew
 *	Date:	    May 30, 1990
 *
 *	Calling Sequence:
 *		STATUS = Dummy_Line(Unit, function, imp, x, y, length, buf, mask)
 *
 *	Parameter List:
 *		Unit:	  Display device unit number
 *		function: Function code, either READ_LINE or WRITE_LINE
 *		imp:	  Image plane number to access
 *		x:	  X coordinate of start of line
 *		y:	  Y coordinate of line
 *		length:   Number of bytes in the line
 *		buf:	  Byte buffer containing the line to be read/written
 *		mask:	  Bit plane mask for writes.
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

FUNCTION Dummy_Line(Unit, function, imp, x, y, length, buf, mask)
int *Unit, function, imp, x, y, length;
unsigned char *buf, mask;
{
  int	samp;

  if (function == WRITE_LINE)
  {
    for (samp = 0; samp < length; samp++)
      DUMMY_IMP(imp, x+samp, y) = (DUMMY_IMP(imp, x+samp, y) & ~mask) |
                                  (buf[samp] & mask);
  }
  else					/* Function = READ_LINE */
  {
    for (samp = 0; samp < length; samp++)
      buf[samp] = DUMMY_IMP(imp, x+samp, y);
  }
  return (SUCCESS);
}
