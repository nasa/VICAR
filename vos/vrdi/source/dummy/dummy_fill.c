/*	Dummy_Fill - description
 *
 *	Purpose: Fill a rectangular area with a constant value.
 *
 *	Written by: Paul Bartholomew
 *	Date:	    May 30, 1990
 *
 *	Calling Sequence:
 *		STATUS = Dummy_Fill(Unit, imp, color, mask, area)
 *
 *	Parameter List:
 *		Unit:	Display device unit number
 *		imp:	Image plane number to fill
 *		color:	Byte value to fill with
 *		mask:	Bit plane mask selecting which planes to modify
 *		area:	Access window array
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

FUNCTION Dummy_Fill(Unit, imp, color, mask, area)
int	 *Unit, imp;
unsigned char color, mask;
int	 area[4];
{
  int   samp, line, nlines, nsamps;

  nsamps = area[RIGHT] - area[LEFT] + 1;
  nlines = area[BOTTOM] - area[TOP] + 1;

  for (line = 0; line < nlines; line++)
    for (samp = 0; samp < nsamps; samp++)
      DUMMY_IMP(imp, samp+area[LEFT], line+area[TOP]) =
                   (DUMMY_IMP(imp, samp+area[LEFT], line+area[TOP]) & ~mask) |
                   (color & mask);

  return (SUCCESS);
}
