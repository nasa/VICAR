/*	TEK_Fill - description
 *
 *	Purpose: Fill a rectangular area with a constant value on the
 *		 Tektronix display device.
 *
 *	Written by: Paul Bartholomew
 *	Date:	    September 18, 1989
 *
 *	Calling Sequence:
 *		STATUS = TEK_Fill(Unit, imp, color, mask, area)
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
 *
 */

#include "xdexterns.h"
#include "xdroutines.h"
#include "xderrors.h"
#include "xdfuncs.h"
#include "xdsupport.h"
#include "tek.h"

FUNCTION TEK_Fill(Unit, imp, color, mask, area)
int	 *Unit, imp;
unsigned char color, mask;
int	 area[4];
{
  int   samp, line, nlines, nsamps, status, size, fill_area[4];
  unsigned char *dummy;

  nsamps = area[RIGHT] - area[LEFT] + 1;
  nlines = area[BOTTOM] - area[TOP] + 1;
  size = nsamps * nlines;

  switch (DEV_TYPE)
  {
    case TEK_4237:
      for (line = 0; line < nlines; line++)
        for (samp = 0; samp < nsamps; samp++)
          TEK_IMP(imp, samp+area[LEFT], line+area[TOP]) =
                      (TEK_IMP(imp, samp+area[LEFT], line+area[TOP]) & ~mask) |
                      (color & mask);
      break;
    case TEK_3D_LEFT:
      for (line = 0; line < nlines; line++)
        for (samp = 0; samp < nsamps; samp++)
          TEK_IMP_3DL(imp, samp+area[LEFT], line+area[TOP]) =
                  (TEK_IMP_3DL(imp, samp+area[LEFT], line+area[TOP]) & ~mask) |
                  (color & mask);
      break;
    case TEK_3D_RIGHT:
      for (line = 0; line < nlines; line++)
        for (samp = 0; samp < nsamps; samp++)
          TEK_IMP_3DR(imp, samp+area[LEFT], line+area[TOP]) =
                  (TEK_IMP_3DR(imp, samp+area[LEFT], line+area[TOP]) & ~mask) |
                  (color & mask);
      break;
  }

  /*  If nothing has yet been written to the image plane and the color  */
  /*  is 0 (transparent), do not refresh the screen.                    */

  if ((TEK_WRITTEN(imp) == FALSE) && (color == 0))
    status = SUCCESS;
  else if ((imp == TEK_OVERLAY_IMP) && (TEK_OVERLAY_ON == FALSE))
    status = SUCCESS;
  else
  {
    fill_area[TOP] = area[TOP];
    fill_area[BOTTOM] = area[BOTTOM];
    fill_area[LEFT] = area[LEFT];
    fill_area[RIGHT] = area[RIGHT];
    status = TEK_Area(Unit, WRITE_AREA, imp, size, fill_area, dummy,
                      ALL_BITS, TEK_REFRESH_AREA);
  }

  if ((area[TOP] == 1) && (area[BOTTOM] == N_LINES) && (area[LEFT] == 1) &&
      (area[RIGHT] == N_SAMPS) && (color == 0))
    TEK_WRITTEN(imp) = FALSE;
  else if ((TEK_WRITTEN(imp) == FALSE) && (color == 0))
    TEK_WRITTEN(imp) = FALSE;
  else
    TEK_WRITTEN(imp) = TRUE;

  return (status);
}
