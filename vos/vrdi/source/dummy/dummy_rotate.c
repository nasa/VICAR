/*	Dummy_Rotate - description
 *
 *	Purpose:  Rotate an image plane on the dummy display terminal.
 *
 *	Written by:  Paul Bartholomew
 *	Date:	     May 30, 1990
 *
 *	Calling Sequence:
 *		STATUS = Dummy_Rotate(Unit, imp, area, angle)
 *
 *	Parameter List:
 *		Unit:	Display device unit number
 *		imp:	Image plane number
 *		area:	Access Window array
 *		angle:  Angle to rotate image plane
 *			1 = 180 degrees
 *			2 = -90 degrees
 *			3 = +90 degrees
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
#include <stdio.h>

Dummy_Rotate(Unit, imp, area, angle)
int	*Unit, imp, area[4], angle;
{
  int	nlines, nsamps, windows_not_square=FALSE;
  int	top, bottom, left, right, ctr, status;
  unsigned char	temp;

  nsamps = area[RIGHT] - area[LEFT] + 1;
  nlines = area[BOTTOM] - area[TOP] + 1;

  if (nlines != nsamps)
  {
    windows_not_square = TRUE;
    nlines = nsamps = MIN(nlines, nsamps);
  }

  top = area[TOP];
  bottom = top + nlines - 1;
  left = area[LEFT];
  right = left + nsamps - 1;

  switch (angle)
  {
    case 1:				/* 180 degrees */
      for ( ; bottom < top; top++, bottom--)
      {
        for ( ; left <= right; left++)
        {
          temp = DUMMY_IMP(imp, left, top);
          DUMMY_IMP(imp, left, top) = DUMMY_IMP(imp, left, bottom);
          DUMMY_IMP(imp, left, bottom) = temp;
        }
      }
      break;

    case 2:				/* -90 degrees */
      for ( ; bottom < top; top++, bottom--, left++, right--)
      {
        for (ctr = 0; ctr <= (right-left); ctr++)
        {
          temp = DUMMY_IMP(imp, left+ctr, top);
          DUMMY_IMP(imp, left+ctr, top) = DUMMY_IMP(imp, right, top+ctr);
          DUMMY_IMP(imp, right, top+ctr) = DUMMY_IMP(imp, right-ctr, bottom);
          DUMMY_IMP(imp, right-ctr, bottom) = DUMMY_IMP(imp, left,bottom-ctr);
          DUMMY_IMP(imp, left, bottom-ctr) = temp;
        }
      }
      break;

    case 3: 			/* +90 degrees */
      for ( ; bottom < top; top++, bottom--, left++, right--)
      {
        for (ctr = 0; ctr <= (right-left); ctr++)
        {
          temp = DUMMY_IMP(imp, left+ctr, top);
          DUMMY_IMP(imp, left+ctr, top) = DUMMY_IMP(imp, left, bottom-ctr);
          DUMMY_IMP(imp, left, bottom-ctr) = DUMMY_IMP(imp, right-ctr,bottom);
          DUMMY_IMP(imp, right-ctr, bottom) = DUMMY_IMP(imp, right, top+ctr);
          DUMMY_IMP(imp, right, top+ctr) = temp;
        }
      }
      break;
  }

  if (windows_not_square)
    status = AW_NOT_SQUARE;
  else
    status = SUCCESS;

  return (status);
}
