/*	TEK_Rotate - description
 *
 *	Purpose:  Rotate an image plane on the Tektronix display terminal.
 *
 *	Written by:  Paul Bartholomew
 *	Date:	     February 7, 1990
 *
 *	Calling Sequence:
 *		STATUS = TEK_Rotate(Unit, imp, area, angle)
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
 *
 */

#include "xdexterns.h"
#include "xdroutines.h"
#include "xderrors.h"
#include "xdfuncs.h"
#include "xdsupport.h"
#include "tek.h"
#include <stdio.h>

TEK_Rotate(Unit, imp, area, angle)
int	*Unit, imp, area[4], angle;
{
  int	i, nlines, nsamps, size, windows_not_square=FALSE;
  int	top, bottom, left, right, ctr, status, temparea[4];
  unsigned char	temp, *dummy;

  if (TEK_ANGLE(imp) != angle)
  {
    TEK_ANGLE(imp) = angle;

    for (i = 0; i < 3; i++)
      temparea[i] = area[i];

    nsamps = temparea[RIGHT] - temparea[LEFT] + 1;
    nlines = temparea[BOTTOM] - temparea[TOP] + 1;
    if (nlines != nsamps)
    {
      windows_not_square = TRUE;
      nlines = nsamps = MIN(nlines, nsamps);
      temparea[RIGHT] = temparea[LEFT] + nsamps - 1;
      temparea[BOTTOM] = temparea[TOP] + nlines - 1;
    }

    top = temparea[TOP];
    bottom = temparea[BOTTOM];
    left = temparea[LEFT];
    right = temparea[RIGHT];

    size = nsamps * nlines;

    switch (angle)
    {
      case 1:				/* 180 degrees */
      {
        for ( ; bottom < top; top++, bottom--)
        {
          for ( ; left <= right; left++)
          {
            switch (DEV_TYPE)
            {
              case TEK_4237:
                temp = TEK_IMP(imp, left, top);
                TEK_IMP(imp, left, top) = TEK_IMP(imp, left, bottom);
                TEK_IMP(imp, left, bottom) = temp;
                break;
              case TEK_3D_LEFT:
                temp = TEK_IMP_3DL(imp, left, top);
                TEK_IMP_3DL(imp, left, top) = TEK_IMP_3DL(imp, left, bottom);
                TEK_IMP_3DL(imp, left, bottom) = temp;
                break;
              case TEK_3D_RIGHT:
                temp = TEK_IMP_3DR(imp, left, top);
                TEK_IMP_3DR(imp, left, top) = TEK_IMP_3DR(imp, left, bottom);
                TEK_IMP_3DR(imp, left, bottom) = temp;
                break;
            }
          }
        }
        break;
      }
      case 2:				/* -90 degrees */
      {
        for ( ; bottom < top; top++, bottom--, left++, right--)
        {
          for (ctr = 0; ctr <= (right-left); ctr++)
          {
            switch (DEV_TYPE)
            {
              case TEK_4237:
                temp = TEK_IMP(imp, left+ctr, top);
                TEK_IMP(imp, left+ctr, top) = TEK_IMP(imp, right, top+ctr);
                TEK_IMP(imp, right, top+ctr) = TEK_IMP(imp, right-ctr, bottom);
                TEK_IMP(imp, right-ctr, bottom) = TEK_IMP(imp, left,bottom-ctr);
                TEK_IMP(imp, left, bottom-ctr) = temp;
                break;
              case TEK_3D_LEFT:
                temp = TEK_IMP_3DL(imp, left+ctr, top);
                TEK_IMP_3DL(imp, left+ctr, top) = 
                                          TEK_IMP_3DL(imp, right, top+ctr);
                TEK_IMP_3DL(imp, right, top+ctr) =
                                          TEK_IMP_3DL(imp, right-ctr, bottom);
                TEK_IMP_3DL(imp, right-ctr, bottom) =
                                          TEK_IMP_3DL(imp, left, bottom-ctr);
                TEK_IMP_3DL(imp, left, bottom-ctr) = temp;
                break;
              case TEK_3D_RIGHT:
                temp = TEK_IMP_3DR(imp, left+ctr, top);
                TEK_IMP_3DR(imp, left+ctr, top) = 
                                          TEK_IMP_3DR(imp, right, top+ctr);
                TEK_IMP_3DR(imp, right, top+ctr) =
                                          TEK_IMP_3DR(imp, right-ctr, bottom);
                TEK_IMP_3DR(imp, right-ctr, bottom) =
                                          TEK_IMP_3DR(imp, left, bottom-ctr);
                TEK_IMP_3DR(imp, left, bottom-ctr) = temp;
                break;
            }
          }
        }
        break;
      }
      case 3: 			/* +90 degrees */
      {
        for ( ; bottom < top; top++, bottom--, left++, right--)
        {
          for (ctr = 0; ctr <= (right-left); ctr++)
          {
            switch (DEV_TYPE)
            {
              case TEK_4237:
                temp = TEK_IMP(imp, left+ctr, top);
                TEK_IMP(imp, left+ctr, top) = TEK_IMP(imp, left, bottom-ctr);
                TEK_IMP(imp, left, bottom-ctr) = TEK_IMP(imp, right-ctr,bottom);
                TEK_IMP(imp, right-ctr, bottom) = TEK_IMP(imp, right, top+ctr);
                TEK_IMP(imp, right, top+ctr) = temp;
                break;
              case TEK_3D_LEFT:
                temp = TEK_IMP_3DL(imp, left+ctr, top);
                TEK_IMP_3DL(imp, left+ctr, top) =
                                           TEK_IMP_3DL(imp, left, bottom-ctr);
                TEK_IMP_3DL(imp, left, bottom-ctr) =
                                           TEK_IMP_3DL(imp, right-ctr, bottom);
                TEK_IMP_3DL(imp, right-ctr, bottom) =
                                           TEK_IMP_3DL(imp, right, top+ctr);
                TEK_IMP_3DL(imp, right, top+ctr) = temp;
                break;
              case TEK_3D_RIGHT:
                temp = TEK_IMP_3DR(imp, left+ctr, top);
                TEK_IMP_3DR(imp, left+ctr, top) =
                                           TEK_IMP_3DR(imp, left, bottom-ctr);
                TEK_IMP_3DR(imp, left, bottom-ctr) =
                                           TEK_IMP_3DR(imp, right-ctr, bottom);
                TEK_IMP_3DR(imp, right-ctr, bottom) =
                                           TEK_IMP_3DR(imp, right, top+ctr);
                TEK_IMP_3DR(imp, right, top+ctr) = temp;
                break;
            }
          }
        }
        break;
      }
    }

    /*  If nothing has yet been written to the image plane, do not  */
    /*  refresh the screen.                                         */

    if (TEK_WRITTEN(imp) == FALSE)
      status = SUCCESS;
    else
      status = TEK_Area(Unit, WRITE_AREA, imp, size, temparea, dummy, ALL_BITS,
                        TEK_REFRESH_AREA);
  }
  else
    status = SUCCESS;

  if ((windows_not_square) && (status == SUCCESS))
    status = AW_NOT_SQUARE;

  return (status);
}
