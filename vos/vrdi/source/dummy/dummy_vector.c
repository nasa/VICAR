/*	Dummy_Vector - description
 *
 *	Purpose: Draw a set of vectors on the dummy terminal.
 *
 *	Written by: Paul Bartholomew
 *	Date:	    May 30, 1990
 *
 *	Calling Sequence:
 *		STATUS = Dummy_Vector(Unit, imp, npts, xcoord, ycoord,
 *                                    color, mask)
 *
 *	Parameter List:
 *		Unit:	Display device unit number
 *		imp:	Image plane number to write to
 *		npts:	Number of points
 *		xcoord:	Array of X coordinates
 *		ycoord:	Array of Y coordinates
 *		color:	Byte value to write
 *		mask:	Bit plane mask
 *
 *	Possible Error Codes:
 *		none
 *
 */

#include "xvmaininc.h"
#include "xdexterns.h"
#include "xdroutines.h"
#include "xderrors.h"
#include "xdfuncs.h"
#include "dummy_dev.h"

FUNCTION Dummy_Vector(Unit, imp, npts, xcoord, ycoord, color, mask)
int *Unit, imp, npts, *xcoord, *ycoord;
unsigned char color, mask;
{
  int   	i, x, y, sign, lut, line, samp;
  float 	slope, intercept;
  int		pt, xdiff, ydiff, npix;
  int		xpos[1024], ypos[1024];

  /*  Carry out the following operations for each vector in the array.      */

  for (i = 1; i < npts; i++)
  {
    sign = 1;

    /*  Find the coordinates of each pixel in the vector.  There are four   */
    /*  possibilities:  1) a single point, 2) a horizontal line, 3) a ver-  */
    /*  tical line, and 4) a slanted line.                                  */

    xdiff = xcoord[i] - xcoord[i-1];
    ydiff = ycoord[i] - ycoord[i-1];

    if ((xdiff == 0) && (ydiff == 0))		/*  A single point  */
    {
      xpos[0] = xcoord[i-1];
      ypos[0] = ycoord[i-1];
      npix = 1;
    }

    else if (xdiff == 0)			/*  A vertical line  */
    {
      if (ydiff < 0)
        sign = -1;
      npix = abs(ydiff) + 1;
      for (pt = 0; pt < npix; pt++)
      {
        xpos[pt] = xcoord[i-1];
        ypos[pt] = ycoord[i-1] + (sign * pt);
      }
    }

    else if (ydiff == 0)			/*  A horizontal line  */
    {
      if (xdiff < 0)
        sign = -1;
      npix = abs(xdiff) + 1;
      for (pt = 0; pt < npix; pt++)
      {
        xpos[pt] = xcoord[i-1] + (sign * pt);
        ypos[pt] = ycoord[i-1];
      }
    }

    else					/*  A slanted line  */
    {
      slope = (float)ydiff / (float)xdiff;
      intercept = (float)ycoord[i] - ((float)xcoord[i] * slope);

      xpos[0] = xcoord[i];
      ypos[0] = ycoord[i];

      if (abs(xdiff) > abs(ydiff))
      {
        if (xdiff < 0)
          sign = -1;
        npix = abs(xdiff) + 1;
        for (pt = 1; pt < npix; pt++)
        {
          xpos[pt] = xcoord[i-1] + (sign * pt);
          ypos[pt] = (int)((slope * xpos[pt]) + intercept);
        }
      }
      else
      {
        if (ydiff < 0)
          sign = -1;
        npix = abs(ydiff) + 1;
        for (pt = 1; pt < npix; pt++)
        {
          ypos[pt] = ycoord[i-1] + (sign * pt);
          xpos[pt] = (int)(((float)ypos[pt] - intercept) / slope);
        }
      }
    }
 
    /*  Put the pixel values into the stored image memory plane  */

    for (pt = 0; pt < npix; pt++)
      DUMMY_IMP(imp, xpos[pt], ypos[pt]) = (DUMMY_IMP(imp, xpos[pt], ypos[pt]) &
                                           ~mask) | (color & mask);
  }

  return (SUCCESS);
}
