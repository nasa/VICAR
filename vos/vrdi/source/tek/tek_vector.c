/*	TEK_Vector - description
 *
 *	Purpose: Draw a set of vectors on the Tektronix display terminal.
 *
 *	Written by: Paul Bartholomew
 *	Date:	    October 30, 1989
 *
 *	Calling Sequence:
 *		STATUS = TEK_Vector(Unit, imp, n, x, y, color, mask)
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

#include "xdexterns.h"
#include "xdroutines.h"
#include "xderrors.h"
#include "xdfuncs.h"
#include "xdsupport.h"
#include "tek.h"

FUNCTION TEK_Vector(Unit, imp, npts, xcoord, ycoord, color, mask)
int *Unit, imp, npts, *xcoord, *ycoord;
unsigned char color, mask;
{
  int   	i, x, y, sign, lut, line, samp;
  float 	slope, intercept;
  int		pt, xdiff, ydiff, npix, size, status;
  int		xpos[1024], ypos[1024], area[4];
  unsigned char *dummy;

  area[TOP] = ycoord[0];
  area[BOTTOM] = ycoord[0];
  area[LEFT] = xcoord[0];
  area[RIGHT] = xcoord[0];

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
      npix = ABS(ydiff) + 1;
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
      npix = ABS(xdiff) + 1;
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

      if (ABS(xdiff) > ABS(ydiff))
      {
        if (xdiff < 0)
          sign = -1;
        npix = ABS(xdiff) + 1;
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
        npix = ABS(ydiff) + 1;
        for (pt = 1; pt < npix; pt++)
        {
          ypos[pt] = ycoord[i-1] + (sign * pt);
          xpos[pt] = (int)(((float)ypos[pt] - intercept) / slope);
        }
      }
    }
 
    /*  Put the pixel values into the stored image memory plane  */
    for (pt = 0; pt < npix; pt++)
    {
      switch (DEV_TYPE)
      {
        case TEK_4237:
          TEK_IMP(imp, xpos[pt], ypos[pt]) = (TEK_IMP(imp, xpos[pt], ypos[pt]) &
                                             ~mask) | (color & mask);
          break;
        case TEK_3D_LEFT:
          TEK_IMP_3DL(imp, xpos[pt], ypos[pt]) = 
                               (TEK_IMP_3DL(imp, xpos[pt], ypos[pt]) & ~mask) | 
                               (color & mask);
          break;
        case TEK_3D_RIGHT:
          TEK_IMP_3DR(imp, xpos[pt], ypos[pt]) = 
                               (TEK_IMP_3DR(imp, xpos[pt], ypos[pt]) & ~mask) | 
                               (color & mask);
          break;
      }
    }
    if (xcoord[i] < area[LEFT])    area[LEFT]   = xcoord[i];
    if (xcoord[i] > area[RIGHT])   area[RIGHT]  = xcoord[i];
    if (ycoord[i] < area[TOP])     area[TOP]    = ycoord[i];
    if (ycoord[i] > area[BOTTOM])  area[BOTTOM] = ycoord[i];
  }

  TEK_WRITTEN(imp) = TRUE;
  size = (area[RIGHT]-area[LEFT]+1) * (area[BOTTOM]-area[TOP]+1);

  if ((imp == TEK_OVERLAY_IMP) && (TEK_OVERLAY_ON == FALSE))
    status = SUCCESS;
  else
    status = TEK_Area(Unit, WRITE_AREA, imp, size, area, dummy, ALL_BITS,
                      TEK_REFRESH_AREA);

  return (status);
}
