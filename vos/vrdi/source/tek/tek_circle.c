/*	TEK_Circle - description
 *
 *	Purpose: Draw a circle on the Tektronix display terminal.
 *
 *	Written by: Paul Bartholomew
 *	Date:	    October 18, 1989
 *
 *	Calling Sequence:
 *		STATUS = TEK_Circle(Unit, imp, xcenter, ycenter, radius,
 *				    color, mask, area)
 *
 *	Parameter List:
 *		Unit:	 Display device unit number
 *		imp:	 Image plane number
 *		xcenter: X coordinate of center of circle
 *		ycenter: Y coordinate of center of circle
 *		radius:  Radius of the circle
 *		color:   Color value (0-255)
 *		mask:	 Bit plane mask
 *		area:    Current access window
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

FUNCTION TEK_Circle(Unit, imp, xcenter, ycenter, radius, color, mask, area)
int *Unit, imp, xcenter, ycenter, radius;
unsigned char color, mask;
int area[4];
{
  int   status, x, y, xpos, ypos, nsamps, nlines, size, d;
  int	xmin=1024, xmax=0, ymin=1024, ymax=0, circle_area[4];
  unsigned char *dummy;

  nsamps = area[RIGHT] - area[LEFT] + 1;
  nlines = area[BOTTOM] - area[TOP] + 1;

  x = 0;
  y = radius;
  d = 3 - (2 * radius);
  while (x <= y)
  {
    xpos = xcenter + x;
    ypos = ycenter + y;
    if ((xpos >= area[LEFT]) && (xpos <= area[RIGHT]) &&
        (ypos >= area[TOP])  && (ypos <= area[BOTTOM]))
    {
      tek_put_pixel(Unit, imp, xpos, ypos, color, mask);
      check_min_max(xpos, ypos, &xmin, &xmax, &ymin, &ymax);
    }

    xpos = xcenter + x;
    ypos = ycenter - y;
    if ((xpos >= area[LEFT]) && (xpos <= area[RIGHT]) &&
        (ypos >= area[TOP])  && (ypos <= area[BOTTOM]))
    {
      tek_put_pixel(Unit, imp, xpos, ypos, color, mask);
      check_min_max(xpos, ypos, &xmin, &xmax, &ymin, &ymax);
    }

    xpos = xcenter - x;
    ypos = ycenter + y;
    if ((xpos >= area[LEFT]) && (xpos <= area[RIGHT]) &&
        (ypos >= area[TOP])  && (ypos <= area[BOTTOM]))
    {
      tek_put_pixel(Unit, imp, xpos, ypos, color, mask);
      check_min_max(xpos, ypos, &xmin, &xmax, &ymin, &ymax);
    }

    xpos = xcenter - x;
    ypos = ycenter - y;
    if ((xpos >= area[LEFT]) && (xpos <= area[RIGHT]) &&
        (ypos >= area[TOP])  && (ypos <= area[BOTTOM]))
    {
      tek_put_pixel(Unit, imp, xpos, ypos, color, mask);
      check_min_max(xpos, ypos, &xmin, &xmax, &ymin, &ymax);
    }

    xpos = xcenter + y;
    ypos = ycenter + x;
    if ((xpos >= area[LEFT]) && (xpos <= area[RIGHT]) &&
        (ypos >= area[TOP])  && (ypos <= area[BOTTOM]))
    {
      tek_put_pixel(Unit, imp, xpos, ypos, color, mask);
      check_min_max(xpos, ypos, &xmin, &xmax, &ymin, &ymax);
    }

    xpos = xcenter + y;
    ypos = ycenter - x;
    if ((xpos >= area[LEFT]) && (xpos <= area[RIGHT]) &&
        (ypos >= area[TOP])  && (ypos <= area[BOTTOM]))
    {
      tek_put_pixel(Unit, imp, xpos, ypos, color, mask);
      check_min_max(xpos, ypos, &xmin, &xmax, &ymin, &ymax);
    }

    xpos = xcenter - y;
    ypos = ycenter + x;
    if ((xpos >= area[LEFT]) && (xpos <= area[RIGHT]) &&
        (ypos >= area[TOP])  && (ypos <= area[BOTTOM]))
    {
      tek_put_pixel(Unit, imp, xpos, ypos, color, mask);
      check_min_max(xpos, ypos, &xmin, &xmax, &ymin, &ymax);
    }

    xpos = xcenter - y;
    ypos = ycenter - x;
    if ((xpos >= area[LEFT]) && (xpos <= area[RIGHT]) &&
        (ypos >= area[TOP])  && (ypos <= area[BOTTOM]))
    {
      tek_put_pixel(Unit, imp, xpos, ypos, color, mask);
      check_min_max(xpos, ypos, &xmin, &xmax, &ymin, &ymax);
    }

    if (d < 0)
    {
      d = d + (4 * x) + 6;
    }
    else
    {
      d = d + (4 * (x - y)) + 10;
      y -= 1;
    }
    x += 1;
  }

  /*  If none of the circle's points were in the access window, then  */
  /*  nothing was written to the image plane.                         */

  if ((ymin > ymax) || (xmin > xmax))
    status = SUCCESS;
  else
  {
    TEK_WRITTEN(imp) = TRUE;

    circle_area[TOP] = ymin;
    circle_area[BOTTOM] = ymax;
    circle_area[LEFT] = xmin;
    circle_area[RIGHT] = xmax;

    size = (xmax - xmin + 1) * (ymax - ymin + 1);

    if ((imp == TEK_OVERLAY_IMP) && (TEK_OVERLAY_ON == FALSE))
      status = SUCCESS;
    else
      status = TEK_Area(Unit, WRITE_AREA, imp, size, circle_area, dummy,
                        ALL_BITS, TEK_REFRESH_AREA);
  }

  return (status);
}


tek_put_pixel(Unit, imp, xpos, ypos, color, mask)
int	*Unit, imp, xpos, ypos;
unsigned char	color, mask;
{
  /*  Put the pixel value into the stored image memory plane  */
  switch (DEV_TYPE)
  {
    case TEK_4237:
      TEK_IMP(imp, xpos, ypos) = (TEK_IMP(imp, xpos, ypos) & ~mask) | 
                                 (color & mask);
      break;
    case TEK_3D_LEFT:
      TEK_IMP_3DL(imp, xpos, ypos) = (TEK_IMP_3DL(imp, xpos, ypos) & ~mask) |
                                     (color & mask);
      break;
    case TEK_3D_RIGHT:
      TEK_IMP_3DR(imp, xpos, ypos) = (TEK_IMP_3DR(imp, xpos, ypos) & ~mask) |
                                     (color & mask);
      break;
  }
}

check_min_max(xpos, ypos, xmin, xmax, ymin, ymax)
int	xpos, ypos, *xmin, *xmax, *ymin, *ymax;
{
  if (xpos < *xmin)  *xmin = xpos;
  if (xpos > *xmax)  *xmax = xpos;
  if (ypos < *ymin)  *ymin = ypos;
  if (ypos > *ymax)  *ymax = ypos;
}
