/*	Dummy_Circle - description
 *
 *	Purpose: Draw a circle on the dummy display terminal.
 *
 *	Written by: Paul Bartholomew
 *	Date:	    May 30, 1990
 *
 *	Calling Sequence:
 *		STATUS = Dummy_Circle(Unit, imp, xcenter, ycenter, radius,
 *				      color, mask, area)
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

#include "xvmaininc.h"
#include "xdexterns.h"
#include "xdroutines.h"
#include "xderrors.h"
#include "xdfuncs.h"
#include "dummy_dev.h"

FUNCTION Dummy_Circle(Unit, imp, xcenter, ycenter, radius, color, mask, area)
int *Unit, imp, xcenter, ycenter, radius;
unsigned char color, mask;
int area[4];
{
  int   x, y, xpos, ypos, d;

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
      DUMMY_IMP(imp, xpos, ypos) = (DUMMY_IMP(imp, xpos, ypos) & ~mask) |
				   (color & mask);
    }

    xpos = xcenter + x;
    ypos = ycenter - y;
    if ((xpos >= area[LEFT]) && (xpos <= area[RIGHT]) &&
        (ypos >= area[TOP])  && (ypos <= area[BOTTOM]))
    {
      DUMMY_IMP(imp, xpos, ypos) = (DUMMY_IMP(imp, xpos, ypos) & ~mask) |
				   (color & mask);
    }

    xpos = xcenter - x;
    ypos = ycenter + y;
    if ((xpos >= area[LEFT]) && (xpos <= area[RIGHT]) &&
        (ypos >= area[TOP])  && (ypos <= area[BOTTOM]))
    {
      DUMMY_IMP(imp, xpos, ypos) = (DUMMY_IMP(imp, xpos, ypos) & ~mask) |
				   (color & mask);
    }

    xpos = xcenter - x;
    ypos = ycenter - y;
    if ((xpos >= area[LEFT]) && (xpos <= area[RIGHT]) &&
        (ypos >= area[TOP])  && (ypos <= area[BOTTOM]))
    {
      DUMMY_IMP(imp, xpos, ypos) = (DUMMY_IMP(imp, xpos, ypos) & ~mask) |
				   (color & mask);
    }

    xpos = xcenter + y;
    ypos = ycenter + x;
    if ((xpos >= area[LEFT]) && (xpos <= area[RIGHT]) &&
        (ypos >= area[TOP])  && (ypos <= area[BOTTOM]))
    {
      DUMMY_IMP(imp, xpos, ypos) = (DUMMY_IMP(imp, xpos, ypos) & ~mask) |
				   (color & mask);
    }

    xpos = xcenter + y;
    ypos = ycenter - x;
    if ((xpos >= area[LEFT]) && (xpos <= area[RIGHT]) &&
        (ypos >= area[TOP])  && (ypos <= area[BOTTOM]))
    {
      DUMMY_IMP(imp, xpos, ypos) = (DUMMY_IMP(imp, xpos, ypos) & ~mask) |
				   (color & mask);
    }

    xpos = xcenter - y;
    ypos = ycenter + x;
    if ((xpos >= area[LEFT]) && (xpos <= area[RIGHT]) &&
        (ypos >= area[TOP])  && (ypos <= area[BOTTOM]))
    {
      DUMMY_IMP(imp, xpos, ypos) = (DUMMY_IMP(imp, xpos, ypos) & ~mask) |
				   (color & mask);
    }

    xpos = xcenter - y;
    ypos = ycenter - x;
    if ((xpos >= area[LEFT]) && (xpos <= area[RIGHT]) &&
        (ypos >= area[TOP])  && (ypos <= area[BOTTOM]))
    {
      DUMMY_IMP(imp, xpos, ypos) = (DUMMY_IMP(imp, xpos, ypos) & ~mask) |
				   (color & mask);
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
  return (SUCCESS);
}
