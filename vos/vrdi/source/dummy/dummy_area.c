/*	Dummy_Area - description
 *
 *	Purpose: Read or write an area to the dummy display terminal.
 *
 *	Written by: Paul Bartholomew
 *	Date:	    May 30, 1990
 *
 *	Calling Sequence:
 *		STATUS = Dummy_Area(Unit, function, imp, size, area, buffer,
 *				    mask);
 *
 *	Parameter List:
 *		Unit:	  Display device unit number
 *		function: Function code, either READ_AREA or WRITE_AREA
 *		imp:	  Image plane number
 *		size:	  Size of buffer to transfer in bytes
 *		area:     Access Window array
 *		buffer:	  Image to display, or buffer to hold image read
 *		mask:	  Bit plane mask
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

FUNCTION Dummy_Area(Unit, function, imp, size, area, buffer, mask)
int *Unit, function, imp, size, area[4];
unsigned char *buffer, mask;
{
  int	nsamps, nlines, vidsamp, vidline, impsamp, impline, ctr;

  nsamps = area[RIGHT] - area[LEFT] + 1;
  nlines = area[BOTTOM] - area[TOP] + 1;

  if (function == WRITE_AREA)
  {
    /*  There are three possibilities to consider when putting the image   */
    /*  into image memory:  1) the image is the same size as the access    */
    /*  window, 2) the image is smaller than the access window, and 3) the */
    /*  image is larger than the access window.                            */

    /*  If the image is smaller than the access window, we'll run out of   */
    /*  data before filling the access window.  When this happens, we      */
    /*  simply break out of the loop.                                      */

    for (ctr=0, vidline=0; (ctr * nsamps) < size; ctr++)
    {
      impline = area[TOP] + vidline;
      for (vidsamp = 0; vidsamp < nsamps; vidsamp++)
      {
        impsamp = area[LEFT] + vidsamp;

        if ((vidsamp+(ctr*nsamps)) >= size)
          break;

        DUMMY_IMP(imp, impsamp, impline) = (DUMMY_IMP(imp,impsamp,impline) &
                              ~mask) | (buffer[vidsamp + (ctr*nsamps)] & mask);

        /*  If the access window is too small, we'll have data left over   */
        /*  after we've filled the access window.  When this happens, we   */
        /*  simply start at the top again--continuing to output data until */
        /*  we've exhausted the input buffer.                              */

        vidline++;
        if (vidline >= nlines)
          vidline = 0;
      }
    }
  }
  else					/* Function = READ_AREA */
  {
    for (impline = 0; impline < nlines; impline++)
      for (impsamp = 0; impsamp < nsamps; impsamp++)
        buffer[impsamp+(nsamps*impline)] = DUMMY_IMP(imp, area[LEFT]+impsamp,
                                                            area[TOP]+impline);
  }
  return (SUCCESS);
}
