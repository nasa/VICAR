/*	IVAS_GConnect - description
 *
 *	Purpose: Processes requests for connecting the graphics LUT to an
 *		 image plane.  The IVAS device currently only allows
 *		 image plane 4 to be used as the overlay plane.  If imp=4,
 *		 section=1, and bypass=0, this function returns SUCCESS.
 *		 Otherwise, it returns DEVICE_CANNOT_DO_IT.
 *
 *	Written by: Paul Bartholomew
 *	Date:	    April 5, 1990
 *
 *	Calling Sequence:
 *		STATUS = IVAS_GConnect(Unit, imp, section, bypass)
 *
 *	Parameter List:
 *		Unit:	 Display device unit number
 *		imp:	 Image plane number to connect
 *		section: Section number of LUT to use (not valid for graphics)
 *		bypass:  1=bypass LUT, 0=use LUT (not valid for graphics)
 *
 *	Possible Error Codes:
 *		none
 */

#include "xvmaininc.h"
#include "xdexterns.h"
#include "xdroutines.h"
#include "xderrors.h"
#include "xdfuncs.h"

IVAS_GConnect(Unit, imp, section, bypass)
int	*Unit, imp, section, bypass;
{
  if ((imp == OVERLAY_IMP) && (section == 1) && (bypass == 0))
    return(SUCCESS);
  else
    return(DEVICE_CANNOT_DO_IT);
}
