/*	xdsvnl - description
 *
 *      Purpose:     Returns the number of lines on the video screen.
 *                   Returns -1 on error.  (Duplicates xddinfo(14)).
 *
 *	Written by:  Paul Bartholomew
 *	Date:        June 28, 1989
 *
 *	Calling Sequence:
 *
 *		vnl = xdsvnl( Unit )
 *
 *	Parameter List:
 *
 *		Unit:    Display device unit number
 *
 *	Possible Error Codes:
 *
 */

#include "xvmaininc.h"
#include "ftnbridge.h"
#include "xdexterns.h"
#include "xdroutines.h"
#include "xdfuncs.h"

FUNCTION FTN_NAME(xdsvnl)( Unit )
INTEGER Unit;
   {
   return ( zdsvnl( *Unit ) );
   }

FUNCTION zdsvnl( unit )
int     unit;
   {
   int	status;

   if (!ZCHECK_UNIT_NUMBER) {
      status = -1;
      }
   else {
      status = ZVIDEO_LINES;
      }
   return (status);
   }
