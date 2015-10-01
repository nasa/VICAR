/*	xdcilocation - description
 *
 *	Purpose:     Returns the location of the cursor in image plane
 *                   coordinates.
 *
 *	Written by:  Paul Bartholomew
 *	Date:        June 28, 1989
 *
 *	Calling Sequence:
 *
 *		STATUS = xdcilocation( Unit, Cursor, X, Y, Imp )
 *
 *	Parameter List:
 *
 *		Unit:	 Display device unit number
 *              Cursor:  Cursor number
 *              X:       X coordinate of the cursor
 *              Y:       Y coordinate of the cursor
 *              Imp:     Image plane number
 *
 *	Possible Error Codes:
 *
 */

#include "xvmaininc.h"
#include "ftnbridge.h"
#include "xdexterns.h"
#include "xdroutines.h"
#include "xderrors.h"
#include "xdfuncs.h"

FUNCTION FTN_NAME(xdcilocation)( Unit, Cursor, X, Y, Imp )
INTEGER	Unit, Cursor, X, Y, Imp;
   {
   return ( zdcilocation( *Unit, *Cursor, X, Y, *Imp ) );
   }

FUNCTION zdcilocation( unit, cursor, x, y, imp )
int     unit, cursor, imp;
int     *x, *y;
   {
   int status;
   int xraw, yraw;

   status = zdclocation( unit, cursor, &xraw, &yraw );

   if (status == SUCCESS) {
      status = zdcraw2imp( unit, imp, xraw, yraw, x, y );
      }

   xd_current_call = CILOCATION;
   xd_error_handler( &unit, status );
   return (status);
   }
