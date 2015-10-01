/*	xdciset - description
 *
 *	Purpose:     Moves the specified cursor to the given location in
 *                   image plane coordinates.
 *  
 *	Written by:  Paul Bartholomew
 *	Date:        June 28, 1989
 *
 *	Calling Sequence:
 *
 *		STATUS = xdciset( parameters )
 *
 *	Parameter List:
 *
 *		Unit:    Display device unit number
 *              Cursor:  Cursor number
 *              X:       Image plane X coordinates
 *              Y:       Image plane Y coordinates
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

FUNCTION FTN_NAME(xdciset)( Unit, Cursor, X, Y, Imp )
INTEGER	Unit, Cursor, X, Y, Imp;
   {
   return ( zdciset( *Unit, *Cursor, *X, *Y, *Imp ) );
   }

FUNCTION zdciset( unit, cursor, x, y, imp )
int unit, cursor, x, y, imp;
   {
   int status;
   int xraw, yraw;

   status = zdcimp2raw( unit, imp, x, y, &xraw, &yraw );

   if (status == SUCCESS)
     status = zdcset( unit, cursor, xraw, yraw );

   xd_current_call = CISET;
   xd_error_handler( &unit, status );
   return (status);
   }
