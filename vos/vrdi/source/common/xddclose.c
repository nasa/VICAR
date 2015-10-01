/*	xddclose - Device is no longer available for use
 *
 *	Purpose:
 *
 *	Written by:	S. Tews
 *	Date:		May 14, 1987
 *
 *	Calling Sequence:
 *
 *	        STATUS = xddclose( Unit )
 *
 *	Parameter List:
 *
 *	        Unit:	Display device unit number
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

FUNCTION FTN_NAME(xddclose)( Unit )
INTEGER	Unit;
   {
   return( zddclose( *Unit ) );
   }

FUNCTION zddclose( unit )
int	unit;
   {
   int status;

   xd_current_call = DCLOSE;

   if (!ZCHECK_UNIT_NUMBER) {
      status = UNIT_OUT_OF_RANGE;
      }
   else if (!ZCHECK_DEVICE_OPEN) {
      status = DEVICE_NOT_OPEN;
      }
   else {
      status = XD_Device_Interface ( &unit, CLOSE_DEVICE );
      if (status == SUCCESS) {
         if (ZSHARED_MEMORY_ACTIVE) {
	    status = Detach_Shmem(unit, &DCB[unit], SHMEM_DCB);
	    ZSHARED_MEMORY_ACTIVE = FALSE;
            }
         else {
	    free ( DCB[unit] );
            }
         }
      DCB[unit] = 0;
      access_window[unit] = 0;
      display_window[unit] = 0;
      zoom_factor[unit] = 0;
      cursor_position[unit] = 0;
      imp_to_lut[unit] = 0;
      lut_section[unit] = 0;
      }
  
   xd_error_handler( &unit, status );
   return ( status );
   }
